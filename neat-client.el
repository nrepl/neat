;;; neat-client.el --- nREPL connection and op dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/neat
;; Version: 0.0.1
;; Keywords: languages, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The neat client.  Manages a TCP connection to an nREPL server,
;; correlates requests with responses via a hash table of pending
;; callbacks keyed by request id, and exposes a small surface for the
;; most useful ops: `describe', `clone', `eval', `interrupt', `close'.
;;
;; This module is UI-agnostic.  `neat-repl' builds on top, but other
;; packages can use these functions directly to build their own
;; interfaces.

;;; Code:

(require 'cl-lib)
(require 'neat-bencode)

(cl-defstruct (neat-connection (:constructor neat-connection--make)
                                (:copier nil))
  "A connection to an nREPL server.

Slots:

  PROCESS      - the underlying network process
  HOST, PORT   - the address we connected to
  SESSION      - the nREPL session id (set by `neat-clone-session')
  CAPABILITIES - parsed response from a `describe' op
  PENDING      - hash table mapping request id -> callback function
  NEXT-ID      - integer counter used to mint fresh request ids
  RECV-BUFFER  - accumulated, undecoded bytes from the wire"
  process host port session capabilities
  (pending (make-hash-table :test 'equal))
  (next-id 0)
  (recv-buffer (unibyte-string)))


;;;; Connecting

(defun neat-connect (host port)
  "Open an nREPL connection to HOST on PORT.

Returns a `neat-connection'.  The function returns immediately --
the network process is asynchronous and responses are delivered
to callbacks registered via `neat-send' and friends."
  (let* ((conn (neat-connection--make :host host :port port))
         (name (format "neat-nrepl-%s:%d" host port))
         (proc (open-network-stream name nil host port :coding 'binary)))
    (setf (neat-connection-process conn) proc)
    (process-put proc 'neat-connection conn)
    (set-process-filter proc #'neat-client--filter)
    (set-process-sentinel proc #'neat-client--sentinel)
    (set-process-query-on-exit-flag proc nil)
    conn))

(defun neat-disconnect (conn)
  "Close CONN and notify any pending callbacks."
  (let ((proc (neat-connection-process conn)))
    (when (process-live-p proc)
      (delete-process proc)))
  (neat-client--flush-pending conn "disconnected"))

(defun neat-connection-live-p (conn)
  "Return non-nil if CONN's underlying process is alive."
  (and conn (process-live-p (neat-connection-process conn))))


;;;; Sending requests

(defun neat-send (conn message &optional callback)
  "Send MESSAGE to CONN, optionally invoking CALLBACK on each response.

MESSAGE is an alist of op fields (e.g. `((op . \"eval\") (code . \"...\"))').
A unique `id' field is added automatically.  CALLBACK, if given, is
called with the parsed response dict for every response sharing the
assigned id.  Callers should inspect the `status' field to detect
completion -- `done' indicates the server is finished with this
request, after which the callback is unregistered.

Returns the assigned id (a string)."
  (unless (neat-connection-live-p conn)
    (error "Neat: connection is not live"))
  (let* ((id (number-to-string (cl-incf (neat-connection-next-id conn))))
         (with-id (cons (cons "id" id) message)))
    (when callback
      (puthash id callback (neat-connection-pending conn)))
    (process-send-string (neat-connection-process conn)
                         (neat-bencode-encode with-id))
    id))


;;;; Standard ops

(defun neat-clone-session (conn &optional callback)
  "Send a `clone' op on CONN to obtain a fresh session id.
On success, the new session id is stored on the connection.
CALLBACK, if given, fires for each response message."
  (neat-send conn
             '((op . "clone"))
             (lambda (resp)
               (let ((new-sess (neat-bencode-get resp "new-session")))
                 (when new-sess
                   (setf (neat-connection-session conn) new-sess)))
               (when callback (funcall callback resp)))))

(defun neat-describe (conn &optional callback)
  "Send a `describe' op on CONN and remember the server's capabilities.
CALLBACK, if given, fires for each response message."
  (neat-send conn
             '((op . "describe"))
             (lambda (resp)
               (setf (neat-connection-capabilities conn) resp)
               (when callback (funcall callback resp)))))

(defun neat-eval (conn code &optional session callback)
  "Send an `eval' op on CONN to run CODE.
SESSION defaults to the connection's current session, set by
`neat-clone-session'.  CALLBACK is called for each response."
  (let* ((sess (or session (neat-connection-session conn)))
         (msg `((op . "eval") (code . ,code)
                ,@(when sess `((session . ,sess))))))
    (neat-send conn msg callback)))

(defun neat-interrupt (conn &optional session interrupt-id callback)
  "Send an `interrupt' op on CONN.
SESSION defaults to the connection's current session.  INTERRUPT-ID,
if given, names a specific in-flight request to interrupt; otherwise
the server interrupts whatever is running.  CALLBACK, if given, fires
for each response message."
  (let* ((sess (or session (neat-connection-session conn)))
         (msg `((op . "interrupt")
                ,@(when sess `((session . ,sess)))
                ,@(when interrupt-id
                    `((interrupt-id . ,interrupt-id))))))
    (neat-send conn msg callback)))

(defun neat-close-session (conn &optional session callback)
  "Send a `close' op on CONN to close SESSION (defaults to the current one).
CALLBACK, if given, fires for each response message."
  (let* ((sess (or session (neat-connection-session conn)))
         (msg `((op . "close")
                ,@(when sess `((session . ,sess))))))
    (neat-send conn msg callback)))


;;;; Process filter / sentinel

(defun neat-client--filter (proc chunk)
  "Process filter for nREPL connections.
Appends CHUNK to PROC's recv-buffer and drains as many complete
bencode messages as it can."
  (let* ((conn (process-get proc 'neat-connection))
         (buf (neat-connection-recv-buffer conn)))
    (setf (neat-connection-recv-buffer conn) (concat buf chunk))
    (neat-client--drain conn)))

(defun neat-client--drain (conn)
  "Decode and dispatch as many complete bencode messages from CONN as possible."
  (let (decoded)
    (while (setq decoded
                 (neat-bencode-decode (neat-connection-recv-buffer conn)))
      (let ((message (car decoded))
            (next-idx (cdr decoded)))
        (setf (neat-connection-recv-buffer conn)
              (substring (neat-connection-recv-buffer conn) next-idx))
        (neat-client--dispatch conn message)))))

(defun neat-client--dispatch (conn message)
  "Look up MESSAGE's callback in CONN and invoke it.

When the response's status contains `done' the callback entry is
pruned afterwards."
  (let* ((id (neat-bencode-get message "id"))
         (status (neat-bencode-get message "status"))
         (callback (and id (gethash id (neat-connection-pending conn)))))
    (when callback
      ;; Don't let a buggy callback nuke the whole filter.
      (condition-case err
          (funcall callback message)
        (error (message "neat: callback error: %S" err))))
    (when (and id (member "done" status))
      (remhash id (neat-connection-pending conn)))))

(defun neat-client--sentinel (proc _event)
  "Sentinel for nREPL connection PROC.  Notify pending callbacks on death."
  (unless (process-live-p proc)
    (let ((conn (process-get proc 'neat-connection)))
      (when conn
        (neat-client--flush-pending conn "connection closed")))))

(defun neat-client--flush-pending (conn reason)
  "Notify every pending callback on CONN that the connection is gone.

Each callback gets a synthesized response with a `done'/`interrupted'
status and an `err' field describing REASON."
  (let ((pending (neat-connection-pending conn)))
    (maphash
     (lambda (id callback)
       (condition-case _
           (funcall callback
                    `(("id" . ,id)
                      ("status" . ("done" "interrupted"))
                      ("err" . ,(format "neat: %s" reason))))
         (error nil)))
     pending)
    (clrhash pending)))

(provide 'neat-client)
;;; neat-client.el ends here
