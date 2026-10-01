;;; neat-client.el --- nREPL connection and op dispatch  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/nrepl/neat
;; Version: 0.1.0
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

(defvar neat-connections nil
  "List of all currently-tracked `neat-connection's, newest first.

`neat-connect' pushes a fresh connection onto this list and
`neat-disconnect' (along with the process sentinel on involuntary
deaths) removes it.  Library consumers and UI code can walk this
list to enumerate, switch, or shut down live connections.")

(defvar neat-default-connection nil
  "Global fallback `neat-connection' used when none is set buffer-locally.
`neat' sets this to the most recently created connection; the
disconnect path demotes it to the next-most-recent live connection.")

(defvar-local neat-current-connection nil
  "Per-buffer override for the active `neat-connection'.
Set automatically in `neat-repl-mode' buffers and available for any
other buffer that wants explicit routing.  Takes precedence over
`neat-default-connection'.")

(defvar neat-disconnect-functions nil
  "Abnormal hook run when a connection dies.
Each function is called with one argument, the dead `neat-connection'.
Fires from the process sentinel exactly once per process death,
regardless of whether the disconnect was user-initiated or caused by
the server going away.  Use to update buffers that reference the
connection (the REPL buffer adds a `connection closed' marker this
way).")

(defvar neat-unhandled-message-functions nil
  "Abnormal hook run for messages that no pending callback claims.
Each function is called with two arguments, the `neat-connection' and
the response dict.  That covers messages with no `id', ids that were
never registered, and anything a server sends for a request after its
`done' (nREPL and Babashka both keep sending `out' from a `future'
that outlives the eval that started it).  The REPL buffer uses this
to show such output above its prompt.")

(defun neat-active-connection ()
  "Return the active connection for the current buffer, or nil.
Prefers `neat-current-connection' when set buffer-locally and still
live, otherwise falls back to `neat-default-connection'.  The
buffer-local slot is not auto-cleared -- callers that want it
overwritten own that decision."
  (if (and neat-current-connection
           (neat-connection-live-p neat-current-connection))
      neat-current-connection
    neat-default-connection))

(cl-defstruct (neat-connection (:constructor neat-connection--make)
                                (:copier nil))
  "A connection to an nREPL server.

Slots:

  PROCESS      - the underlying network process
  HOST, PORT   - the address we connected to
  SESSION      - the nREPL session id (set by `neat-clone-session')
  NS           - the namespace the REPL last reported, if any
  CAPABILITIES - parsed response from a `describe' op
  PENDING      - hash table mapping request id -> callback function
  EVALS        - ids of `eval' and `load-file' requests awaiting `done'
  STALLED      - when a tooling request last timed out behind an eval
                 (a `float-time'), or nil; see `neat-tooling-stalled-p'
  NEXT-ID      - integer counter used to mint fresh request ids
  RECV-BUFFER  - accumulated, undecoded bytes from the wire"
  process host port session ns capabilities
  (pending (make-hash-table :test 'equal))
  evals stalled
  (next-id 0)
  (recv-buffer (unibyte-string)))


;;;; Connecting

(defun neat-connect (host port)
  "Open an nREPL connection to HOST on PORT.

Returns a `neat-connection'.  The function returns immediately --
the network process is asynchronous and responses are delivered
to callbacks registered via `neat-send' and friends.  The new
connection is pushed onto `neat-connections' so it can be found
later by enumeration or by `neat-set-default-connection'."
  (let* ((conn (neat-connection--make :host host :port port))
         (name (format "neat-nrepl-%s:%d" host port))
         (proc (open-network-stream name nil host port :coding 'binary)))
    (setf (neat-connection-process conn) proc)
    (process-put proc 'neat-connection conn)
    (set-process-filter proc #'neat-client--filter)
    (set-process-sentinel proc #'neat-client--sentinel)
    (set-process-query-on-exit-flag proc nil)
    (push conn neat-connections)
    conn))

(defun neat-disconnect (conn)
  "Close CONN and notify any pending callbacks.
If CONN's underlying process is still alive, deleting it triggers the
sentinel, which runs `neat-client--cleanup'.  If the process is already
dead (e.g., the server went away before we got here) we run cleanup
directly.  The cleanup itself is idempotent, so the two paths cannot
double-fire."
  (let ((proc (neat-connection-process conn)))
    (if (process-live-p proc)
        (delete-process proc)
      (neat-client--cleanup conn))))

(defun neat-connection-live-p (conn)
  "Return non-nil if CONN's underlying process is alive."
  (and conn (process-live-p (neat-connection-process conn))))


;;;; Message logging

(defcustom neat-log-messages nil
  "Non-nil to mirror every nREPL message to `neat-message-log-buffer-name'.
Handy for debugging client/server interactions.  Toggle interactively
with `neat-toggle-message-log'."
  :type 'boolean
  :group 'neat)

(defcustom neat-message-log-buffer-name "*neat-messages*"
  "Name of the buffer used by `neat-log-messages'."
  :type 'string
  :group 'neat)

(defcustom neat-message-log-max-message-length 2000
  "Max characters to insert per message in the log buffer.
Messages longer than this are truncated and tagged with the original
byte count.  Set to nil to disable truncation (slow on huge values)."
  :type '(choice integer (const :tag "No limit" nil))
  :group 'neat)

(defcustom neat-message-log-max-buffer-lines 5000
  "Max lines to retain in `neat-message-log-buffer-name'.
When exceeded, the oldest entries get trimmed from the front so the
buffer doesn't grow without bound during long sessions.  Set to nil
to disable trimming."
  :type '(choice integer (const :tag "No limit" nil))
  :group 'neat)

(defface neat-message-log-out
  '((t :inherit font-lock-function-name-face))
  "Face for outgoing-request markers in the message log."
  :group 'neat)

(defface neat-message-log-in
  '((t :inherit font-lock-string-face))
  "Face for incoming-response markers in the message log."
  :group 'neat)

(defface neat-message-log-note
  '((t :inherit warning))
  "Face for internal note entries (e.g. dropped malformed bytes)."
  :group 'neat)

(defface neat-message-log-meta
  '((t :inherit shadow))
  "Face for timestamp and host:port metadata in the message log."
  :group 'neat)

(defvar neat-message-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "c") #'neat-clear-message-log)
    map)
  "Keymap for `neat-message-log-mode'.")

(define-derived-mode neat-message-log-mode special-mode "neat-log"
  "Major mode for the `*neat-messages*' buffer."
  (setq-local truncate-lines nil)
  (setq-local buffer-undo-list t))

(defun neat--message-log-buffer ()
  "Return the message log buffer, creating and setting up its mode if needed."
  (let ((buf (get-buffer-create neat-message-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'neat-message-log-mode)
        (neat-message-log-mode)))
    buf))

(defun neat--message-log-format (message)
  "Pretty-print MESSAGE for the log, truncating past the configured limit."
  (let ((s (with-output-to-string (pp message))))
    (if (and neat-message-log-max-message-length
             (> (length s) neat-message-log-max-message-length))
        (concat (substring s 0 neat-message-log-max-message-length)
                (format "... [truncated, %d bytes total]\n"
                        (length s)))
      s)))

(defun neat--message-log-trim ()
  "Trim the current buffer to `neat-message-log-max-buffer-lines'."
  (when neat-message-log-max-buffer-lines
    (let ((excess (- (count-lines (point-min) (point-max))
                     neat-message-log-max-buffer-lines)))
      (when (> excess 0)
        (save-excursion
          (goto-char (point-min))
          (forward-line excess)
          (delete-region (point-min) (point)))))))

;;;###autoload
(defun neat-toggle-message-log ()
  "Toggle `neat-log-messages' and pop to the log buffer when enabling."
  (interactive)
  (setq neat-log-messages (not neat-log-messages))
  (message "neat: message log %s" (if neat-log-messages "enabled" "disabled"))
  (when neat-log-messages
    (display-buffer (neat--message-log-buffer))))

;;;###autoload
(defun neat-show-message-log ()
  "Pop up the message log buffer regardless of `neat-log-messages'."
  (interactive)
  (display-buffer (neat--message-log-buffer)))

(defun neat-clear-message-log ()
  "Wipe the message log buffer."
  (interactive)
  (when-let* ((buf (get-buffer neat-message-log-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun neat-client--log (conn direction message)
  "Append MESSAGE for CONN to the log buffer.
DIRECTION is `:out' for outgoing requests, `:in' for incoming
responses, or `:note' for internal entries (e.g. dropped malformed
bytes).  No-op unless `neat-log-messages' is non-nil.  Windows showing
the log buffer that are scrolled to the end auto-follow new entries;
windows scrolled up are left alone."
  (when neat-log-messages
    (let ((buf (neat--message-log-buffer)))
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (windows (get-buffer-window-list buf nil t))
               (following (cl-remove-if-not
                           (lambda (w) (= (window-point w) (point-max)))
                           windows))
               (arrow (pcase direction
                        (:out "-->") (:in "<--") (:note "!!!") (_ "???")))
               (face (pcase direction
                       (:out 'neat-message-log-out)
                       (:in 'neat-message-log-in)
                       (:note 'neat-message-log-note)
                       (_ 'default))))
          (save-excursion
            (goto-char (point-max))
            (insert (propertize arrow 'face face) " "
                    (propertize (format "%s %s:%d"
                                        (format-time-string "%H:%M:%S.%3N")
                                        (neat-connection-host conn)
                                        (neat-connection-port conn))
                                'face 'neat-message-log-meta)
                    "\n"
                    (neat--message-log-format message)
                    "\n")
            (neat--message-log-trim))
          (dolist (w following)
            (set-window-point w (point-max))))))))


;;;; Sending requests

(defun neat-send (conn message &optional callback)
  "Send MESSAGE to CONN, optionally invoking CALLBACK on each response.

MESSAGE is an alist of op fields (e.g. `((op . \"eval\") (code . \"...\"))').
A unique `id' field is added automatically.  CALLBACK, if given, is
called with the parsed response dict for every response sharing the
assigned id.  Callers should inspect the `status' field to detect
completion: `done' indicates the server is finished with this
request, after which the callback is unregistered.

If the connection goes away before `done' arrives, CALLBACK gets one
last response made up by neat itself, with status
\(\"done\" \"connection-closed\") and nothing else.  No server sends
`connection-closed'; it only ever comes from the client.

Returns the assigned id (a string)."
  (unless (neat-connection-live-p conn)
    (user-error "Neat: connection is not live"))
  (let* ((id (number-to-string (cl-incf (neat-connection-next-id conn))))
         (with-id (cons (cons "id" id) message))
         ;; Encode first, so a message that can't be encoded leaves no
         ;; callback or log entry behind.
         (bytes (neat-bencode-encode with-id)))
    (when callback
      (puthash id callback (neat-connection-pending conn)))
    (neat-client--log conn :out with-id)
    (process-send-string (neat-connection-process conn) bytes)
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
               ;; A `done' sent on its own mustn't wipe out the ops.
               (when (or (assoc "ops" resp)
                         (not (neat-connection-capabilities conn)))
                 (setf (neat-connection-capabilities conn) resp))
               (when callback (funcall callback resp)))))

(defun neat-op-supported-p (conn op)
  "Return non-nil if CONN's server advertises the op named OP.
Looks at the `ops' of the stored `describe' reply, which servers send
either as a dict keyed by op name or as a plain list of names.  With
no reply yet, or one that lists no ops, every op counts as supported:
better to try and get `unknown-op' back than to turn a feature off
on a guess."
  (let ((ops (neat-bencode-get (neat-connection-capabilities conn) "ops")))
    (or (not (consp ops))
        (and (if (consp (car ops)) (assoc op ops) (member op ops)) t))))

(defun neat-eval (conn code &rest plist)
  "Send an `eval' op on CONN to run CODE.

PLIST is a property list of optional fields:
  :session   session id; defaults to the connection's current session.
  :ns        namespace to evaluate in.
  :file      source file path.
  :line      1-indexed line number where the code starts.
  :column    1-indexed column number where the code starts.
  :callback  function called for each response."
  (let* ((session  (or (plist-get plist :session)
                       (neat-connection-session conn)))
         (ns       (plist-get plist :ns))
         (file     (plist-get plist :file))
         (line     (plist-get plist :line))
         (column   (plist-get plist :column))
         (callback (plist-get plist :callback))
         (msg `((op . "eval") (code . ,code)
                ,@(when session `((session . ,session)))
                ,@(when ns `((ns . ,ns)))
                ,@(when file `((file . ,file)))
                ,@(when line `((line . ,line)))
                ,@(when column `((column . ,column))))))
    (neat-client--track-eval conn (neat-send conn msg callback))))

(defun neat-load-file (conn file-contents &rest plist)
  "Send a `load-file' op on CONN carrying FILE-CONTENTS.

PLIST is a property list of optional fields:
  :file-path  client-side path the contents come from.
  :file-name  display name (typically the basename).
  :session    session id; defaults to the connection's current session.
  :callback   function called for each response.

FILE-PATH and FILE-NAME let the server attribute file and line info
to errors and other diagnostics."
  (let* ((file-path (plist-get plist :file-path))
         (file-name (plist-get plist :file-name))
         (session   (or (plist-get plist :session)
                        (neat-connection-session conn)))
         (callback  (plist-get plist :callback))
         (msg `((op . "load-file") (file . ,file-contents)
                ,@(when file-path `((file-path . ,file-path)))
                ,@(when file-name `((file-name . ,file-name)))
                ,@(when session `((session . ,session))))))
    (neat-client--track-eval conn (neat-send conn msg callback))))

(defun neat-client--track-eval (conn id)
  "Note request ID as an eval in flight on CONN and return ID."
  (push id (neat-connection-evals conn))
  id)

(defun neat-eval-pending-p (conn)
  "Return non-nil while an `eval' or `load-file' on CONN awaits `done'."
  (and (neat-connection-evals conn) t))

(defcustom neat-tooling-probe-interval 5
  "Seconds between probes of a connection whose tooling requests stall.
See `neat-tooling-stalled-p'."
  :type 'number
  :group 'neat)

(defun neat-tooling-stalled-p (conn)
  "Return non-nil if tooling requests on CONN should hold off for now.
Some servers (Babashka among them) work through one request at a time
per connection, so anything sent behind a running eval waits for it
to finish.  There's no telling from `describe', so this goes by what
happened: a tooling request that timed out while an eval was in
flight marks CONN, and the mark goes once no evals are left or a
tooling request does get answered mid-eval.  Servers that answer
tooling ops while an eval runs, nREPL included, never get marked.

So the mark can go while the eval keeps running, or when its `done'
never comes, the answer is nil once every
`neat-tooling-probe-interval' seconds: that lets one request through
to see whether the server answers now.  Ask right before sending."
  (let ((since (neat-connection-stalled conn)))
    (cond
     ((not since) nil)
     ((not (neat-eval-pending-p conn))
      (setf (neat-connection-stalled conn) nil)
      nil)
     ((< (- (float-time) since) neat-tooling-probe-interval) t)
     (t
      (setf (neat-connection-stalled conn) (float-time))
      nil))))

(defun neat-client--note-tooling (conn answered)
  "Update CONN's stall mark after a tooling request.
ANSWERED nil means the request timed out, which only counts while an
eval is in flight; see `neat-tooling-stalled-p'."
  (cond (answered (setf (neat-connection-stalled conn) nil))
        ((neat-eval-pending-p conn)
         (setf (neat-connection-stalled conn) (float-time)))))

(defun neat-stdin (conn input &rest plist)
  "Send a `stdin' op on CONN delivering INPUT to a paused eval.

Servers send a response with `status: (\"need-input\")' when the
running code reads from standard input.  The client replies with a
`stdin' op whose `stdin' field carries the text -- including any
trailing newline the reader is waiting on.

PLIST is a property list of optional fields:
  :session   session id; defaults to the connection's current session.
  :callback  function called for each response."
  (let* ((session  (or (plist-get plist :session)
                       (neat-connection-session conn)))
         (callback (plist-get plist :callback))
         (msg `((op . "stdin") (stdin . ,input)
                ,@(when session `((session . ,session))))))
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
Closing CONN's current session also clears it from CONN, so later
requests don't go out with a session the server has thrown away.
CALLBACK, if given, fires for each response message."
  (let* ((sess (or session (neat-connection-session conn)))
         (msg `((op . "close")
                ,@(when sess `((session . ,sess))))))
    (prog1 (neat-send conn msg callback)
      (when (and sess (equal sess (neat-connection-session conn)))
        (setf (neat-connection-session conn) nil)))))

(defun neat-response-error (resp &optional explained)
  "Return a short description of the error status in RESP, or nil.
Picks out the statuses servers send in place of any output, which
would otherwise leave nothing to see: `namespace-not-found',
`unknown-session', `unknown-op', and a bare `error' with no `err' or
`ex' next to it.  EXPLAINED non-nil means the request already showed
an `err' or `ex' earlier, so a bare `error' needs no line of its own.
Statuses are matched by membership, as servers pair them with `done'
and `error' in no fixed order."
  (let ((status (neat-bencode-get resp "status")))
    (cl-flet ((detailed (text key)
                (let ((detail (neat-bencode-get resp key)))
                  (if (stringp detail) (format "%s: %s" text detail) text))))
      (cond
       ((member "namespace-not-found" status)
        (detailed "namespace not found" "ns"))
       ((member "unknown-session" status) "unknown session")
       ((member "unknown-op" status) (detailed "unknown op" "op"))
       ((and (member "error" status)
             (not explained)
             (not (neat-bencode-get resp "err"))
             (not (neat-bencode-get resp "ex")))
        "error")))))

(defun neat-completions (conn prefix &optional ns callback)
  "Send a `completions' op on CONN for PREFIX (and optionally NS).
Servers that don't implement the op return an `unknown-op' status.
CALLBACK, if given, fires for each response."
  (let* ((sess (neat-connection-session conn))
         (msg `((op . "completions") (prefix . ,prefix)
                ,@(when ns `((ns . ,ns)))
                ,@(when sess `((session . ,sess))))))
    (neat-send conn msg callback)))

(defun neat-lookup (conn sym &optional ns callback)
  "Send a `lookup' op on CONN for SYM (and optionally NS).
Servers that don't implement the op return an `unknown-op' status.
CALLBACK, if given, fires for each response."
  (let* ((sess (neat-connection-session conn))
         (msg `((op . "lookup") (sym . ,sym)
                ,@(when ns `((ns . ,ns)))
                ,@(when sess `((session . ,sess))))))
    (neat-send conn msg callback)))


;;;; Blocking helpers

;; These wrap the async ops so callers expecting a return value (CAPF,
;; eldoc) can use them.  They pump `accept-process-output' until the
;; response arrives or the timeout fires.

(defun neat-client--request-sync (conn timeout send)
  "Send a request with SEND and block until CONN's server is done with it.
SEND is called with the callback to hand to the op function it calls,
and must return that request's id, as the op functions do.  Pumps
CONN's process output for up to TIMEOUT seconds.

Returns the responses that arrived, in the order they came: all of
them once `done' arrives, or whatever came before TIMEOUT ran out.
If the connection went away first, the last one is neat's own
`connection-closed' reply (see `neat-send').  Giving up unregisters
the callback, so a reply that never comes doesn't leave it behind in
the pending table."
  (let ((deadline (+ (float-time) timeout))
        responses done)
    (let ((id (funcall send
                       (lambda (resp)
                         (push resp responses)
                         (when (member "done" (neat-bencode-get resp "status"))
                           (setq done t))))))
      (while (and (not done)
                  (< (float-time) deadline))
        (accept-process-output (neat-connection-process conn) 0.05))
      (unless done
        (remhash id (neat-connection-pending conn)))
      (neat-client--note-tooling conn done))
    (nreverse responses)))

(defun neat-client--request-async (conn timeout send callback)
  "Send a request with SEND and call CALLBACK with CONN's responses.
SEND is as for `neat-client--request-sync'.  CALLBACK gets the
responses in the order they came once `done' arrives, or whatever
came before TIMEOUT seconds ran out.  Either way it counts toward
`neat-tooling-stalled-p', and a timeout unregisters the request, so
a late reply goes nowhere."
  (let (responses finished timer id)
    (setq id (funcall
              send
              (lambda (resp)
                (push resp responses)
                (when (and (not finished)
                           (member "done" (neat-bencode-get resp "status")))
                  (setq finished t)
                  (when timer (cancel-timer timer))
                  (neat-client--note-tooling conn t)
                  (funcall callback (nreverse responses))))))
    (unless finished
      (setq timer (run-at-time
                   timeout nil
                   (lambda ()
                     (unless finished
                       (setq finished t)
                       (remhash id (neat-connection-pending conn))
                       (neat-client--note-tooling conn nil)
                       (funcall callback (nreverse responses)))))))
    id))

(defun neat-client--ns-rejected-p (responses)
  "Return non-nil if RESPONSES look like the server turned down their ns.
That's a `namespace-not-found' status, or a bare `error' with nothing
more specific next to it.  Servers differ in what they do with a
namespace that isn't loaded yet, so a tooling request that named one
and got this back, with no results, is worth repeating without it.
An `unknown-op' or `unknown-session' comes with `error' as well, but
has nothing to do with the ns."
  (cl-some (lambda (resp)
             (let ((status (neat-bencode-get resp "status")))
               (or (member "namespace-not-found" status)
                   (and (member "error" status)
                        (cl-every (lambda (s) (member s '("error" "done")))
                                  status)))))
           responses))

(defun neat-completions-sync (conn prefix &optional ns timeout)
  "Block until `completions' for PREFIX (in NS) come back from CONN.
Return the list of candidate dicts (typically `(\"candidate\" . \"foo\")
`(\"type\" . \"function\")' shaped), as many as arrived within
TIMEOUT seconds (default 1).  If the server turns NS down
\(`namespace-not-found', or a bare `error'), the request goes out
again without it."
  (let* ((responses (neat-client--request-sync
                     conn (or timeout 1)
                     (lambda (callback)
                       (neat-completions conn prefix ns callback))))
         (candidates (mapcan (lambda (resp)
                               (copy-sequence
                                (neat-bencode-get resp "completions")))
                             responses)))
    (if (and ns (not candidates) (neat-client--ns-rejected-p responses))
        (neat-completions-sync conn prefix nil timeout)
      candidates)))

(defun neat-lookup-sync (conn sym &optional ns timeout)
  "Block until CONN responds to a `lookup' for SYM (in NS).
Return the `info' dict, or nil if none came within TIMEOUT seconds
\(default 1).  If the server turns NS down (`namespace-not-found', or
a bare `error'), the request goes out again without it."
  (let* ((responses (neat-client--request-sync
                     conn (or timeout 1)
                     (lambda (callback) (neat-lookup conn sym ns callback))))
         (info (cl-some (lambda (resp) (neat-bencode-get resp "info"))
                        responses)))
    (if (and ns (not info) (neat-client--ns-rejected-p responses))
        (neat-lookup-sync conn sym nil timeout)
      info)))

(defun neat-lookup-async (conn sym ns timeout callback)
  "Look SYM up on CONN (in NS) and call CALLBACK with the `info' dict.
CALLBACK gets nil when the server has none or doesn't answer within
TIMEOUT seconds; a timeout counts toward `neat-tooling-stalled-p'.
Like `neat-lookup-sync', this asks again without NS when the server
turns NS down."
  (neat-client--request-async
   conn timeout
   (lambda (cb) (neat-lookup conn sym ns cb))
   (lambda (responses)
     (let ((info (cl-some (lambda (r) (neat-bencode-get r "info"))
                          responses)))
       (if (and ns (not info) (neat-client--ns-rejected-p responses)
                (neat-connection-live-p conn))
           (neat-lookup-async conn sym nil timeout callback)
         (funcall callback info))))))

(defun neat-close-session-sync (conn &optional session timeout)
  "Close SESSION on CONN and block until the server confirms.
SESSION defaults to CONN's current one.  Return non-nil if the server
confirmed within TIMEOUT seconds, which defaults to 1 second; a
connection that goes away first doesn't count."
  (let ((status (neat-bencode-get
                 (car (last (neat-client--request-sync
                             conn (or timeout 1)
                             (lambda (callback)
                               (neat-close-session conn session callback)))))
                 "status")))
    (and (member "done" status)
         (not (member "connection-closed" status))
         t)))


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
  "Decode and dispatch as many complete bencode messages from CONN as possible.

Malformed bencode signals `neat-bencode-error', which would otherwise
propagate up out of the process filter and silently kill the cycle.
We catch it here, log the offending bytes via the message log, and
disconnect.  Past a bad byte there's no telling where the next
message starts, so reading on would only misparse whatever follows;
disconnecting at least flushes every pending callback.  As elsewhere,
`debug-on-error' steps the guard aside so the bug is visible during
interactive debugging."
  (condition-case-unless-debug err
      (let (decoded)
        (while (setq decoded
                     (neat-bencode-decode
                      (neat-connection-recv-buffer conn)))
          (let ((message (car decoded))
                (next-idx (cdr decoded)))
            (setf (neat-connection-recv-buffer conn)
                  (substring (neat-connection-recv-buffer conn) next-idx))
            (neat-client--dispatch conn message))))
    (neat-bencode-error
     (neat-client--log conn :note (list 'malformed
                                        (neat-connection-recv-buffer conn)
                                        err))
     (setf (neat-connection-recv-buffer conn) (unibyte-string))
     (message "neat: malformed bencode from %s:%s, disconnecting (%S)"
              (neat-connection-host conn)
              (neat-connection-port conn)
              err)
     (neat-disconnect conn))))

(defun neat-client--dispatch (conn message)
  "Look up MESSAGE's callback in CONN and invoke it.

A server can send any bencode value, so MESSAGE gets checked first:
anything other than a dict is logged and skipped, a `status' sent as
a plain string counts as a list of one, and any other `status' that
isn't a list is dropped as if it weren't there.  Either would
otherwise blow up in here or in the callback.  An `unknown-session'
reply gets a `done' added if it lacks one, since nothing more can
come for a request the server has no session for.  A message without
a callback goes to `neat-unhandled-message-functions'.  When the
response's status contains `done' the callback entry is pruned
before the callback runs."
  (neat-client--log conn :in message)
  (if (not (neat-bencode-dict-p message))
      (neat-client--log conn :note '(skipped "not a dict"))
    (let ((status (neat-bencode-get message "status")))
      (when (stringp status)
        (setq status (list status))
        (setf (alist-get "status" message nil nil #'equal) status))
      (cond
       ((not (listp status))
        (setq message (cl-remove "status" message :key #'car :test #'equal)))
       ((and (member "unknown-session" status)
             (not (member "done" status)))
        (setf (alist-get "status" message nil nil #'equal)
              (append status '("done"))))))
    (let* ((id (neat-bencode-get message "id"))
           (status (neat-bencode-get message "status"))
           (callback (and id (gethash id (neat-connection-pending conn)))))
      ;; Prune before the callback runs: one that gets quit out of
      ;; (C-g at a prompt it opened) must not leave its request behind.
      (when (and id (member "done" status))
        (remhash id (neat-connection-pending conn))
        (setf (neat-connection-evals conn)
              (delete id (neat-connection-evals conn)))
        (unless (neat-connection-evals conn)
          (setf (neat-connection-stalled conn) nil)))
      (if callback
          ;; Don't let a buggy callback nuke the whole filter.  Skip the
          ;; trap when the user is debugging, so `toggle-debug-on-error'
          ;; reveals the underlying problem instead of swallowing it.
          (condition-case-unless-debug err
              (funcall callback message)
            (error (message "neat: callback error: %S" err)))
        (with-demoted-errors "neat: unhandled message hook: %S"
          (run-hook-with-args 'neat-unhandled-message-functions
                              conn message))))))

(defun neat-client--sentinel (proc _event)
  "Sentinel for nREPL connection PROC.  Delegates to `neat-client--cleanup'."
  (unless (process-live-p proc)
    (when-let* ((conn (process-get proc 'neat-connection)))
      (neat-client--cleanup conn))))

(defun neat-client--cleanup (conn)
  "Single cleanup path for a dead CONN.
Drops CONN from `neat-connections', demotes the default if needed,
flushes pending callbacks, and runs `neat-disconnect-functions'.

Idempotent: the registry-membership check makes subsequent calls
on the same connection no-ops, so `neat-disconnect' (synchronous)
and the sentinel (asynchronous on involuntary deaths) can both
funnel through here without double-firing.  Order matters: pending
callbacks see their synthesized response before the global hook
updates buffers built on top."
  (when (memq conn neat-connections)
    (setq neat-connections (delq conn neat-connections))
    (when (eq neat-default-connection conn)
      (setq neat-default-connection (car neat-connections)))
    (neat-client--flush-pending conn)
    (with-demoted-errors "neat: disconnect hook: %S"
      (run-hook-with-args 'neat-disconnect-functions conn))))

(defun neat-client--flush-pending (conn)
  "Notify every pending callback on CONN that the connection is gone.

Each callback gets a synthesized response whose status is
\(\"done\" \"connection-closed\"), as documented in `neat-send'.
It deliberately carries no `err' text: the caller decides how (and
whether) to tell the user."
  (let ((pending (neat-connection-pending conn)))
    (maphash
     (lambda (id callback)
       (condition-case-unless-debug _
           (funcall callback
                    `(("id" . ,id)
                      ("status" . ("done" "connection-closed"))))
         (error nil)))
     pending)
    (clrhash pending)
    (setf (neat-connection-evals conn) nil
          (neat-connection-stalled conn) nil)))

(provide 'neat-client)
;;; neat-client.el ends here
