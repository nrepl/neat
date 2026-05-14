;;; neat-repl.el --- REPL buffer for neat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/neat
;; Version: 0.0.1
;; Keywords: languages, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A simple comint-based REPL buffer that talks to an nREPL server via
;; `neat-client'.  The buffer's comint "process" is an internal pipe
;; with no command attached; we override `comint-input-sender' to ship
;; input as an `eval' op, and insert responses with `comint-output-filter'.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'neat-bencode)
(require 'neat-client)

(defcustom neat-repl-prompt "neat> "
  "Prompt string displayed in the REPL buffer."
  :type 'string
  :group 'neat)

(defcustom neat-repl-default-host "localhost"
  "Default host for `neat'."
  :type 'string
  :group 'neat)

(defcustom neat-repl-default-port 7888
  "Default port for `neat'."
  :type 'integer
  :group 'neat)

(defface neat-repl-output
  '((t :inherit shadow))
  "Face for `out' (stdout) chunks streamed back from the server."
  :group 'neat)

(defface neat-repl-error
  '((t :inherit error))
  "Face for `err' chunks and exception summaries from the server."
  :group 'neat)

(defface neat-repl-value
  '((t :inherit font-lock-constant-face))
  "Face for `value' lines produced by an eval."
  :group 'neat)

(defvar-local neat-repl-connection nil
  "The `neat-connection' associated with this REPL buffer.")

(defvar neat-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-c") #'neat-repl-interrupt)
    (define-key map (kbd "C-c C-q") #'neat-repl-quit)
    map)
  "Keymap for `neat-repl-mode'.")

(define-derived-mode neat-repl-mode comint-mode "neat-repl"
  "Major mode for an nREPL REPL buffer."
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote neat-repl-prompt)))
  (setq-local comint-prompt-read-only t)
  (setq-local comint-input-sender #'neat-repl--input-sender)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-scroll-show-maximum-output t)
  (add-hook 'kill-buffer-hook #'neat-repl--kill-buffer-cleanup nil t))

(defun neat-repl-buffer-name (conn)
  "Return the canonical REPL buffer name for CONN."
  (format "*neat: %s:%d*"
          (neat-connection-host conn)
          (neat-connection-port conn)))

(defun neat-repl-buffer-for (conn)
  "Return CONN's REPL buffer if one exists, else nil."
  (get-buffer (neat-repl-buffer-name conn)))

(defun neat-repl-create-buffer (conn)
  "Get or create CONN's REPL buffer and put it in `neat-repl-mode'."
  (let ((buffer (get-buffer-create (neat-repl-buffer-name conn))))
    (with-current-buffer buffer
      (neat-repl--ensure-pipe-process)
      (neat-repl-mode)
      (setq neat-repl-connection conn))
    buffer))

(defun neat-repl--ensure-pipe-process ()
  "Attach an idle pipe process to the current buffer.
Comint requires a process; this one is a no-op sink whose only
purpose is to satisfy `comint-output-filter' and friends."
  (unless (get-buffer-process (current-buffer))
    (let ((proc (make-pipe-process
                 :name (format "neat-pipe-%s" (buffer-name))
                 :buffer (current-buffer)
                 :noquery t
                 :coding 'utf-8)))
      (set-marker (process-mark proc) (point-max)))))

(defun neat-repl--insert-prompt ()
  "Insert a fresh prompt at the end of the buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (comint-output-filter proc neat-repl-prompt))))

(defun neat-repl--input-sender (_proc input)
  "Eval INPUT on the current REPL buffer's connection."
  (let* ((buffer (current-buffer))
         (conn neat-repl-connection)
         (trimmed (string-trim-right input)))
    (cond
     ((not conn)
      (message "Neat: no connection in this buffer"))
     ((string-empty-p trimmed)
      (neat-repl--insert-prompt))
     (t
      (neat-eval
       conn trimmed nil
       (lambda (resp)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (neat-repl--render-response resp)))))))))

(defun neat-repl--render-response (resp)
  "Insert the user-visible parts of nREPL response RESP into the buffer."
  (let ((proc (get-buffer-process (current-buffer)))
        (value (neat-bencode-get resp "value"))
        (out (neat-bencode-get resp "out"))
        (err (neat-bencode-get resp "err"))
        (ex (neat-bencode-get resp "ex"))
        (status (neat-bencode-get resp "status")))
    (when proc
      (when out
        (comint-output-filter
         proc (propertize out 'face 'neat-repl-output)))
      (when err
        (comint-output-filter
         proc (propertize err 'face 'neat-repl-error)))
      (when value
        (comint-output-filter
         proc (concat (propertize value 'face 'neat-repl-value) "\n")))
      (when ex
        (comint-output-filter
         proc (propertize (format "%s\n" ex) 'face 'neat-repl-error)))
      (when (member "done" status)
        (comint-output-filter proc neat-repl-prompt)))))

(defun neat-repl-interrupt ()
  "Send an `interrupt' op to the REPL's connection."
  (interactive)
  (if neat-repl-connection
      (neat-interrupt neat-repl-connection)
    (user-error "Neat: no connection in this buffer")))

(defun neat-repl-quit ()
  "Disconnect from the nREPL server and bury this buffer."
  (interactive)
  (when neat-repl-connection
    (neat-disconnect neat-repl-connection)
    (setq neat-repl-connection nil))
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (delete-process proc)))
  (bury-buffer))

(defun neat-repl--kill-buffer-cleanup ()
  "Tear down the connection and pipe process when the REPL buffer dies."
  (when (and neat-repl-connection
             (neat-connection-live-p neat-repl-connection))
    (ignore-errors (neat-disconnect neat-repl-connection)))
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (delete-process proc))))

(provide 'neat-repl)
;;; neat-repl.el ends here
