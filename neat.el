;;; neat.el --- A small, language-agnostic nREPL client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/neat
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; neat is a small nREPL client for Emacs, modelled on monroe but aiming
;; to be language-agnostic.  This file is the entry point: it ties the
;; client and REPL modules together, defines `neat-mode' for source
;; buffers, and exposes the `neat' command to connect to a server.

;;; Code:

(require 'cl-lib)
(require 'neat-bencode)
(require 'neat-client)
(require 'neat-repl)

(defgroup neat nil
  "A language-agnostic nREPL client for Emacs."
  :group 'tools
  :prefix "neat-"
  :link '(url-link :tag "GitHub" "https://github.com/bbatsov/neat"))


;;;; Active connection

(defvar neat-default-connection nil
  "Global fallback `neat-connection' used when none is set buffer-locally.
`neat' sets this to the most recently created connection.")

(defvar-local neat-current-connection nil
  "Per-buffer override for the active `neat-connection'.
When non-nil, takes precedence over `neat-default-connection'.")

(defun neat-current-connection ()
  "Return the active connection for the current buffer, or nil."
  (or neat-current-connection neat-default-connection))


;;;; Connecting

;;;###autoload
(defun neat (host port)
  "Connect to an nREPL server at HOST and PORT, and open a REPL buffer.

The new connection becomes `neat-default-connection', so source buffers
with `neat-mode' enabled will use it automatically."
  (interactive
   (list (read-string (format "Host (default %s): " neat-repl-default-host)
                      nil nil neat-repl-default-host)
         (read-number "Port: " neat-repl-default-port)))
  (let* ((conn (neat-connect host port))
         (buffer (neat-repl-create-buffer conn)))
    (setq neat-default-connection conn)
    (neat-describe conn)
    (neat-clone-session
     conn
     (lambda (resp)
       (when (member "done" (neat-bencode-get resp "status"))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (neat-repl--insert-prompt))))))
    (pop-to-buffer buffer)
    conn))


;;;; Source-buffer evaluation

(defun neat--require-connection ()
  "Return the active connection, or signal a user-error."
  (or (neat-current-connection)
      (user-error "Neat: no active connection; M-x neat to start one")))

(defun neat--render-into-repl (conn resp)
  "If CONN has a REPL buffer, render RESP there; otherwise message a brief result."
  (let ((repl (neat-repl-buffer-for conn)))
    (if (buffer-live-p repl)
        (with-current-buffer repl
          (neat-repl--render-response resp))
      (let ((value (neat-bencode-get resp "value"))
            (err (neat-bencode-get resp "err")))
        (cond (err (message "neat: %s" (string-trim err)))
              (value (message "=> %s" value)))))))

(defun neat--eval-string (code)
  "Evaluate CODE on the active connection."
  (let ((conn (neat--require-connection)))
    (neat-eval conn code nil
               (lambda (resp) (neat--render-into-repl conn resp)))))

(defun neat-eval-last-sexp ()
  "Evaluate the sexp before point."
  (interactive)
  (let ((sexp (thing-at-point 'sexp t)))
    (unless sexp
      (user-error "No sexp at point"))
    (neat--eval-string sexp)))

(defun neat-eval-defun ()
  "Evaluate the top-level form containing point."
  (interactive)
  (save-excursion
    (let* ((end (progn (end-of-defun) (point)))
           (start (progn (beginning-of-defun) (point))))
      (neat--eval-string (buffer-substring-no-properties start end)))))

(defun neat-eval-region (beg end)
  "Evaluate the region between BEG and END."
  (interactive "r")
  (neat--eval-string (buffer-substring-no-properties beg end)))

(defun neat-eval-buffer ()
  "Evaluate the current buffer."
  (interactive)
  (neat--eval-string (buffer-substring-no-properties (point-min) (point-max))))

(defun neat-switch-to-repl ()
  "Pop to the REPL buffer for the active connection."
  (interactive)
  (let* ((conn (neat--require-connection))
         (buf (neat-repl-buffer-for conn)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (user-error "Neat: no REPL buffer for this connection"))))

(defun neat-cancel ()
  "Interrupt the in-flight eval on the active connection."
  (interactive)
  (neat-interrupt (neat--require-connection)))


;;;; Minor mode

(defvar neat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'neat-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'neat-eval-defun)
    (define-key map (kbd "C-c C-r") #'neat-eval-region)
    (define-key map (kbd "C-c C-b") #'neat-eval-buffer)
    (define-key map (kbd "C-c C-z") #'neat-switch-to-repl)
    (define-key map (kbd "C-c C-k") #'neat-cancel)
    map)
  "Keymap for `neat-mode'.")

;;;###autoload
(define-minor-mode neat-mode
  "Minor mode that adds nREPL evaluation bindings to a source buffer.

Bindings:
\\{neat-mode-map}"
  :lighter " neat"
  :keymap neat-mode-map
  :group 'neat)

(provide 'neat)
;;; neat.el ends here
