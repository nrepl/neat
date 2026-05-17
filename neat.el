;;; neat.el --- A small, language-agnostic nREPL client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/nrepl/neat
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2"))
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
  :link '(url-link :tag "GitHub" "https://github.com/nrepl/neat"))


;;;; Active connection

(defun neat--connection-label (conn)
  "Return a short, human-readable label for CONN."
  (let ((sess (neat-connection-session conn)))
    (format "%s:%d%s"
            (neat-connection-host conn)
            (neat-connection-port conn)
            (if sess
                (format " [%s]" (substring sess 0 (min 8 (length sess))))
              ""))))

;;;###autoload
(defun neat-set-default-connection (conn)
  "Set CONN as the active `neat-default-connection'.

Interactively, pick from `neat-connections' via `completing-read'.
Source buffers with `neat-mode' enabled (and no buffer-local
`neat-current-connection') will route their requests to the new
default."
  (interactive
   (progn
     (unless neat-connections
       (user-error "Neat: no live connections; M-x neat to start one"))
     (let* ((labels (mapcar (lambda (c) (cons (neat--connection-label c) c))
                            neat-connections))
            (choice (completing-read "Default connection: "
                                     (mapcar #'car labels)
                                     nil t)))
       (list (cdr (assoc choice labels))))))
  (setq neat-default-connection conn)
  (message "Neat: default connection is now %s"
           (neat--connection-label conn)))


;;;; Port-file discovery

(defcustom neat-port-file-name ".nrepl-port"
  "Name of the file an nREPL server drops in the project root.
Walked up the directory tree from the current buffer to auto-fill the
port in `neat'.  Override only if your tooling writes a different name."
  :type 'string
  :group 'neat)

(defun neat-discover-port-file (&optional dir)
  "Return the path to the nearest `neat-port-file-name', or nil.
Searches upward from DIR (default: the current buffer's directory)."
  (when-let* ((start (or dir buffer-file-name default-directory))
              (containing (locate-dominating-file start neat-port-file-name)))
    (expand-file-name neat-port-file-name containing)))

(defun neat-discover-port (&optional dir)
  "Return the port number from the nearest port file, or nil.
The file is whatever `neat-port-file-name' names; search starts from
DIR (default: the current buffer's directory).  Unreadable, empty, or
non-numeric files are treated as \"not discovered\" and return nil."
  (when-let ((path (neat-discover-port-file dir)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path)
          (let ((s (string-trim (buffer-string))))
            (and (string-match-p "\\`[0-9]+\\'" s)
                 (string-to-number s))))
      (file-error nil))))


;;;; Connecting

;;;###autoload
(defun neat (host port)
  "Connect to an nREPL server at HOST and PORT, and open a REPL buffer.

When called interactively, the port defaults to whatever is in the
nearest `.nrepl-port' file (see `neat-discover-port'), so in a project
that has a running server `M-x neat RET RET' is enough.

The new connection becomes `neat-default-connection', so source buffers
with `neat-mode' enabled will use it automatically."
  (interactive
   (let ((discovered (neat-discover-port)))
     (list (read-string (format-prompt "Host" neat-repl-default-host)
                        nil nil neat-repl-default-host)
           (read-number "Port: " (or discovered neat-repl-default-port)))))
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
  (or (neat-active-connection)
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

(defun neat-interrupt-eval ()
  "Interrupt the in-flight eval on the active connection."
  (interactive)
  (neat-interrupt (neat--require-connection)))


;;;; Completion-at-point and eldoc

;; These rely on the `completions' and `lookup' ops, which are provided
;; by cider-nrepl (or compatible) middleware -- bare nREPL does not
;; support them.  When the server doesn't have the op, the sync helpers
;; in `neat-client' return nil and we quietly defer to other backends.

(defcustom neat-completion-timeout 1.0
  "Seconds to wait for a `completions' response before giving up."
  :type 'number
  :group 'neat)

(defun neat-completion-at-point ()
  "`completion-at-point-functions' entry for `neat-mode'.
Asks the server for completions of the symbol at point via the
`completions' op and returns them as a static candidate list."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((conn (neat-active-connection))
             (start (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties start end)))
        (when (and conn (neat-connection-live-p conn)
                   (>= (length prefix) 1))
          (let ((cands (delq nil
                             (mapcar (lambda (c)
                                       (neat-bencode-get c "candidate"))
                                     (neat-completions-sync
                                      conn prefix nil
                                      neat-completion-timeout)))))
            (when cands
              (list start end cands :exclusive 'no))))))))

(defun neat--eldoc-format (info)
  "Pick the displayable string from a `lookup' INFO dict, or return nil."
  (let* ((arglists (neat-bencode-get info "arglists-str"))
         (doc (neat-bencode-get info "doc"))
         (first-doc-line (and doc (car (split-string doc "\n")))))
    (cond
     ((and arglists first-doc-line)
      (format "%s: %s" arglists first-doc-line))
     (arglists arglists)
     (first-doc-line first-doc-line))))

(defun neat-eldoc-function (callback &rest _ignored)
  "Eldoc backend driven by the `lookup' op.

Conforms to `eldoc-documentation-functions': fires the supplied
CALLBACK with the docstring once the server responds, so the UI
never blocks waiting on a lookup.  When the user has moved point
by the time the response arrives, eldoc may briefly show stale
output -- acceptable trade-off for not blocking the editor."
  (let ((conn (neat-active-connection))
        (sym (thing-at-point 'symbol t)))
    (when (and conn sym (neat-connection-live-p conn))
      (neat-lookup
       conn sym nil
       (lambda (resp)
         (when-let* ((info (neat-bencode-get resp "info"))
                     (str (neat--eldoc-format info)))
           (funcall callback str :thing sym))))
      ;; Tell eldoc we'll call the callback asynchronously.
      t)))


;;;; Minor mode

(defvar neat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'neat-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'neat-eval-defun)
    (define-key map (kbd "C-c C-r") #'neat-eval-region)
    (define-key map (kbd "C-c C-b") #'neat-eval-buffer)
    (define-key map (kbd "C-c C-z") #'neat-switch-to-repl)
    (define-key map (kbd "C-c C-k") #'neat-interrupt-eval)
    map)
  "Keymap for `neat-mode'.")

;;;###autoload
(define-minor-mode neat-mode
  "Minor mode that adds nREPL evaluation bindings to a source buffer.

Bindings:
\\{neat-mode-map}"
  :lighter " neat"
  :keymap neat-mode-map
  :group 'neat
  (cond
   (neat-mode
    (add-hook 'completion-at-point-functions
              #'neat-completion-at-point nil t)
    (add-hook 'eldoc-documentation-functions
              #'neat-eldoc-function nil t))
   (t
    (remove-hook 'completion-at-point-functions
                 #'neat-completion-at-point t)
    (remove-hook 'eldoc-documentation-functions
                 #'neat-eldoc-function t))))

(provide 'neat)
;;; neat.el ends here
