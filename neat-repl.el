;;; neat-repl.el --- REPL buffer for neat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/nrepl/neat
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

(defcustom neat-repl-prompt-format "%s> "
  "Format string used to build the REPL prompt.
The single %s is replaced with the current namespace (see
`neat-repl--current-ns'), or `neat-repl-default-ns' before the
server has reported one."
  :type 'string
  :group 'neat)

(defcustom neat-repl-default-ns "neat"
  "Namespace shown in the REPL prompt before the server reports one."
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

(defcustom neat-repl-history-file
  (expand-file-name "neat-repl-history" user-emacs-directory)
  "File where REPL input history is persisted between sessions.
Set to nil to disable persistence."
  :type '(choice file (const :tag "Disabled" nil))
  :group 'neat)

(defcustom neat-repl-history-size 1000
  "Maximum number of input entries to keep in the REPL history ring."
  :type 'integer
  :group 'neat)

(defvar neat-repl-input-syntax-table emacs-lisp-mode-syntax-table
  "Syntax table used when checking REPL input balance before submit.
Defaults to Emacs Lisp syntax, which is close enough for the Clojure
family.  Set to a different syntax table if you're talking to a server
in a language with very different bracketing rules.")

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

(defvar-local neat-repl--current-ns nil
  "Most-recent namespace reported by the server for this buffer.")

;; Forward declarations so neat-repl-mode can hook these without
;; requiring neat.el (which depends on neat-repl.el, not the other way).
(declare-function neat-completion-at-point "neat" ())
(declare-function neat-eldoc-function "neat" (callback &rest _ignored))
(declare-function neat--xref-backend "neat" ())

(defvar neat-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "RET") #'neat-repl-return)
    (define-key map (kbd "C-c C-c") #'neat-repl-interrupt)
    (define-key map (kbd "C-c C-q") #'neat-repl-quit)
    (define-key map (kbd "C-c M-o") #'neat-repl-clear-buffer)
    map)
  "Keymap for `neat-repl-mode'.")

(define-derived-mode neat-repl-mode comint-mode "neat-repl"
  "Major mode for an nREPL REPL buffer."
  ;; A permissive prompt regex so the prompt format can vary with the
  ;; current namespace.  Matches `<anything-but-newline>> ' at line start.
  (setq-local comint-prompt-regexp "^[^\n]*?> ")
  (setq-local comint-prompt-read-only t)
  (setq-local comint-input-sender #'neat-repl--input-sender)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-scroll-show-maximum-output t)
  (when neat-repl-history-file
    (setq-local comint-input-ring-file-name neat-repl-history-file)
    (setq-local comint-input-ring-size neat-repl-history-size)
    (ignore-errors (comint-read-input-ring t)))
  ;; Same backends `neat-mode' uses in source buffers.  They route via
  ;; `neat-active-connection', which sees this buffer's
  ;; `neat-current-connection' first.
  (add-hook 'completion-at-point-functions
            #'neat-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions
            #'neat-eldoc-function nil t)
  (add-hook 'xref-backend-functions
            #'neat--xref-backend nil t)
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
      (setq neat-current-connection conn))
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

(defun neat-repl--prompt ()
  "Compute the prompt string for the current buffer."
  (format neat-repl-prompt-format
          (or neat-repl--current-ns neat-repl-default-ns)))

(defun neat-repl--insert-prompt ()
  "Insert a fresh prompt at the end of the buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (comint-output-filter proc (neat-repl--prompt)))))

(defun neat-repl--input-complete-p (input)
  "Return non-nil if INPUT is a balanced, complete form.

Empty input counts as complete.  Otherwise the string is parsed under
`neat-repl-input-syntax-table' and we require zero open parens, no
in-string state, and no in-comment state at end of input."
  (or (string-empty-p (string-trim input))
      (with-temp-buffer
        (set-syntax-table neat-repl-input-syntax-table)
        (insert input)
        (let ((state (parse-partial-sexp (point-min) (point-max))))
          (and (zerop (car state))   ; depth in parens
               (null (nth 3 state))   ; inside a string
               (null (nth 4 state)))))))

(defun neat-repl-return ()
  "Submit the pending REPL input when it is balanced.
Otherwise insert a newline so the user can keep typing the form."
  (interactive)
  (let* ((start (comint-line-beginning-position))
         (input (buffer-substring-no-properties start (point-max))))
    (if (neat-repl--input-complete-p input)
        (comint-send-input)
      (newline))))

(defun neat-repl--input-sender (_proc input)
  "Eval INPUT on the current REPL buffer's connection."
  (let* ((buffer (current-buffer))
         (conn neat-current-connection)
         (trimmed (string-trim-right (substring-no-properties input))))
    (cond
     ((not conn)
      (message "Neat: no connection in this buffer"))
     ((string-empty-p trimmed)
      (neat-repl--insert-prompt))
     (t
      (neat-eval
       conn trimmed
       :callback (lambda (resp)
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
        (ns (neat-bencode-get resp "ns"))
        (status (neat-bencode-get resp "status")))
    ;; Track the namespace as soon as we see one so the next prompt
    ;; reflects any `(in-ns ...)' or namespace-switching form.
    (when ns
      (setq neat-repl--current-ns ns))
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
        (comint-output-filter proc (neat-repl--prompt))))))

(defun neat-repl-interrupt ()
  "Send an `interrupt' op to the REPL's connection."
  (interactive)
  (if neat-current-connection
      (neat-interrupt neat-current-connection)
    (user-error "Neat: no connection in this buffer")))

(defun neat-repl-clear-buffer ()
  "Wipe the REPL buffer's history but keep the live prompt.
Doesn't touch the input ring (`M-p' / `M-n' still work) or the
underlying connection."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (neat-repl--insert-prompt)))

(defun neat-repl-quit ()
  "Disconnect from the nREPL server and bury this buffer."
  (interactive)
  (when neat-current-connection
    (neat-disconnect neat-current-connection)
    (setq neat-current-connection nil))
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (delete-process proc)))
  (bury-buffer))

(defun neat-repl--kill-buffer-cleanup ()
  "Tear down the connection, persist history, and stop the pipe process."
  (when comint-input-ring-file-name
    (ignore-errors (comint-write-input-ring)))
  (when (and neat-current-connection
             (neat-connection-live-p neat-current-connection))
    (ignore-errors (neat-disconnect neat-current-connection)))
  (let ((proc (get-buffer-process (current-buffer))))
    (when (process-live-p proc)
      (delete-process proc))))

(provide 'neat-repl)
;;; neat-repl.el ends here
