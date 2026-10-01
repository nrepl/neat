;;; neat-repl.el --- REPL buffer for neat  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/nrepl/neat
;; Version: 0.1.0
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

(defvar-local neat-repl--prompt-start nil
  "Marker at the start of the prompt waiting for input, or nil.
Set whenever a prompt goes in and cleared once input is sent, so
output that turns up between evals can be put above the prompt
instead of after it.")

(defvar-local neat-repl--connection-dead nil
  "Non-nil once the buffer's connection has died.
Set by `neat-repl--handle-disconnect'.  The input sender consults this
to refuse sending and message the user instead.")

;; Forward declarations so neat-repl-mode can hook these without
;; requiring neat.el (which depends on neat-repl.el, not the other way).
(declare-function neat-completion-at-point "neat" ())
(declare-function neat-eldoc-function "neat" (callback &rest _ignored))
(declare-function neat--xref-backend "neat" ())
(declare-function neat--mode-line-info "neat" ())
(declare-function neat-show-doc-at-point "neat" ())

(defvar neat-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "RET") #'neat-repl-return)
    (define-key map (kbd "C-c C-c") #'neat-repl-interrupt)
    (define-key map (kbd "C-c C-q") #'neat-repl-quit)
    (define-key map (kbd "C-c M-o") #'neat-repl-clear-buffer)
    (define-key map (kbd "C-c C-d C-d") #'neat-show-doc-at-point)
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
  ;; Override the inherited ":run" comint chatter -- our pipe process
  ;; says nothing about the actual connection state.
  (setq-local mode-line-process
              '(:eval (let ((info (neat--mode-line-info)))
                        (if info (concat " " info) ""))))
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
  "Insert a fresh prompt at the end of the buffer.
It starts on a line of its own, even after output that didn't end one
or after the dead prompt of an earlier connection in a reused buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (unless (save-excursion (goto-char (process-mark proc)) (bolp))
        (comint-output-filter proc "\n"))
      (setq neat-repl--prompt-start (copy-marker (process-mark proc)))
      (comint-output-filter proc (neat-repl--prompt)))))

(defun neat-repl--emit-output (text face)
  "Insert TEXT in FACE above the prompt waiting for input.
With no prompt showing (an eval from this buffer is still running)
TEXT just streams in at the process mark like any other output.
Either way it goes through `comint-output-filter'.  A chunk that
doesn't end in a newline gets one, so the prompt stays on its own
line; the next chunk picks up where that one left off."
  (let ((proc (get-buffer-process (current-buffer))))
    (cond
     ((not proc) nil)
     ((and neat-repl--prompt-start (marker-position neat-repl--prompt-start))
      (neat-repl--insert-above-prompt proc text face))
     (t (comint-output-filter proc (propertize text 'face face))))))

(defvar ansi-color-context-region)

(defvar-local neat-repl--ansi-context nil
  "ANSI color state for text inserted above the prompt.
It plays the part of `ansi-color-context-region' for that text alone.
The context streamed output uses points past the prompt, so sharing
one would let a color left on at either end trip up the other.")

(defun neat-repl--insert-above-prompt (proc text face)
  "Insert TEXT in FACE right above the waiting prompt in PROC's buffer.
The text goes through `comint-output-filter' itself, with the process
mark moved to the start of the prompt for the duration, so it gets
what any other output gets: filters, ANSI colors, carriage motion.
A marker that moves along with insertions keeps track of the prompt,
and the process mark and comint's record of where the prompt is are
put back afterwards.  ANSI colors keep their own state here (see
`neat-repl--ansi-context'), a chunk ending in a carriage return gets
its line overwritten by the next one, as comint does for streamed
output, and `comint-move-point-for-output' moves point to the input
rather than onto the prompt."
  (unless (string-empty-p text)
    (let* ((pmark (process-mark proc))
           (end (copy-marker (process-mark proc)))
           (prompt (copy-marker neat-repl--prompt-start t))
           (streamed-ansi (and (boundp 'ansi-color-context-region)
                               ansi-color-context-region))
           (overwrite (string-suffix-p "\r" text))
           (text (if overwrite (string-trim-right text "\r+") text))
           (padded (not (string-suffix-p "\n" text))))
      (save-restriction
        (widen)
        (unwind-protect
            (progn
              (when (and (> prompt (point-min))
                         (get-text-property (1- prompt) 'neat-repl-padding))
                (let ((inhibit-read-only t))
                  (delete-region (1- prompt) prompt)))
              (set-marker pmark prompt)
              ;; A color still on from the last chunk carries on here.
              (when-let* ((marker (cadr neat-repl--ansi-context))
                          ((markerp marker))
                          ((marker-position marker)))
                (set-marker marker prompt))
              (setq-local ansi-color-context-region neat-repl--ansi-context)
              ;; comint takes the last line of what it inserts for a
              ;; prompt unless it ends in a newline, and with the
              ;; process mark at the prompt it would move point there.
              (let ((comint-move-point-for-output nil))
                (comint-output-filter
                 proc (propertize (if padded
                                      (concat text (propertize
                                                    "\n" 'neat-repl-padding t))
                                    text)
                                  'face face)))
              (when (and overwrite padded)
                (let ((eol (1- prompt))
                      (inhibit-field-text-motion t))
                  (with-silent-modifications
                    (put-text-property (save-excursion
                                         (goto-char eol)
                                         (line-beginning-position))
                                       eol 'comint-must-overwrite t)))))
          (setq neat-repl--ansi-context ansi-color-context-region)
          (setq-local ansi-color-context-region streamed-ansi)
          (set-marker pmark end)
          (set-marker neat-repl--prompt-start prompt)
          (setq comint-last-prompt
                (cons (copy-marker prompt) (copy-marker end)))
          (let ((inhibit-read-only t))
            (font-lock-append-text-property prompt end 'font-lock-face
                                            'comint-highlight-prompt))
          (set-marker prompt nil)
          (set-marker end nil)))
      (when comint-move-point-for-output
        (dolist (window (get-buffer-window-list nil nil t))
          (comint-adjust-window-point window proc))))))

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
    (setq neat-repl--prompt-start nil)
    (cond
     ((or neat-repl--connection-dead
          (and conn (not (neat-connection-live-p conn))))
      (message "Neat: connection is closed"))
     ((not conn)
      (message "Neat: no connection in this buffer"))
     ((string-empty-p trimmed)
      (neat-repl--insert-prompt))
     (t
      (let ((request (neat-repl--request-create)))
        (neat-eval
         conn trimmed
         :callback (lambda (resp)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (neat-repl--render-response resp request))))))))))

(defun neat-repl--handle-disconnect (conn)
  "Mark CONN's REPL buffer as closed, if it has one.
Run from `neat-disconnect-functions'.  Idempotent: subsequent calls
on the same dead connection are no-ops."
  (when-let* ((buf (neat-repl-buffer-for conn))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (unless neat-repl--connection-dead
        (setq neat-repl--connection-dead t)
        (when (process-live-p (get-buffer-process (current-buffer)))
          (neat-repl--emit-output ";; connection closed\n"
                                  'neat-repl-error))))))

;;;###autoload
(add-hook 'neat-disconnect-functions #'neat-repl--handle-disconnect)

(cl-defstruct (neat-repl--request (:constructor neat-repl--request-create)
                                  (:copier nil))
  "Rendering state for one request whose responses land in a REPL buffer.
NS is the namespace the request named explicitly, if any.  The prompt
follows the `ns' a reply reports only when NS is nil.  A request that
names its own `ns' gets it bound for that one eval and then dropped,
so following it would leave the prompt showing a namespace the REPL
isn't in.  Without one the eval runs in the session's namespace, and
an `(ns ...)' or `(in-ns ...)' in it really does move the session,
whether it was typed into the REPL or sent from a source buffer.

SAW-ERR is set once the request has printed anything on `err'.  EX
holds the last `ex' it reported, held back until `done' and dropped
if `err' had something to say: Basilisp and jank put the whole
traceback in both fields, and nREPL's `ex' is only the exception
class, so `err' is the better of the two whenever there is one."
  ns saw-err ex)

(defun neat-repl--request-update (request resp)
  "Note RESP's `err' and `ex' on REQUEST and return what's due now.
The result is (EX . PROBLEM).  EX is an `ex' to show now: at `done',
when nothing came on `err'.  PROBLEM is the error status line from
`neat-response-error', leaving out a bare `error' that an earlier
`err' or `ex' already explained."
  (when (neat-bencode-get resp "err")
    (setf (neat-repl--request-saw-err request) t))
  (when-let* ((ex (neat-bencode-get resp "ex")))
    (setf (neat-repl--request-ex request) ex))
  (let ((saw-err (neat-repl--request-saw-err request))
        (ex (neat-repl--request-ex request)))
    (cons (and (member "done" (neat-bencode-get resp "status"))
               (not saw-err)
               ex)
          (neat-response-error resp (or saw-err ex)))))

(defun neat-repl--render-response (resp &optional request)
  "Insert the user-visible parts of nREPL response RESP into the buffer.
REQUEST is the `neat-repl--request' RESP answers; pass the same one
for every response to a request.  Without it RESP is rendered on its
own, and an `ex' only shows up if `done' comes in the same message."
  (let* ((request (or request (neat-repl--request-create)))
         (proc (get-buffer-process (current-buffer)))
         (value (neat-bencode-get resp "value"))
         (out (neat-bencode-get resp "out"))
         (err (neat-bencode-get resp "err"))
         (ns (neat-bencode-get resp "ns"))
         (status (neat-bencode-get resp "status"))
         (due (neat-repl--request-update request resp)))
    ;; Track the namespace as soon as we see one so the next prompt
    ;; reflects any `(in-ns ...)' or namespace-switching form.  A
    ;; `namespace-not-found' reply names the namespace that isn't there.
    (when (and ns (not (neat-repl--request-ns request))
               (not (member "namespace-not-found" status)))
      (setq neat-repl--current-ns ns)
      (when neat-current-connection
        (setf (neat-connection-ns neat-current-connection) ns)))
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
      (when (car due)
        (comint-output-filter
         proc (propertize (format "%s\n" (car due)) 'face 'neat-repl-error)))
      (when (cdr due)
        (comint-output-filter
         proc (propertize (format ";; %s\n" (cdr due)) 'face 'neat-repl-error)))
      (when neat-current-connection
        (neat-repl--answer-status neat-current-connection resp))
      ;; A dead connection gets its own marker from
      ;; `neat-repl--handle-disconnect'; a prompt would only invite
      ;; input that has nowhere to go.
      (when (and (member "done" status)
                 (not (member "connection-closed" status)))
        (neat-repl--insert-prompt)))))

(defun neat-repl--answer-status (conn resp)
  "Deal with the statuses in RESP from CONN that need the user.
A `need-input' gets answered from the minibuffer and an
`unknown-session' brings up the offer of a new session.  Shared by
the REPL and by source-buffer evals with no REPL buffer to go to."
  (let ((status (neat-bencode-get resp "status")))
    (when (and (member "need-input" status)
               (neat-connection-live-p conn))
      (neat-repl--handle-need-input conn resp))
    (when (member "unknown-session" status)
      (neat-repl--offer-new-session conn resp))))

(defun neat-repl--offer-new-session (conn resp)
  "Offer to clone a fresh session after CONN's server disowned one.
RESP is the `unknown-session' reply.  When it's about CONN's current
session that session is dropped from CONN, so later requests stop
sending it, and the user gets asked whether to clone a new one.
Declining leaves CONN without a session, which nREPL answers with a
throwaway session for every request.  Either way the namespace the
old session was in is forgotten too, by CONN and by its REPL, since
whatever comes next starts out somewhere else."
  (let ((session (neat-bencode-get resp "session"))
        (current (neat-connection-session conn)))
    (when (and current
               (or (null session) (equal session current))
               (neat-connection-live-p conn))
      (setf (neat-connection-session conn) nil
            (neat-connection-ns conn) nil)
      (when-let* ((buf (neat-repl-buffer-for conn))
                  ((buffer-live-p buf)))
        (with-current-buffer buf
          (setq neat-repl--current-ns nil)))
      (when (y-or-n-p
             "Neat: the server doesn't know this session; clone a new one? ")
        (neat-clone-session conn)))))

(defun neat-repl--handle-unhandled-message (conn message)
  "Show `out' and `err' from MESSAGE above the prompt in CONN's REPL.
Run from `neat-unhandled-message-functions', so MESSAGE belongs to no
pending request: output with no `id', or late output from an eval
that already finished."
  (when-let* ((buf (neat-repl-buffer-for conn))
              ((buffer-live-p buf)))
    (let ((out (neat-bencode-get message "out"))
          (err (neat-bencode-get message "err")))
      (when (or out err)
        (with-current-buffer buf
          (when out (neat-repl--emit-output out 'neat-repl-output))
          (when err (neat-repl--emit-output err 'neat-repl-error)))))))

;; Not autoloaded: the handler only exists once this file is loaded,
;; and a client used without the REPL has no buffer to show output in.
(add-hook 'neat-unhandled-message-functions
          #'neat-repl--handle-unhandled-message)

(defvar neat-repl-stdin-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c C-d") #'neat-repl-stdin-eof)
    map)
  "Keymap for the minibuffer prompt that answers a `need-input'.")

(defvar neat-repl--stdin-eof nil
  "Non-nil once the `stdin' prompt has been answered with end-of-file.")

(defun neat-repl-stdin-eof ()
  "Answer the pending `stdin' prompt with end-of-file instead of a line."
  (interactive)
  (setq neat-repl--stdin-eof t)
  (exit-minibuffer))

(defun neat-repl--handle-need-input (conn resp)
  "Prompt the user for a line of input and ship it to CONN via the `stdin' op.
Server-side reads (`read-line', `input', ...) trigger a `need-input'
status response RESP that pauses the eval until the client replies
with a `stdin' op.  The reply goes to RESP's session, which needn't
be CONN's current one.  A trailing newline is appended so
`read-line'-style readers finish.

\\<neat-repl-stdin-map>\\[neat-repl-stdin-eof] at the prompt sends \
end-of-file (an empty `stdin') instead.
`C-g' interrupts the eval, or sends end-of-file when the server can't
interrupt, so the eval doesn't sit waiting forever."
  (let ((session (neat-bencode-get resp "session"))
        (neat-repl--stdin-eof nil))
    (condition-case nil
        (let ((input (read-from-minibuffer
                      (substitute-command-keys
                       (concat "stdin (\\<neat-repl-stdin-map>"
                               "\\[neat-repl-stdin-eof] for EOF): "))
                      nil neat-repl-stdin-map)))
          (neat-stdin conn (if neat-repl--stdin-eof "" (concat input "\n"))
                      :session session))
      (quit (if (neat-op-supported-p conn "interrupt")
                (neat-repl--interrupt conn session (neat-bencode-get resp "id"))
              (neat-stdin conn "" :session session))))))

(defun neat-repl--interrupt (conn &optional session interrupt-id)
  "Ask CONN's server to interrupt the eval it's running.
SESSION and INTERRUPT-ID go to `neat-interrupt'.  Signals a
`user-error' up front when the server doesn't advertise `interrupt',
and reports any reply that says the interrupt didn't happen.  Without
that the user would just see nothing happen."
  (unless (neat-op-supported-p conn "interrupt")
    (user-error "Neat: the server doesn't support interrupt"))
  (neat-interrupt
   conn session interrupt-id
   (lambda (resp)
     (let ((status (neat-bencode-get resp "status")))
       (cond ((member "unknown-op" status)
              (message "Neat: the server doesn't support interrupt"))
             ((member "session-idle" status)
              (message "Neat: nothing to interrupt"))
             ((member "session-ephemeral" status)
              (message "Neat: can't interrupt an eval with no session"))
             ((member "interrupt-id-mismatch" status)
              (message "Neat: that eval isn't the one running"))
             ((member "error" status)
              (message "Neat: the interrupt failed")))))))

(defun neat-repl-interrupt ()
  "Send an `interrupt' op to the REPL's connection."
  (interactive)
  (if neat-current-connection
      (neat-repl--interrupt neat-current-connection)
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
