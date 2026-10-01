;;; neat-repl-test.el --- Tests for the REPL buffer helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the pure helpers in `neat-repl' -- balance-aware
;; input checks, prompt formatting, and namespace tracking via the
;; rendered response.  The full comint UI is not driven here; that's
;; out of scope for the fast suite.

;;; Code:

(require 'buttercup)
(require 'neat-bencode)
(require 'neat-repl)

(defmacro neat-repl-test--with-repl (conn &rest body)
  "Run BODY in a real `neat-repl-mode' buffer for a fake connection.
CONN is bound to the connection, which has no network process; the
buffer gets its usual comint pipe process, so rendering really
inserts text."
  (declare (indent 1))
  `(let* ((neat-repl-history-file nil)
          (,conn (neat-connection--make :host "h" :port 1))
          (buf (neat-repl-create-buffer ,conn)))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (kill-buffer buf))))

(defun neat-repl-test--text ()
  "Return the current buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(describe "neat-repl--input-complete-p"
  (it "accepts an empty string as complete"
    (expect (neat-repl--input-complete-p "") :to-be-truthy))

  (it "accepts whitespace-only input as complete"
    (expect (neat-repl--input-complete-p "   \n  ") :to-be-truthy))

  (it "accepts a balanced form as complete"
    (expect (neat-repl--input-complete-p "(+ 1 2)") :to-be-truthy))

  (it "accepts a balanced multi-line form as complete"
    (expect (neat-repl--input-complete-p "(let [x 1\n      y 2]\n  (+ x y))")
            :to-be-truthy))

  (it "rejects an unclosed open paren"
    (expect (neat-repl--input-complete-p "(+ 1 2") :to-be nil))

  (it "rejects mismatched bracket types as unbalanced"
    ;; Emacs Lisp syntax doesn't recognise [ ] as paren-like, so this
    ;; spec keeps to plain (), which is the common Clojure/Lisp case.
    (expect (neat-repl--input-complete-p "(foo (bar 1)") :to-be nil))

  (it "rejects input that ends inside a string"
    (expect (neat-repl--input-complete-p "(println \"hello") :to-be nil))

  (it "accepts input with a closed string"
    (expect (neat-repl--input-complete-p "(println \"hi\")") :to-be-truthy)))

(describe "neat-repl--prompt"
  (it "uses `neat-repl-default-ns' when no namespace is known"
    (let ((neat-repl-prompt-format "%s> ")
          (neat-repl-default-ns "neat")
          (neat-repl--current-ns nil))
      (expect (neat-repl--prompt) :to-equal "neat> ")))

  (it "uses the tracked namespace when one is set"
    (let ((neat-repl-prompt-format "%s> ")
          (neat-repl-default-ns "neat")
          (neat-repl--current-ns "myapp.core"))
      (expect (neat-repl--prompt) :to-equal "myapp.core> ")))

  (it "honours a custom prompt format"
    (let ((neat-repl-prompt-format "[%s] => ")
          (neat-repl-default-ns "neat")
          (neat-repl--current-ns nil))
      (expect (neat-repl--prompt) :to-equal "[neat] => "))))

(describe "neat-repl--render-response (namespace tracking)"
  (it "updates `neat-repl--current-ns' when the server reports `ns'"
    (with-temp-buffer
      (setq-local neat-repl--current-ns nil)
      ;; Render a response with an `ns' field.  We don't have a comint
      ;; process attached, so the user-visible writes are no-ops, but
      ;; the buffer-local ns slot should still get updated.
      (neat-repl--render-response
       '(("id" . "1")
         ("ns" . "myapp.core")
         ("value" . "nil")
         ("status" "done"))
       (neat-repl--request-create))
      (expect neat-repl--current-ns :to-equal "myapp.core")))

  (it "leaves `neat-repl--current-ns' alone when the response has no `ns'"
    (with-temp-buffer
      (setq-local neat-repl--current-ns "stays")
      (neat-repl--render-response
       '(("id" . "1") ("value" . "nil") ("status" "done"))
       (neat-repl--request-create))
      (expect neat-repl--current-ns :to-equal "stays")))

  (it "follows the `ns' reported for input typed into the REPL"
    (neat-repl-test--with-repl _conn
      (let (callback)
        (cl-letf (((symbol-function 'neat-connection-live-p) (lambda (_) t))
                  ((symbol-function 'neat-eval)
                   (lambda (_c _code &rest plist)
                     (setq callback (plist-get plist :callback)))))
          (neat-repl--input-sender nil "(in-ns 'myapp.core)")
          (funcall callback '(("id" . "1") ("ns" . "myapp.core")
                              ("value" . "nil")))
          (expect neat-repl--current-ns :to-equal "myapp.core")))))

  (it "ignores the `ns' of an eval that named its own"
    ;; The source buffer sent an explicit ns, which the server only
    ;; binds for that eval; the REPL is still wherever it was.
    (neat-repl-test--with-repl _conn
      (setq neat-repl--current-ns "user")
      (let ((request (neat-repl--request-create :ns "myapp.core")))
        (dolist (resp '((("id" . "1") ("ns" . "myapp.core") ("value" . "nil"))
                        (("id" . "1") ("status" "done"))))
          (neat-repl--render-response resp request)))
      (expect neat-repl--current-ns :to-equal "user")
      (expect (neat-repl-test--text) :to-equal "nil\nuser> ")))

  (it "follows a source-buffer eval that named no ns"
    ;; Say it ran `(ns myapp.core)': the session really moved.
    (neat-repl-test--with-repl _conn
      (setq neat-repl--current-ns "user")
      (neat-repl-test--render-all
       '((("id" . "1") ("ns" . "myapp.core") ("value" . "nil"))
         (("id" . "1") ("status" "done"))))
      (expect neat-repl--current-ns :to-equal "myapp.core"))))

(defun neat-repl-test--render-all (responses)
  "Render RESPONSES in order as replies to one request."
  (let ((request (neat-repl--request-create)))
    (dolist (resp responses)
      (neat-repl--render-response resp request))))

(describe "neat-repl--render-response (output)"
  (it "renders a value and then a prompt on done"
    (neat-repl-test--with-repl _conn
      (neat-repl-test--render-all
       '((("id" . "1") ("value" . "3"))
         (("id" . "1") ("status" "done"))))
      (expect (neat-repl-test--text) :to-equal "3\nneat> ")))

  (it "renders out before the value"
    (neat-repl-test--with-repl _conn
      (neat-repl-test--render-all
       '((("id" . "1") ("out" . "hi\n"))
         (("id" . "1") ("value" . ":ok"))
         (("id" . "1") ("status" "done"))))
      (expect (neat-repl-test--text) :to-equal "hi\n:ok\nneat> ")
      (expect (get-text-property 1 'face) :to-be 'neat-repl-output)))

  (it "shows err and drops an ex that came after it"
    (neat-repl-test--with-repl _conn
      (neat-repl-test--render-all
       '((("id" . "1") ("err" . "Traceback: boom\n"))
         (("id" . "1") ("ex" . "Traceback: boom") ("status" "eval-error"))
         (("id" . "1") ("status" "done"))))
      (expect (neat-repl-test--text) :to-equal "Traceback: boom\nneat> ")))

  (it "drops an ex that came before err"
    ;; nREPL sends the eval-error status with `ex' first, then `err'.
    (neat-repl-test--with-repl _conn
      (neat-repl-test--render-all
       '((("id" . "1") ("ex" . "class java.lang.ArithmeticException")
          ("status" "eval-error"))
         (("id" . "1") ("err" . "Divide by zero\n"))
         (("id" . "1") ("status" "done"))))
      (expect (neat-repl-test--text) :to-equal "Divide by zero\nneat> ")))

  (it "shows ex at done when nothing came on err"
    (neat-repl-test--with-repl _conn
      (neat-repl-test--render-all
       '((("id" . "1") ("ex" . "boom") ("status" "eval-error"))
         (("id" . "1") ("status" "done"))))
      (expect (neat-repl-test--text) :to-equal "boom\nneat> ")
      (expect (get-text-property 1 'face) :to-be 'neat-repl-error)))

  (it "shows ex and done arriving together without a request"
    (neat-repl-test--with-repl _conn
      (neat-repl--render-response
       '(("id" . "1") ("ex" . "boom") ("status" "eval-error" "done")))
      (expect (neat-repl-test--text) :to-equal "boom\nneat> "))))

(describe "neat-repl--render-response (connection-closed)"
  (it "inserts a prompt on a normal done"
    (neat-repl-test--with-repl _conn
      (neat-repl--render-response '(("id" . "1") ("status" "done")))
      (expect (neat-repl-test--text) :to-equal "neat> ")))

  (it "doesn't prompt for input on the synthesized connection-closed done"
    (neat-repl-test--with-repl _conn
      (neat-repl--render-response
       '(("id" . "1") ("status" "done" "connection-closed")))
      (expect (neat-repl-test--text) :to-equal ""))))

(describe "neat-repl--handle-unhandled-message"
  (it "puts late output above the waiting prompt"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
      (expect (neat-repl-test--text) :to-equal "late\nneat> ")))

  (it "joins chunks that don't end in a newline"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-unhandled-message conn '(("out" . "la")))
      (expect (neat-repl-test--text) :to-equal "la\nneat> ")
      (neat-repl--handle-unhandled-message conn '(("out" . "te\n")))
      (expect (neat-repl-test--text) :to-equal "late\nneat> ")))

  (it "leaves half-typed input alone"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (goto-char (point-max))
      (insert "(foo")
      (neat-repl--handle-unhandled-message conn '(("err" . "oops\n")))
      (expect (neat-repl-test--text) :to-equal "oops\nneat> (foo")
      (expect (get-text-property 1 'face) :to-be 'neat-repl-error)
      (expect (point) :to-equal (point-max))))

  (it "keeps landing above the prompt after an eval finishes"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--render-response '(("id" . "1") ("value" . "3")))
      (neat-repl--render-response '(("id" . "1") ("status" "done")))
      (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
      (expect (neat-repl-test--text) :to-equal "neat> 3\nlate\nneat> ")))

  (it "streams in at the end while an eval is still running"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (setq neat-repl--prompt-start nil)
      (neat-repl--handle-unhandled-message conn '(("out" . "x\n")))
      (expect (neat-repl-test--text) :to-equal "neat> x\n")))

  (it "runs the text through comint's output filters"
    (neat-repl-test--with-repl conn
      (let (seen)
        (neat-repl--insert-prompt)
        (setq-local comint-preoutput-filter-functions (list #'upcase))
        (setq-local comint-output-filter-functions
                    (list (lambda (_s)
                            (push (buffer-substring-no-properties
                                   comint-last-output-start
                                   (process-mark (get-buffer-process
                                                  (current-buffer))))
                                  seen))))
        (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
        (expect seen :to-equal '("LATE\n"))
        (expect (neat-repl-test--text) :to-equal "LATE\nneat> ")
        ;; And the process mark is back after the prompt.
        (expect (marker-position (process-mark (get-buffer-process buf)))
                :to-equal (point-max)))))

  (it "renders ANSI colors like any other output"
    (require 'ansi-color)
    (neat-repl-test--with-repl conn
      (let ((ansi-color-for-comint-mode t))
        (neat-repl--insert-prompt)
        (setq-local comint-output-filter-functions
                    (list #'ansi-color-process-output))
        (neat-repl--handle-unhandled-message
         conn '(("out" . "\e[31mred\e[0m\n")))
        (expect (neat-repl-test--text) :to-equal "red\nneat> "))))

  (it "applies comint's carriage motion"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-unhandled-message conn '(("out" . "50%\r100%\n")))
      (expect (neat-repl-test--text) :to-equal "100%\nneat> ")))

  (it "works with the buffer narrowed away from the prompt"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (narrow-to-region (point-min) (point-min))
      (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
      (widen)
      (expect (neat-repl-test--text) :to-equal "late\nneat> ")))

  (it "leaves the prompt a highlighted, read-only prompt"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-unhandled-message conn '(("out" . "late")))
      (let ((start (marker-position neat-repl--prompt-start)))
        (expect (buffer-substring-no-properties start (point-max))
                :to-equal "neat> ")
        (expect (marker-position (car comint-last-prompt)) :to-equal start)
        (expect (memq 'comint-highlight-prompt
                      (ensure-list (get-text-property start 'font-lock-face)))
                :to-be-truthy)
        (expect (get-text-property start 'read-only) :to-be-truthy)
        (expect (get-text-property 1 'read-only) :to-be nil))))

  (it "doesn't trip over a color that streamed output left on"
    (require 'ansi-color)
    (neat-repl-test--with-repl conn
      (let ((ansi-color-for-comint-mode t)
            (debug-on-error nil))
        (setq-local comint-output-filter-functions
                    (list #'ansi-color-process-output))
        (neat-repl--insert-prompt)
        ;; An eval from the REPL is running and prints bold, never
        ;; switching it off again.
        (setq neat-repl--prompt-start nil)
        (neat-repl--handle-unhandled-message conn '(("out" . "\e[1mbold\n")))
        (neat-repl--insert-prompt)
        (expect (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
                :not :to-throw)
        (expect (neat-repl--handle-unhandled-message conn '(("out" . "more\n")))
                :not :to-throw)
        (expect (neat-repl-test--text)
                :to-equal "neat> bold\nlate\nmore\nneat> "))))

  (it "carries a color across two chunks above the prompt"
    (require 'ansi-color)
    (neat-repl-test--with-repl conn
      (let ((ansi-color-for-comint-mode t))
        (neat-repl--insert-prompt)
        (setq-local comint-output-filter-functions
                    (list #'ansi-color-process-output))
        (neat-repl--handle-unhandled-message conn '(("out" . "\e[31mred\n")))
        (neat-repl--handle-unhandled-message conn '(("out" . "still\e[0m\n")))
        (expect (neat-repl-test--text) :to-equal "red\nstill\nneat> ")
        (expect (cl-some (lambda (o) (overlay-get o 'face))
                         (overlays-at (+ (point-min) (length "red\n"))))
                :to-be-truthy))))

  (it "overwrites a line that ended in a carriage return"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (dolist (chunk '("10%\r" "20%\r" "30%\r" "done\n"))
        (neat-repl--handle-unhandled-message conn `(("out" . ,chunk))))
      (expect (neat-repl-test--text) :to-equal "done\nneat> ")))

  (it "moves point to the input, not onto the prompt"
    (neat-repl-test--with-repl conn
      (let ((window (selected-window)))
        (neat-repl--insert-prompt)
        (set-window-buffer window buf)
        (setq-local comint-move-point-for-output t)
        (goto-char (point-min))
        (set-window-point window (point-min))
        (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
        (expect (window-point window) :to-equal (point-max))
        (expect (with-selected-window window
                  (insert "x")
                  (neat-repl-test--text))
                :to-equal "late\nneat> x"))))

  (it "keeps a window that started at the prompt showing the new text"
    (neat-repl-test--with-repl conn
      (let ((window (selected-window)))
        (neat-repl--insert-prompt)
        (set-window-buffer window buf)
        (set-window-start window (point-min))
        (neat-repl--handle-unhandled-message conn '(("out" . "late\n")))
        (expect (window-start window) :to-equal (point-min)))))

  (it "ignores messages with no output"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-unhandled-message conn '(("status" "done")))
      (expect (neat-repl-test--text) :to-equal "neat> "))))

(describe "neat-repl--handle-need-input"
  (it "reads input from the minibuffer and sends it via the stdin op"
    (let ((conn (neat-connection--make :host "h" :port 1))
          captured)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "world"))
                ((symbol-function 'neat-stdin)
                 (lambda (_c input &rest _) (setq captured input))))
        (neat-repl--handle-need-input conn)
        ;; The handler must append a trailing newline so read-line-style
        ;; readers actually finish.
        (expect captured :to-equal "world\n"))))

  (it "interrupts the eval when the user hits C-g at the prompt"
    (let ((conn (neat-connection--make :host "h" :port 1))
          interrupted)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'neat-interrupt)
                 (lambda (c &rest _) (setq interrupted c)))
                ((symbol-function 'neat-stdin)
                 (lambda (&rest _) (error "should not send stdin"))))
        (neat-repl--handle-need-input conn)
        (expect interrupted :to-be conn)))))

(describe "neat-repl--handle-disconnect"
  (it "sets the dead flag on the conn's REPL buffer"
    (let* ((conn (neat-connection--make :host "h" :port 1))
           (buf (get-buffer-create (neat-repl-buffer-name conn))))
      (unwind-protect
          (with-current-buffer buf
            ;; Pretend we're in neat-repl-mode without booting comint.
            (setq-local neat-repl--connection-dead nil)
            (neat-repl--handle-disconnect conn)
            (expect neat-repl--connection-dead :to-be-truthy))
        (kill-buffer buf))))

  (it "is idempotent on repeated calls"
    (let* ((conn (neat-connection--make :host "h" :port 2))
           (buf (get-buffer-create (neat-repl-buffer-name conn))))
      (unwind-protect
          (with-current-buffer buf
            (setq-local neat-repl--connection-dead nil)
            (expect (progn (neat-repl--handle-disconnect conn)
                           (neat-repl--handle-disconnect conn)
                           t)
                    :to-be-truthy))
        (kill-buffer buf))))

  (it "lets a reconnect in the same buffer start on a fresh line"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-disconnect conn)
      ;; What `neat' does with a buffer that's still around.
      (neat-repl-create-buffer conn)
      (neat-repl--insert-prompt)
      (expect (neat-repl-test--text)
              :to-equal ";; connection closed\nneat> \nneat> ")))

  (it "puts the marker above a waiting prompt"
    (neat-repl-test--with-repl conn
      (neat-repl--insert-prompt)
      (neat-repl--handle-disconnect conn)
      (expect (neat-repl-test--text)
              :to-equal ";; connection closed\nneat> ")))

  (it "is a no-op when no REPL buffer exists for the connection"
    (let ((conn (neat-connection--make :host "h" :port 999999)))
      (expect (neat-repl--handle-disconnect conn) :not :to-throw))))

;;; neat-repl-test.el ends here
