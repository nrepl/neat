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
         ("status" "done")))
      (expect neat-repl--current-ns :to-equal "myapp.core")))

  (it "leaves `neat-repl--current-ns' alone when the response has no `ns'"
    (with-temp-buffer
      (setq-local neat-repl--current-ns "stays")
      (neat-repl--render-response
       '(("id" . "1") ("value" . "nil") ("status" "done")))
      (expect neat-repl--current-ns :to-equal "stays"))))

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

  (it "is a no-op when no REPL buffer exists for the connection"
    (let ((conn (neat-connection--make :host "h" :port 999999)))
      (expect (neat-repl--handle-disconnect conn) :not :to-throw))))

;;; neat-repl-test.el ends here
