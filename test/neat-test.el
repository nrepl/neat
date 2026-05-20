;;; neat-test.el --- Tests for the top-level neat.el helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-local helpers from `neat.el' that don't need a live server.

;;; Code:

(require 'buttercup)
(require 'neat)

(defun neat-test--thing-at (text pos)
  "Insert TEXT in a temp buffer, jump to POS, return the eldoc thing.
POS is a 1-indexed buffer position."
  (with-temp-buffer
    (insert text)
    (goto-char pos)
    (neat--eldoc-thing-at-point)))

(describe "neat--eldoc-thing-at-point"
  (it "returns the symbol when point is inside it"
    ;; "(str)" -> point between `s' and `t'.
    (expect (neat-test--thing-at "(str)" 3) :to-equal "str"))

  (it "returns the symbol when point is right after it"
    ;; "str" -> point at end of buffer.
    (expect (neat-test--thing-at "str" 4) :to-equal "str"))

  (it "falls back to the enclosing list head in trailing whitespace"
    ;; "(str )" -> point between space and `)'.
    (expect (neat-test--thing-at "(str )" 6) :to-equal "str"))

  (it "falls back to the enclosing list head in inter-arg whitespace"
    ;; "(str 1  2)" -> point between the two spaces.
    (expect (neat-test--thing-at "(str 1  2)" 8) :to-equal "str"))

  (it "returns the innermost head in nested calls"
    ;; "(str (sub ))" -> point between space and the inner `)'.
    (expect (neat-test--thing-at "(str (sub ))" 11) :to-equal "sub"))

  (it "returns nil at top-level whitespace"
    (expect (neat-test--thing-at "  " 2) :to-be nil)))

(defun neat-test--arg-index-at (text pos)
  "Insert TEXT, jump to POS, return `neat--current-arg-index'."
  (with-temp-buffer
    (insert text)
    (goto-char pos)
    (neat--current-arg-index)))

(describe "neat--current-arg-index"
  (it "is 0 when point is on the first arg"
    ;; "(foo a b)" -> point at `a'.
    (expect (neat-test--arg-index-at "(foo a b)" 6) :to-equal 0))

  (it "is 1 when point is on the second arg"
    ;; "(foo a b)" -> point at `b'.
    (expect (neat-test--arg-index-at "(foo a b)" 8) :to-equal 1))

  (it "is 0 in the whitespace right after the head"
    ;; "(foo  a)" -> point in the second space.
    (expect (neat-test--arg-index-at "(foo  a)" 6) :to-equal 0))

  (it "counts past completed args when in trailing whitespace"
    ;; "(foo a b )" -> point between `b' and `)'.
    (expect (neat-test--arg-index-at "(foo a b )" 10) :to-equal 2))

  (it "returns nil at the top level"
    (expect (neat-test--arg-index-at "(foo a)" 1) :to-be nil)))

(describe "neat--lispy-parse-arglist"
  (it "parses a single-arity arglist"
    (expect (neat--lispy-parse-arglist "[f coll]")
            :to-equal '(("f" "coll"))))

  (it "parses a variadic single-arity arglist"
    (expect (neat--lispy-parse-arglist "[x & rest]")
            :to-equal '(("x" "&" "rest"))))

  (it "parses a multi-arity arglist"
    (expect (neat--lispy-parse-arglist "([] [x] [x & ys])")
            :to-equal '(() ("x") ("x" "&" "ys"))))

  (it "returns nil for an unparseable arglist (destructuring with maps)"
    (expect (neat--lispy-parse-arglist "[{:keys [a b]} coll]")
            :to-be nil)))

(describe "neat--pick-arity"
  (it "picks the fixed arity matching ARG-INDEX"
    (expect (neat--pick-arity '(("x") ("x" "y")) 1)
            :to-equal '("x" "y")))

  (it "picks the variadic arity for out-of-range ARG-INDEX"
    (expect (neat--pick-arity '(("x") ("x" "&" "ys")) 4)
            :to-equal '("x" "&" "ys")))

  (it "returns nil when no arity fits"
    (expect (neat--pick-arity '(("x")) 5) :to-be nil)))

(describe "neat--lispy-highlight-arglist"
  (it "highlights the correct param in a single arity"
    (let ((out (neat--lispy-highlight-arglist "[f coll]" 1)))
      (expect (substring-no-properties out) :to-equal "[f coll]")
      (expect (get-text-property (+ (length "[f ") (- (length "coll") 1))
                                 'face out)
              :to-equal 'eldoc-highlight-function-argument)))

  (it "highlights the rest param when past the variadic marker"
    (let ((out (neat--lispy-highlight-arglist "[x & rest]" 3)))
      (expect (substring-no-properties out) :to-equal "[x & rest]")
      ;; The `rest' token should carry the highlight face.
      (expect (get-text-property (+ (length "[x & ") 0) 'face out)
              :to-equal 'eldoc-highlight-function-argument)))

  (it "falls back to the raw string when parsing fails"
    (expect (neat--lispy-highlight-arglist "[{:keys [a]} c]" 0)
            :to-equal "[{:keys [a]} c]"))

  (it "falls back to the raw string when ARG-INDEX is nil"
    (expect (neat--lispy-highlight-arglist "[a b]" nil)
            :to-equal "[a b]")))

(defun neat-test--capture-eval-plist (body-fn)
  "Run BODY-FN with `neat-eval' stubbed; return the plist it was called with."
  (let (captured)
    (cl-letf (((symbol-function 'neat--require-connection)
               (lambda () 'stub-conn))
              ((symbol-function 'neat-eval)
               (lambda (_conn _code &rest plist) (setq captured plist))))
      (funcall body-fn))
    captured))

(describe "neat--eval-string source-location metadata"
  (it "computes 1-indexed line/column for the start of the form"
    (with-temp-buffer
      (insert "line1\nline2\n  (foo)\n")
      ;; Position of the open paren on line 3: after "line1\nline2\n  ".
      (let* ((pos (1+ (length "line1\nline2\n  ")))
             (plist (neat-test--capture-eval-plist
                     (lambda () (neat--eval-string "(foo)" pos)))))
        (expect (plist-get plist :line) :to-equal 3)
        (expect (plist-get plist :column) :to-equal 3))))

  (it "uses character columns, not display columns (tabs count as one)"
    (with-temp-buffer
      (insert "\t(foo)")
      (let ((plist (neat-test--capture-eval-plist
                    (lambda () (neat--eval-string "(foo)" 2)))))
        ;; "(" is at character offset 1 -> column 2.  A display-column
        ;; calculation would say 9 because of tab-width.
        (expect (plist-get plist :column) :to-equal 2))))

  (it "ignores narrowing when computing line numbers"
    (with-temp-buffer
      (insert "a\nb\nc\nd\n")
      (narrow-to-region 5 7)
      (let ((plist (neat-test--capture-eval-plist
                    (lambda () (neat--eval-string "c" 5)))))
        ;; Without ABSOLUTE=t, narrowing would yield line 1.
        (expect (plist-get plist :line) :to-equal 3))))

  (it "omits line/column when no position is given"
    (let ((plist (neat-test--capture-eval-plist
                  (lambda () (neat--eval-string "(+ 1 2)")))))
      (expect (plist-get plist :line) :to-be nil)
      (expect (plist-get plist :column) :to-be nil)))

  (it "passes the namespace from neat-buffer-ns-function"
    (with-temp-buffer
      (setq neat-ns "my.ns")
      (let ((plist (neat-test--capture-eval-plist
                    (lambda () (neat--eval-string "(+ 1 2)")))))
        (expect (plist-get plist :ns) :to-equal "my.ns"))))

  (it "honours a custom neat-buffer-ns-function override"
    (let ((neat-buffer-ns-function (lambda () "derived.ns")))
      (with-temp-buffer
        (let ((plist (neat-test--capture-eval-plist
                      (lambda () (neat--eval-string "(+ 1 2)")))))
          (expect (plist-get plist :ns) :to-equal "derived.ns"))))))

(describe "neat--lookup-file-path"
  (it "returns plain paths unchanged"
    (expect (neat--lookup-file-path "/tmp/foo.clj")
            :to-equal "/tmp/foo.clj"))

  (it "strips a file:// URL prefix"
    (expect (neat--lookup-file-path "file:///tmp/foo.clj")
            :to-equal "/tmp/foo.clj"))

  (it "strips a bare file: prefix"
    (expect (neat--lookup-file-path "file:/tmp/foo.clj")
            :to-equal "/tmp/foo.clj"))

  (it "returns nil for jar URLs"
    (expect (neat--lookup-file-path "jar:file:/p/clj.jar!/clojure/core.clj")
            :to-be nil))

  (it "returns nil for other scheme URLs"
    (expect (neat--lookup-file-path "http://example/foo.clj") :to-be nil))

  (it "returns nil for a nil input"
    (expect (neat--lookup-file-path nil) :to-be nil)))

(describe "neat--xref-location-from-info"
  (it "returns an xref location for a resolvable file"
    (let* ((tmp (make-temp-file "neat-xref-"))
           (info `(("file" . ,tmp) ("line" . 4) ("column" . 3))))
      (unwind-protect
          (let ((loc (neat--xref-location-from-info info)))
            (expect loc :not :to-be nil)
            (expect (xref-location-group loc) :to-equal tmp))
        (delete-file tmp))))

  (it "returns nil when the file doesn't exist on disk"
    (let ((info '(("file" . "/no/such/file.clj") ("line" . 1))))
      (expect (neat--xref-location-from-info info) :to-be nil)))

  (it "returns nil for jar URLs"
    (let ((info '(("file" . "jar:file:/p/clj.jar!/clojure/core.clj")
                  ("line" . 10))))
      (expect (neat--xref-location-from-info info) :to-be nil)))

  (it "treats a 0 column as missing rather than going negative"
    (let* ((tmp (make-temp-file "neat-xref-"))
           (info `(("file" . ,tmp) ("line" . 1) ("column" . 0))))
      (unwind-protect
          (let ((loc (neat--xref-location-from-info info)))
            (expect loc :not :to-be nil))
        (delete-file tmp)))))

(provide 'neat-test)
;;; neat-test.el ends here
