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

(provide 'neat-test)
;;; neat-test.el ends here
