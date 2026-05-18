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

(provide 'neat-test)
;;; neat-test.el ends here
