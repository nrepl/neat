;;; neat-bencode-test.el --- Tests for neat-bencode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup suite for the bencode codec.

;;; Code:

(require 'buttercup)
(require 'neat-bencode)

(defun neat-bencode-test--bytes (str)
  "Return STR as a unibyte byte string (UTF-8)."
  (if (multibyte-string-p str)
      (encode-coding-string str 'utf-8 t)
    str))

(describe "neat-bencode-encode"
  (it "encodes zero"
    (expect (neat-bencode-encode 0) :to-equal "i0e"))

  (it "encodes positive integers"
    (expect (neat-bencode-encode 42) :to-equal "i42e"))

  (it "encodes negative integers"
    (expect (neat-bencode-encode -17) :to-equal "i-17e"))

  (it "encodes ASCII strings with byte-length prefix"
    (expect (neat-bencode-encode "spam") :to-equal "4:spam"))

  (it "encodes the empty string"
    (expect (neat-bencode-encode "") :to-equal "0:"))

  (it "uses byte length, not character count, for multibyte strings"
    (let ((encoded (neat-bencode-encode "héllo")))
      ;; "héllo" is 5 chars but 6 bytes when UTF-8 encoded
      (expect encoded :to-equal (neat-bencode-test--bytes "6:héllo"))))

  (it "encodes the empty list"
    (expect (neat-bencode-encode '()) :to-equal "le"))

  (it "encodes a list of integers"
    (expect (neat-bencode-encode '(1 2 3)) :to-equal "li1ei2ei3ee"))

  (it "encodes a mixed list"
    (expect (neat-bencode-encode '(1 "spam" (2 3)))
            :to-equal "li1e4:spamli2ei3eee"))

  (it "encodes an alist as a dict and sorts the keys"
    ;; Input order: foo, bar.  Output should be sorted: bar, foo.
    (expect (neat-bencode-encode '(("foo" . 1) ("bar" . 2)))
            :to-equal "d3:bari2e3:fooi1ee"))

  (it "accepts symbol keys"
    (expect (neat-bencode-encode '((op . "describe")))
            :to-equal "d2:op8:describee"))

  (it "rejects values it cannot encode"
    (expect (neat-bencode-encode 3.14) :to-throw 'neat-bencode-error))

  (it "produces a unibyte byte string"
    (expect (multibyte-string-p (neat-bencode-encode "héllo"))
            :to-be nil)))

(describe "neat-bencode-decode"
  (it "decodes integers"
    (expect (neat-bencode-decode "i42e") :to-equal '(42 . 4)))

  (it "decodes negative integers"
    (expect (neat-bencode-decode "i-17e") :to-equal '(-17 . 5)))

  (it "decodes bytestrings"
    (expect (neat-bencode-decode "4:spam") :to-equal '("spam" . 6)))

  (it "decodes empty bytestrings"
    (expect (neat-bencode-decode "0:") :to-equal '("" . 2)))

  (it "decodes UTF-8 bytestrings into multibyte elisp strings"
    (let* ((input (neat-bencode-test--bytes "6:héllo"))
           (result (neat-bencode-decode input)))
      (expect (car result) :to-equal "héllo")))

  (it "decodes empty lists"
    (expect (neat-bencode-decode "le") :to-equal '(nil . 2)))

  (it "decodes lists"
    (expect (neat-bencode-decode "li1ei2ei3ee")
            :to-equal '((1 2 3) . 11)))

  (it "decodes nested lists"
    (expect (neat-bencode-decode "lli1ei2eeli3ei4eee")
            :to-equal '(((1 2) (3 4)) . 18)))

  (it "decodes dicts as alists with string keys"
    (expect (neat-bencode-decode "d3:bari2e3:fooi1ee")
            :to-equal '((("bar" . 2) ("foo" . 1)) . 18)))

  (it "advances past the consumed value, leaving trailing bytes alone"
    (let ((result (neat-bencode-decode "i42ei7e")))
      (expect (car result) :to-equal 42)
      (expect (cdr result) :to-equal 4)))

  (it "can decode the second value by passing a start offset"
    (let* ((input "i42ei7e")
           (first (neat-bencode-decode input))
           (second (neat-bencode-decode input (cdr first))))
      (expect (car second) :to-equal 7)
      (expect (cdr second) :to-equal 7))))

(describe "neat-bencode-decode (streaming)"
  (it "returns nil for a truncated integer"
    (expect (neat-bencode-decode "i42") :to-be nil))

  (it "returns nil for a truncated bytestring header"
    (expect (neat-bencode-decode "4") :to-be nil))

  (it "returns nil when bytestring payload is short"
    (expect (neat-bencode-decode "4:sp") :to-be nil))

  (it "returns nil for an unfinished list"
    (expect (neat-bencode-decode "li1e") :to-be nil))

  (it "returns nil for an unfinished dict"
    (expect (neat-bencode-decode "d3:foo") :to-be nil))

  (it "decodes once the rest of the bytes arrive"
    (let* ((partial "d3:foo")
           (full (concat partial "i1ee"))
           (result (neat-bencode-decode full)))
      (expect (car result) :to-equal '(("foo" . 1)))
      (expect (cdr result) :to-equal (length full)))))

(describe "neat-bencode-decode (errors)"
  (it "signals on a stray closing 'e'"
    (expect (neat-bencode-decode "e") :to-throw 'neat-bencode-error))

  (it "signals on a dict with an odd number of items"
    (expect (neat-bencode-decode "d3:fooe") :to-throw 'neat-bencode-error)))

(describe "roundtrip"
  (it "preserves common nREPL-shaped messages"
    (dolist (msg '(((op . "describe"))
                   ((op . "eval") (code . "(+ 1 2)") (id . "1"))
                   ((id . "2") (session . "abc") (status ("done")))))
      (let* ((encoded (neat-bencode-encode msg))
             (decoded (neat-bencode-decode encoded))
             (re-encoded (neat-bencode-encode (car decoded))))
        ;; The decoded form uses string keys, so we can't compare to the
        ;; original directly; but the encoded form is canonical, so a
        ;; re-encode should match the first encode.
        (expect re-encoded :to-equal encoded)))))

(describe "neat-bencode-get"
  (it "returns the value associated with a key"
    (expect (neat-bencode-get '(("foo" . 1) ("bar" . 2)) "foo")
            :to-equal 1))

  (it "returns nil when the key is absent"
    (expect (neat-bencode-get '(("foo" . 1)) "missing")
            :to-be nil))

  (it "returns DEFAULT when the key is absent"
    (expect (neat-bencode-get '(("foo" . 1)) "missing" 'fallback)
            :to-equal 'fallback)))

(describe "neat-bencode-keys"
  (it "returns the keys in wire order"
    (expect (neat-bencode-keys '(("bar" . 2) ("foo" . 1)))
            :to-equal '("bar" "foo"))))

;;; neat-bencode-test.el ends here
