;;; neat-bencode.el --- Bencode codec for nREPL  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/neat
;; Version: 0.0.1
;; Keywords: languages, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Bencode is the wire format used by nREPL.  This module is a pure,
;; dependency-free codec: it does no I/O of its own.  Callers feed in
;; byte strings (typically accumulated from a network process filter)
;; and get parsed elisp values back, or pass elisp values in to get
;; back a byte string suitable for shipping to a server.
;;
;; Decoding is iterative and stack-based so deeply nested messages
;; can't blow the elisp call stack.  When the input is truncated
;; mid-value the decoder returns nil rather than signalling an error,
;; which makes it easy to drive from a streaming filter.

;;; Code:

(require 'cl-lib)

(define-error 'neat-bencode-error "bencode error")


;;;; Encoding

(defun neat-bencode-encode (obj)
  "Encode OBJ as a bencode byte string (unibyte).

OBJ may be:
  - an integer
  - a string (encoded as a bytestring; multibyte strings are UTF-8 encoded)
  - a list of encodable values
  - an alist whose keys are strings or symbols (encoded as a bencode dict
    with keys sorted lexicographically by their byte representation)

Signals `neat-bencode-error' for values that do not match any of the
above."
  (cond
   ((integerp obj)
    (neat-bencode--ascii (format "i%de" obj)))
   ((stringp obj)
    (let ((bytes (neat-bencode--to-bytes obj)))
      (concat (neat-bencode--ascii (number-to-string (length bytes)))
              (neat-bencode--ascii ":")
              bytes)))
   ((neat-bencode--alistp obj)
    (let ((pairs (cl-sort (mapcar (lambda (cell)
                                    (cons (neat-bencode--key-string (car cell))
                                          (cdr cell)))
                                  obj)
                          #'string< :key #'car)))
      (apply #'concat
             `(,(neat-bencode--ascii "d")
               ,@(mapcar (lambda (cell)
                           (concat (neat-bencode-encode (car cell))
                                   (neat-bencode-encode (cdr cell))))
                         pairs)
               ,(neat-bencode--ascii "e")))))
   ((listp obj)
    (apply #'concat
           `(,(neat-bencode--ascii "l")
             ,@(mapcar #'neat-bencode-encode obj)
             ,(neat-bencode--ascii "e"))))
   (t (signal 'neat-bencode-error (list "cannot encode" obj)))))

(defun neat-bencode--ascii (s)
  "Force ASCII string S to a unibyte representation."
  (if (multibyte-string-p s)
      (encode-coding-string s 'us-ascii t)
    s))

(defun neat-bencode--to-bytes (s)
  "Return string S as a unibyte byte sequence (UTF-8)."
  (if (multibyte-string-p s)
      (encode-coding-string s 'utf-8 t)
    s))

(defun neat-bencode--key-string (k)
  "Coerce K (a string or symbol) into a string usable as a dict key."
  (cond
   ((stringp k) k)
   ((symbolp k) (symbol-name k))
   (t (signal 'neat-bencode-error (list "non-string dict key" k)))))

(defun neat-bencode--alistp (obj)
  "Return non-nil if OBJ has the shape of a dict-style alist.
That is, every cell of OBJ is a cons whose car is a string or symbol.
An empty list is not an alist (it encodes as a list)."
  (and (consp obj)
       (consp (car obj))
       (cl-every (lambda (cell)
                   (and (consp cell)
                        (let ((k (car cell)))
                          (or (stringp k) (symbolp k)))))
                 obj)))


;;;; Decoding

(defun neat-bencode-decode (str &optional start)
  "Decode the bencode value at position START (default 0) in STR.

STR should be a unibyte string of bencoded bytes.  Returns a cons
\(VALUE . NEXT-INDEX), where NEXT-INDEX is the position immediately
after the consumed bytes.  Returns nil if STR is truncated mid-value;
callers driving a streaming decoder can append more bytes and retry.

Signals `neat-bencode-error' on malformed input.

Decoded shapes:
  - integers          -> integers
  - bytestrings       -> multibyte strings (UTF-8 decoded)
  - lists             -> lists
  - dicts             -> alists with string keys, in wire order
                        (which is sorted, per the spec)"
  (catch 'neat-bencode--incomplete
    (let ((pos (or start 0))
          (len (length str))
          (stack nil))
      (cl-labels
          ((require-byte ()
             (when (>= pos len)
               (throw 'neat-bencode--incomplete nil))
             (aref str pos))
           (emit (value)
             (if stack
                 (push value (cdr (car stack)))
               (throw 'neat-bencode--result (cons value pos)))))
        (catch 'neat-bencode--result
          (while t
            (let ((ch (require-byte)))
              (cond
               ;; integer: i<digits>e
               ((eq ch ?i)
                (cl-incf pos)
                (let ((end (cl-position ?e str :start pos)))
                  (unless end (throw 'neat-bencode--incomplete nil))
                  (let ((n (string-to-number (substring str pos end))))
                    (setq pos (1+ end))
                    (emit n))))
               ;; bytestring: <length>:<bytes>
               ((and (>= ch ?0) (<= ch ?9))
                (let ((colon (cl-position ?: str :start pos)))
                  (unless colon (throw 'neat-bencode--incomplete nil))
                  (let* ((blen (string-to-number (substring str pos colon)))
                         (data-start (1+ colon))
                         (data-end (+ data-start blen)))
                    (when (> data-end len)
                      (throw 'neat-bencode--incomplete nil))
                    (let ((bytes (substring str data-start data-end)))
                      (setq pos data-end)
                      (emit (decode-coding-string bytes 'utf-8))))))
               ;; list: l...e
               ((eq ch ?l)
                (cl-incf pos)
                (push (cons 'list nil) stack))
               ;; dict: d...e
               ((eq ch ?d)
                (cl-incf pos)
                (push (cons 'dict nil) stack))
               ;; close current container
               ((eq ch ?e)
                (unless stack
                  (signal 'neat-bencode-error
                          (list "unexpected 'e' at" pos)))
                (cl-incf pos)
                (let* ((frame (pop stack))
                       (type (car frame))
                       (items (nreverse (cdr frame))))
                  (emit (pcase type
                          ('list items)
                          ('dict (neat-bencode--items-to-alist items))))))
               (t (signal 'neat-bencode-error
                          (list "unexpected byte" ch "at" pos)))))))))))

(defun neat-bencode--items-to-alist (items)
  "Pair a flat sequence of (key value key value ...) ITEMS into an alist."
  (when (cl-oddp (length items))
    (signal 'neat-bencode-error (list "odd number of items in dict")))
  (cl-loop for (k v) on items by #'cddr collect (cons k v)))


;;;; Convenience accessors

(defsubst neat-bencode-get (dict key &optional default)
  "Look up KEY in DICT (an alist with string keys).
Return the associated value, or DEFAULT (nil by default) if absent."
  (let ((cell (assoc key dict)))
    (if cell (cdr cell) default)))

(defun neat-bencode-keys (dict)
  "Return the list of keys in DICT, in the order they appear."
  (mapcar #'car dict))

(provide 'neat-bencode)
;;; neat-bencode.el ends here
