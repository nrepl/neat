;;; neat-client-test.el --- Tests for neat-client  -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the dispatch and op-construction logic without
;; touching the network.  We build `neat-connection' structs directly,
;; stub `process-live-p' and `process-send-string' with `cl-letf', and
;; drive `neat-client--drain' by hand to simulate the server.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'neat-bencode)
(require 'neat-client)

(defun neat-client-test--push-bytes (conn bytes)
  "Append BYTES to CONN's recv buffer and drain."
  (setf (neat-connection-recv-buffer conn)
        (concat (neat-connection-recv-buffer conn) bytes))
  (neat-client--drain conn))

(describe "neat-client--drain"
  (it "dispatches a complete message to the registered callback"
    (let* ((conn (neat-connection--make))
           (got '()))
      (puthash "1" (lambda (m) (push m got))
               (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1") ("value" . "3"))))
      (expect (length got) :to-equal 1)
      (expect (neat-bencode-get (car got) "value") :to-equal "3")))

  (it "handles fragmented input across two filter calls"
    (let* ((conn (neat-connection--make))
           (got '())
           (bytes (neat-bencode-encode '(("id" . "1") ("value" . "42"))))
           (mid (/ (length bytes) 2)))
      (puthash "1" (lambda (m) (push m got))
               (neat-connection-pending conn))
      (neat-client-test--push-bytes conn (substring bytes 0 mid))
      (expect got :to-equal '())
      (neat-client-test--push-bytes conn (substring bytes mid))
      (expect (length got) :to-equal 1)))

  (it "drains multiple back-to-back messages in one buffer"
    (let* ((conn (neat-connection--make))
           (count 0))
      (puthash "1" (lambda (_) (cl-incf count))
               (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (concat (neat-bencode-encode '(("id" . "1") ("value" . "a")))
                    (neat-bencode-encode '(("id" . "1") ("out" . "b")))
                    (neat-bencode-encode '(("id" . "1") ("status" . ("done"))))))
      (expect count :to-equal 3)))

  (it "prunes the pending entry when status contains 'done'"
    (let ((conn (neat-connection--make)))
      (puthash "1" #'ignore (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1") ("status" . ("done")))))
      (expect (gethash "1" (neat-connection-pending conn)) :to-be nil)))

  (it "ignores messages whose id has no callback registered"
    (let ((conn (neat-connection--make)))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "99") ("value" . "?"))))
      ;; If we got here without throwing, we're good.
      (expect t :to-be-truthy)))

  (it "shields the filter from a buggy callback"
    (let ((conn (neat-connection--make)))
      (puthash "1" (lambda (_) (error "boom"))
               (neat-connection-pending conn))
      ;; The error should be caught and not propagated out of drain.
      (expect (neat-client-test--push-bytes
               conn (neat-bencode-encode
                     '(("id" . "1") ("status" . ("done")))))
              :not :to-throw)
      ;; And the done entry was still pruned.
      (expect (gethash "1" (neat-connection-pending conn)) :to-be nil))))

(describe "neat-clone-session"
  (it "captures new-session from the response and assigns it to the connection"
    (let ((conn (neat-connection--make))
          (sent nil))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-clone-session conn)
        (expect sent :not :to-be nil)
        ;; Verify the sent wire bytes describe a clone op.
        (let* ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "clone")
          (expect (neat-bencode-get decoded "id") :to-equal "1"))
        ;; Simulate the server's response.
        (neat-client-test--push-bytes
         conn (neat-bencode-encode '(("id" . "1")
                                     ("new-session" . "S-123")
                                     ("status" . ("done")))))
        (expect (neat-connection-session conn) :to-equal "S-123")))))

(describe "neat-describe"
  (it "stores the response on the connection's capabilities slot"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore))
        (neat-describe conn)
        (neat-client-test--push-bytes
         conn (neat-bencode-encode
               '(("id" . "1")
                 ("versions" . (("nrepl" . (("major" . 1))))))))
        (expect (neat-bencode-get (neat-connection-capabilities conn)
                                  "versions")
                :not :to-be nil)))))

(describe "neat-eval"
  (it "includes the session and code fields in the sent message"
    (let ((conn (neat-connection--make))
          (sent nil))
      (setf (neat-connection-session conn) "S-1")
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-eval conn "(+ 1 2)")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "eval")
          (expect (neat-bencode-get decoded "code") :to-equal "(+ 1 2)")
          (expect (neat-bencode-get decoded "session") :to-equal "S-1"))))))

(describe "neat-send"
  (it "increments the request id for each call"
    (let ((conn (neat-connection--make))
          ids)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore))
        (push (neat-send conn '((op . "describe"))) ids)
        (push (neat-send conn '((op . "describe"))) ids)
        (push (neat-send conn '((op . "describe"))) ids)
        (expect (nreverse ids) :to-equal '("1" "2" "3")))))

  (it "errors when the connection is not live"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil)))
        (expect (neat-send conn '((op . "describe")))
                :to-throw 'error)))))

;;; neat-client-test.el ends here
