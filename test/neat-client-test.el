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

  (it "shields the filter from malformed bencode (production semantics)"
    ;; A stray `e' is the simplest malformed input: `neat-bencode-decode'
    ;; signals `neat-bencode-error' on it.  The drain has to catch that
    ;; or the filter cycle dies silently.
    (let ((conn (neat-connection--make))
          (debug-on-error nil))
      (expect (neat-client-test--push-bytes conn "e") :not :to-throw)
      ;; And after dropping the bad bytes, a subsequent good message
      ;; still dispatches normally.
      (let ((got nil))
        (puthash "1" (lambda (m) (push m got))
                 (neat-connection-pending conn))
        (neat-client-test--push-bytes
         conn (neat-bencode-encode '(("id" . "1") ("value" . "ok"))))
        (expect (length got) :to-equal 1))))

  (it "shields the filter from a buggy callback (production semantics)"
    ;; The dispatch wraps callbacks in `condition-case-unless-debug',
    ;; which deliberately steps aside under `debug-on-error' so the
    ;; underlying bug surfaces during interactive development.  This
    ;; test pins down the production behaviour, with the debug guard
    ;; off.
    (let ((conn (neat-connection--make))
          (debug-on-error nil))
      (puthash "1" (lambda (_) (error "boom"))
               (neat-connection-pending conn))
      (expect (neat-client-test--push-bytes
               conn (neat-bencode-encode
                     '(("id" . "1") ("status" . ("done")))))
              :not :to-throw)
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
          (expect (neat-bencode-get decoded "session") :to-equal "S-1")))))

  (it "includes file/line/column/ns when provided"
    (let ((conn (neat-connection--make))
          sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-eval conn "(+ 1 2)"
                   :file "/tmp/foo.clj" :line 42 :column 7 :ns "my.ns")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "file")
                  :to-equal "/tmp/foo.clj")
          (expect (neat-bencode-get decoded "line") :to-equal 42)
          (expect (neat-bencode-get decoded "column") :to-equal 7)
          (expect (neat-bencode-get decoded "ns") :to-equal "my.ns")))))

  (it "omits file/line/column/ns when not provided"
    (let ((conn (neat-connection--make))
          sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-eval conn "(+ 1 2)")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (assoc "file" decoded) :to-be nil)
          (expect (assoc "line" decoded) :to-be nil)
          (expect (assoc "column" decoded) :to-be nil)
          (expect (assoc "ns" decoded) :to-be nil))))))

(describe "neat-load-file"
  (it "builds a load-file op with contents and metadata"
    (let ((conn (neat-connection--make))
          sent)
      (setf (neat-connection-session conn) "S-2")
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-load-file conn "(ns foo) (def x 1)"
                        :file-path "/tmp/foo.clj" :file-name "foo.clj")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "load-file")
          (expect (neat-bencode-get decoded "file")
                  :to-equal "(ns foo) (def x 1)")
          (expect (neat-bencode-get decoded "file-path")
                  :to-equal "/tmp/foo.clj")
          (expect (neat-bencode-get decoded "file-name") :to-equal "foo.clj")
          (expect (neat-bencode-get decoded "session") :to-equal "S-2")))))

  (it "omits path/name/session when not provided"
    (let ((conn (neat-connection--make))
          sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-load-file conn "(+ 1 2)")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "load-file")
          (expect (neat-bencode-get decoded "file") :to-equal "(+ 1 2)")
          (expect (assoc "file-path" decoded) :to-be nil)
          (expect (assoc "file-name" decoded) :to-be nil)
          (expect (assoc "session" decoded) :to-be nil))))))

(describe "neat-completions"
  (it "builds a completions op with prefix, ns, and session"
    (let ((conn (neat-connection--make))
          sent)
      (setf (neat-connection-session conn) "S-7")
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-completions conn "foo" "user")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "completions")
          (expect (neat-bencode-get decoded "prefix") :to-equal "foo")
          (expect (neat-bencode-get decoded "ns") :to-equal "user")
          (expect (neat-bencode-get decoded "session") :to-equal "S-7"))))))

(describe "neat-stdin"
  (it "builds a stdin op with input and session"
    (let ((conn (neat-connection--make))
          sent)
      (setf (neat-connection-session conn) "S-9")
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-stdin conn "hello\n")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "stdin")
          (expect (neat-bencode-get decoded "stdin") :to-equal "hello\n")
          (expect (neat-bencode-get decoded "session") :to-equal "S-9"))))))

(describe "neat-lookup"
  (it "builds a lookup op with sym and ns"
    (let ((conn (neat-connection--make))
          sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-lookup conn "map" "clojure.core")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "lookup")
          (expect (neat-bencode-get decoded "sym") :to-equal "map")
          (expect (neat-bencode-get decoded "ns") :to-equal "clojure.core"))))))

(describe "neat-connections registry"
  ;; These tests use `make-pipe-process' as a stand-in for the real
  ;; network process: it's alive, supports process-put/get and filter/
  ;; sentinel hooks, but doesn't actually open a socket.
  (it "pushes a fresh connection onto neat-connections on connect"
    (let ((neat-connections nil)
          (proc (make-pipe-process :name "neat-test-pipe-1" :noquery t)))
      (unwind-protect
          (cl-letf (((symbol-function 'open-network-stream)
                     (lambda (_n _b _h _p &rest _) proc)))
            (let ((conn (neat-connect "h" 1)))
              (expect neat-connections :to-equal (list conn))))
        (when (process-live-p proc) (delete-process proc)))))

  (it "removes the connection on neat-disconnect"
    (let ((neat-connections nil)
          (proc (make-pipe-process :name "neat-test-pipe-2" :noquery t)))
      (unwind-protect
          (cl-letf (((symbol-function 'open-network-stream)
                     (lambda (_n _b _h _p &rest _) proc)))
            (let ((conn (neat-connect "h" 2)))
              (neat-disconnect conn)
              (expect neat-connections :to-equal nil)))
        (when (process-live-p proc) (delete-process proc)))))

  (it "runs cleanup exactly once when neat-disconnect kills a live process"
    ;; The sentinel and neat-disconnect used to both call flush + hook,
    ;; relying on idempotency to avoid double-firing.  The single
    ;; cleanup path makes this an assertion.
    (let ((neat-connections nil)
          (calls 0)
          (proc (make-pipe-process :name "neat-test-pipe-once"
                                   :noquery t)))
      (unwind-protect
          (let ((neat-disconnect-functions
                 (list (lambda (_c) (cl-incf calls)))))
            (cl-letf (((symbol-function 'open-network-stream)
                       (lambda (_n _b _h _p &rest _) proc)))
              (let ((conn (neat-connect "h" 1)))
                (neat-disconnect conn)
                (expect calls :to-equal 1))))
        (when (process-live-p proc) (delete-process proc)))))

  (it "runs cleanup when neat-disconnect is called on an already-dead conn"
    (let ((neat-connections nil)
          (calls 0)
          (proc (make-pipe-process :name "neat-test-pipe-dead"
                                   :noquery t)))
      (unwind-protect
          (let ((neat-disconnect-functions
                 (list (lambda (_c) (cl-incf calls)))))
            (cl-letf (((symbol-function 'open-network-stream)
                       (lambda (_n _b _h _p &rest _) proc)))
              (let ((conn (neat-connect "h" 1)))
                ;; Detach the sentinel so killing the process doesn't
                ;; auto-cleanup; the conn looks "dead but uncleaned".
                (set-process-sentinel proc #'ignore)
                (delete-process proc)
                ;; Now neat-disconnect runs cleanup itself.
                (neat-disconnect conn)
                (expect calls :to-equal 1))))
        (when (process-live-p proc) (delete-process proc)))))

  (it "runs neat-disconnect-functions when a connection's process dies"
    (let ((neat-connections nil)
          (got '())
          (proc (make-pipe-process :name "neat-test-pipe-hook"
                                   :noquery t)))
      (unwind-protect
          (let ((neat-disconnect-functions
                 (list (lambda (c) (push c got)))))
            (cl-letf (((symbol-function 'open-network-stream)
                       (lambda (_n _b _h _p &rest _) proc)))
              (let ((conn (neat-connect "h" 1)))
                (neat-disconnect conn)
                ;; The sentinel runs synchronously when delete-process
                ;; closes a pipe process.
                (expect got :to-equal (list conn)))))
        (when (process-live-p proc) (delete-process proc)))))

  (it "demotes neat-default-connection when its target disconnects"
    (let ((neat-connections nil)
          (neat-default-connection nil)
          (proc-a (make-pipe-process :name "neat-test-pipe-a" :noquery t))
          (proc-b (make-pipe-process :name "neat-test-pipe-b" :noquery t)))
      (unwind-protect
          (let ((stubbed-procs (list proc-a proc-b)))
            (cl-letf (((symbol-function 'open-network-stream)
                       (lambda (_n _b _h _p &rest _) (pop stubbed-procs))))
              (let* ((conn-a (neat-connect "h" 1))
                     (conn-b (neat-connect "h" 2)))
                ;; Pretend conn-a is the active default.
                (setq neat-default-connection conn-a)
                (neat-disconnect conn-a)
                ;; conn-a is gone; default should fall through to the
                ;; next-most-recent live connection, which is conn-b.
                (expect neat-default-connection :to-be conn-b))))
        (dolist (p (list proc-a proc-b))
          (when (process-live-p p) (delete-process p)))))))

(describe "neat-active-connection"
  (it "prefers the buffer-local override when it's live"
    (let* ((proc-a (make-pipe-process :name "neat-test-active-a" :noquery t))
           (proc-b (make-pipe-process :name "neat-test-active-b" :noquery t))
           (conn-a (neat-connection--make :host "h" :port 1 :process proc-a))
           (conn-b (neat-connection--make :host "h" :port 2 :process proc-b))
           (neat-default-connection conn-b))
      (unwind-protect
          (with-temp-buffer
            (setq neat-current-connection conn-a)
            (expect (neat-active-connection) :to-be conn-a))
        (when (process-live-p proc-a) (delete-process proc-a))
        (when (process-live-p proc-b) (delete-process proc-b)))))

  (it "falls back to the default when the buffer-local override is dead"
    (let* ((proc-a (make-pipe-process :name "neat-test-active-a2" :noquery t))
           (proc-b (make-pipe-process :name "neat-test-active-b2" :noquery t))
           (conn-a (neat-connection--make :host "h" :port 1 :process proc-a))
           (conn-b (neat-connection--make :host "h" :port 2 :process proc-b))
           (neat-default-connection conn-b))
      (unwind-protect
          (with-temp-buffer
            (setq neat-current-connection conn-a)
            (delete-process proc-a)
            (expect (neat-active-connection) :to-be conn-b))
        (when (process-live-p proc-b) (delete-process proc-b)))))

  (it "returns nil when neither override nor default is live"
    (with-temp-buffer
      (let ((neat-default-connection nil))
        (setq neat-current-connection nil)
        (expect (neat-active-connection) :to-be nil)))))

(describe "message log helpers"
  (it "passes short messages through untruncated"
    (let ((neat-message-log-max-message-length 1000))
      (expect (neat--message-log-format '((foo . "bar")))
              :to-equal (with-output-to-string (pp '((foo . "bar")))))))

  (it "truncates messages longer than the limit and tags the byte count"
    (let ((neat-message-log-max-message-length 20))
      (let ((out (neat--message-log-format
                  (cons 'big (make-string 500 ?x)))))
        (expect (length out) :to-be-greater-than 20)
        (expect out :to-match "truncated, [0-9]+ bytes total"))))

  (it "respects nil (no truncation) for the message length cap"
    (let ((neat-message-log-max-message-length nil))
      (let ((out (neat--message-log-format (cons 'big (make-string 500 ?x)))))
        (expect out :not :to-match "truncated"))))

  (it "trims the buffer to max-buffer-lines"
    (with-temp-buffer
      (neat-message-log-mode)
      (let ((neat-message-log-max-buffer-lines 3)
            (inhibit-read-only t))
        (insert "1\n2\n3\n4\n5\n6\n")
        (neat--message-log-trim)
        ;; The last 3 lines survive.
        (expect (buffer-string) :to-equal "4\n5\n6\n")))))

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
