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
    (let ((conn (neat-connection--make))
          (neat-unhandled-message-functions nil))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "99") ("value" . "?"))))
      ;; If we got here without throwing, we're good.
      (expect t :to-be-truthy)))

  (it "shields the filter from malformed bencode (production semantics)"
    ;; A stray `e' is the simplest malformed input: `neat-bencode-decode'
    ;; signals `neat-bencode-error' on it.  The drain has to catch that
    ;; or the filter cycle dies silently.
    (let* ((conn (neat-connection--make))
           (neat-connections (list conn))
           (debug-on-error nil))
      (expect (neat-client-test--push-bytes conn "e") :not :to-throw)))

  (it "dispatches what precedes malformed bytes, then disconnects"
    ;; Past the bad byte there's no telling where the next message
    ;; starts, so the trailing (valid-looking) message must not be
    ;; dispatched; the pending callback hears about the disconnect
    ;; instead.
    (let* ((conn (neat-connection--make))
           (neat-connections (list conn))
           (neat-disconnect-functions nil)
           (debug-on-error nil)
           (got '()))
      (puthash "1" (lambda (m) (push m got))
               (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (concat (neat-bencode-encode '(("id" . "1") ("value" . "ok")))
                    "e"
                    (neat-bencode-encode '(("id" . "1") ("value" . "late")))))
      (setq got (nreverse got))
      (expect (length got) :to-equal 2)
      (expect (neat-bencode-get (car got) "value") :to-equal "ok")
      (expect (neat-bencode-get (cadr got) "value") :to-be nil)
      (expect (member "done" (neat-bencode-get (cadr got) "status"))
              :to-be-truthy)
      (expect neat-connections :to-equal nil)
      (expect (neat-connection-recv-buffer conn) :to-equal "")))

  (it "skips top-level values that aren't dicts"
    (let ((conn (neat-connection--make))
          (got '()))
      (puthash "1" (lambda (m) (push m got))
               (neat-connection-pending conn))
      (expect (neat-client-test--push-bytes
               conn (concat (neat-bencode-encode 42)
                            (neat-bencode-encode "id")
                            (neat-bencode-encode ["id" "1"])
                            (neat-bencode-encode '(("id" . "1")
                                                   ("value" . "ok")))))
              :not :to-throw)
      (expect (length got) :to-equal 1)
      (expect (neat-bencode-get (car got) "value") :to-equal "ok")))

  (it "ends a request on unknown-session even without done"
    (let ((conn (neat-connection--make :host "h" :port 1))
          got)
      (puthash "1" (lambda (m) (push m got)) (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1")
                                   ("status" "error" "unknown-session"))))
      (expect (neat-bencode-get (car got) "status")
              :to-equal '("error" "unknown-session" "done"))
      (expect (gethash "1" (neat-connection-pending conn)) :to-be nil)))

  (it "treats a status sent as a plain string as a list of one"
    (let ((conn (neat-connection--make))
          (got '()))
      (puthash "1" (lambda (m) (push m got)) (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1") ("status" . "done"))))
      (expect (neat-bencode-get (car got) "status") :to-equal '("done"))
      ;; So the request does finish.
      (expect (gethash "1" (neat-connection-pending conn)) :to-be nil)))

  (it "treats any other status that isn't a list as no status"
    (let ((conn (neat-connection--make))
          (got '()))
      (puthash "1" (lambda (m)
                     ;; The usual callback idiom must not choke.
                     (member "done" (neat-bencode-get m "status"))
                     (push m got))
               (neat-connection-pending conn))
      (expect (neat-client-test--push-bytes
               conn (concat (neat-bencode-encode '(("id" . "1")
                                                   ("status" . 7)))
                            (neat-bencode-encode '(("id" . "1")
                                                   ("value" . "next")))))
              :not :to-throw)
      ;; The message behind the bad one still went through.
      (expect (length got) :to-equal 2)
      (expect (assoc "status" (cadr got)) :to-be nil)
      (expect (gethash "1" (neat-connection-pending conn)) :not :to-be nil)))

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

(describe "neat-unhandled-message-functions"
  :var (conn seen)
  (before-each
    (setq conn (neat-connection--make)
          seen nil))

  (it "gets messages that carry no id"
    (let ((neat-unhandled-message-functions
           (list (lambda (c m) (push (cons c m) seen)))))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("out" . "hi\n"))))
      (expect (length seen) :to-equal 1)
      (expect (car (car seen)) :to-be conn)
      (expect (neat-bencode-get (cdr (car seen)) "out") :to-equal "hi\n")))

  (it "gets messages for an id that was never registered"
    (let ((neat-unhandled-message-functions
           (list (lambda (_c m) (push m seen)))))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "99") ("out" . "?"))))
      (expect (length seen) :to-equal 1)))

  (it "gets output for a request that already finished, and a second done"
    (let ((neat-unhandled-message-functions
           (list (lambda (_c m) (push m seen))))
          (got 0))
      (puthash "1" (lambda (_) (cl-incf got)) (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (concat (neat-bencode-encode '(("id" . "1") ("status" "done")))
                    (neat-bencode-encode '(("id" . "1") ("out" . "late\n")))
                    (neat-bencode-encode '(("id" . "1") ("status" "done")))))
      (expect got :to-equal 1)
      (setq seen (nreverse seen))
      (expect (length seen) :to-equal 2)
      (expect (neat-bencode-get (car seen) "out") :to-equal "late\n")
      (expect (neat-bencode-get (cadr seen) "status") :to-equal '("done"))))

  (it "doesn't get messages a pending callback claims"
    (let ((neat-unhandled-message-functions
           (list (lambda (_c m) (push m seen)))))
      (puthash "1" #'ignore (neat-connection-pending conn))
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1") ("out" . "x"))))
      (expect seen :to-be nil)))

  (it "prunes a finished request even when its callback is quit out of"
    (let ((conn (neat-connection--make :host "h" :port 1 :evals '("1"))))
      (puthash "1" (lambda (_) (signal 'quit nil))
               (neat-connection-pending conn))
      (condition-case nil
          (neat-client-test--push-bytes
           conn (neat-bencode-encode '(("id" . "1") ("status" "done"))))
        (quit nil))
      (expect (gethash "1" (neat-connection-pending conn)) :to-be nil)
      (expect (neat-eval-pending-p conn) :to-be nil)))

  (it "survives a hook function that errors (production semantics)"
    (let ((neat-unhandled-message-functions
           (list (lambda (_c _m) (error "boom"))))
          (debug-on-error nil)
          (got nil))
      (puthash "2" (lambda (m) (push m got)) (neat-connection-pending conn))
      (expect (neat-client-test--push-bytes
               conn (concat (neat-bencode-encode '(("out" . "x")))
                            (neat-bencode-encode '(("id" . "2") ("value" . "1")))))
              :not :to-throw)
      (expect (length got) :to-equal 1))))

(describe "neat-client--flush-pending"
  (it "hands each pending callback a connection-closed done and no err"
    (let* ((conn (neat-connection--make))
           (neat-connections (list conn))
           (neat-disconnect-functions nil)
           got)
      (puthash "7" (lambda (m) (push m got)) (neat-connection-pending conn))
      (neat-disconnect conn)
      (expect got :to-equal
              '((("id" . "7") ("status" "done" "connection-closed"))))
      (expect (hash-table-count (neat-connection-pending conn))
              :to-equal 0))))

(describe "sync helpers"
  (it "unregister the callback when the reply times out"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output) #'ignore))
        (expect (neat-completions-sync conn "ma" nil 0.05) :to-be nil)
        (expect (hash-table-count (neat-connection-pending conn))
                :to-equal 0)
        (expect (neat-lookup-sync conn "map" nil 0.05) :to-be nil)
        (expect (hash-table-count (neat-connection-pending conn))
                :to-equal 0))))

  (it "treat a connection that closes mid-request as no answer"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (neat-client--flush-pending conn))))
        (expect (neat-lookup-sync conn "map" nil 1) :to-be nil))))

  (it "keep what arrived before the timeout"
    (let ((conn (neat-connection--make))
          (sent nil))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (unless sent
                     (setq sent t)
                     (neat-client-test--push-bytes
                      conn (neat-bencode-encode
                            '(("id" . "1")
                              ("completions" . ((("candidate" . "map")))))))))))
        (expect (neat-completions-sync conn "ma" nil 0.1)
                :to-equal '((("candidate" . "map"))))
        (expect (hash-table-count (neat-connection-pending conn))
                :to-equal 0))))

  (it "return what arrived before done"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (neat-client-test--push-bytes
                    conn (neat-bencode-encode
                          '(("id" . "1")
                            ("completions" . ((("candidate" . "map"))))
                            ("status" "done")))))))
        (expect (neat-completions-sync conn "ma" nil 1)
                :to-equal '((("candidate" . "map"))))))))

(defun neat-client-test--fake-server (conn reply-fn)
  "Make CONN's sends get an answer from REPLY-FN.
REPLY-FN gets each decoded request and returns the reply dicts; the
returned function goes in place of `process-send-string'."
  (lambda (_proc bytes)
    (let ((request (car (neat-bencode-decode bytes))))
      (dolist (reply (funcall reply-fn request))
        (neat-client-test--push-bytes
         conn (neat-bencode-encode
               (cons (cons "id" (neat-bencode-get request "id")) reply)))))))

(describe "sync tooling ops and an ns the server turns down"
  :var (conn sent-ns)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 1)
          sent-ns nil))

  (it "retry completions without the ns"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) #'ignore)
              ((symbol-function 'process-send-string)
               (neat-client-test--fake-server
                conn (lambda (req)
                       (push (neat-bencode-get req "ns") sent-ns)
                       (if (neat-bencode-get req "ns")
                           '((("status" "done" "error" "namespace-not-found")))
                         '((("completions" . ((("candidate" . "map"))))
                            ("status" "done"))))))))
      (expect (neat-completions-sync conn "ma" "not.loaded")
              :to-equal '((("candidate" . "map"))))
      (expect (nreverse sent-ns) :to-equal '("not.loaded" nil))))

  (it "retry lookup without the ns after a bare error"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) #'ignore)
              ((symbol-function 'process-send-string)
               (neat-client-test--fake-server
                conn (lambda (req)
                       (push (neat-bencode-get req "ns") sent-ns)
                       (if (neat-bencode-get req "ns")
                           '((("status" "error" "done")))
                         '((("info" . (("name" . "map")))
                            ("status" "done"))))))))
      (expect (neat-lookup-sync conn "map" "not.loaded")
              :to-equal '(("name" . "map")))
      (expect (nreverse sent-ns) :to-equal '("not.loaded" nil))))

  (it "don't retry on an error that has nothing to do with the ns"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) #'ignore)
              ((symbol-function 'process-send-string)
               (neat-client-test--fake-server
                conn (lambda (req)
                       (push (neat-bencode-get req "ns") sent-ns)
                       '((("status" "done" "error" "unknown-op")))))))
      (expect (neat-completions-sync conn "ma" "user") :to-be nil)
      (expect sent-ns :to-equal '("user"))))

  (it "retry the async lookup without the ns too"
    (let (got)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (neat-client-test--fake-server
                  conn (lambda (req)
                         (push (neat-bencode-get req "ns") sent-ns)
                         (if (neat-bencode-get req "ns")
                             '((("status" "done" "error" "namespace-not-found")))
                           '((("info" . (("name" . "map")))
                              ("status" "done"))))))))
        (neat-lookup-async conn "map" "not.loaded" 1
                           (lambda (i) (setq got i))))
      (expect got :to-equal '(("name" . "map")))
      (expect (nreverse sent-ns) :to-equal '("not.loaded" nil))))

  (it "don't retry when the ns was fine"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) #'ignore)
              ((symbol-function 'process-send-string)
               (neat-client-test--fake-server
                conn (lambda (req)
                       (push (neat-bencode-get req "ns") sent-ns)
                       '((("status" "done")))))))
      (expect (neat-completions-sync conn "zz" "user") :to-be nil)
      (expect sent-ns :to-equal '("user")))))

(describe "neat-close-session-sync"
  (it "returns non-nil once the server confirms the close"
    (let ((conn (neat-connection--make :session "S-1")))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _)
                   (neat-client-test--push-bytes
                    conn (neat-bencode-encode
                          '(("id" . "1") ("status" "done" "session-closed")))))))
        (expect (neat-close-session-sync conn) :to-be-truthy)
        (expect (neat-connection-session conn) :to-be nil))))

  (it "gives up after the timeout"
    (let ((conn (neat-connection--make :session "S-1")))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output) #'ignore))
        (expect (neat-close-session-sync conn nil 0.05) :to-be nil)
        (expect (hash-table-count (neat-connection-pending conn))
                :to-equal 0))))

  (it "doesn't count a connection that went away as confirmation"
    (let ((conn (neat-connection--make :session "S-1")))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore)
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (neat-client--flush-pending conn))))
        (expect (neat-close-session-sync conn) :to-be nil)))))

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

(describe "neat-describe (ops)"
  (it "keeps the ops when done comes in a message of its own"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore))
        (neat-describe conn)
        ;; Empty dicts can't be written as alists, so raw bencode it is.
        (neat-client-test--push-bytes
         conn (concat "d2:id1:13:opsd5:clonede4:evaldeee"
                      (neat-bencode-encode '(("id" . "1") ("status" "done")))))
        (expect (neat-op-supported-p conn "eval") :to-be-truthy)
        (expect (neat-op-supported-p conn "interrupt") :to-be nil)))))

(describe "neat-op-supported-p"
  (it "reads ops sent as a dict"
    (let ((conn (neat-connection--make
                 :capabilities (car (neat-bencode-decode
                                     "d3:opsd5:clonede4:evald3:doc1:xeeee")))))
      (expect (neat-op-supported-p conn "eval") :to-be-truthy)
      (expect (neat-op-supported-p conn "clone") :to-be-truthy)
      (expect (neat-op-supported-p conn "interrupt") :to-be nil)))

  (it "reads ops sent as a list"
    (let ((conn (neat-connection--make
                 :capabilities (car (neat-bencode-decode
                                     "d3:opsl5:clone4:evalee")))))
      (expect (neat-op-supported-p conn "eval") :to-be-truthy)
      (expect (neat-op-supported-p conn "lookup") :to-be nil)))

  (it "treats every op as supported before describe answers"
    (expect (neat-op-supported-p (neat-connection--make) "interrupt")
            :to-be-truthy))

  (it "treats every op as supported when describe lists none"
    (dolist (caps (list '(("versions" . (("nrepl" . (("major" . 1))))))
                        (car (neat-bencode-decode "d3:opsdee"))
                        (car (neat-bencode-decode "d3:opslee"))))
      (expect (neat-op-supported-p (neat-connection--make :capabilities caps)
                                   "interrupt")
              :to-be-truthy))))

(describe "neat-interrupt"
  (it "builds an interrupt op with session and interrupt-id"
    (let ((conn (neat-connection--make))
          sent)
      (setf (neat-connection-session conn) "S-3")
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-interrupt conn nil "42")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "interrupt")
          (expect (neat-bencode-get decoded "session") :to-equal "S-3")
          (expect (neat-bencode-get decoded "interrupt-id")
                  :to-equal "42")))))

  (it "omits interrupt-id when none is given"
    (let ((conn (neat-connection--make))
          sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-interrupt conn "S-4")
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "session") :to-equal "S-4")
          (expect (assoc "interrupt-id" decoded) :to-be nil))))))

(describe "neat-close-session"
  (it "builds a close op for the current session"
    (let ((conn (neat-connection--make))
          sent)
      (setf (neat-connection-session conn) "S-5")
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-close-session conn)
        (let ((decoded (car (neat-bencode-decode sent))))
          (expect (neat-bencode-get decoded "op") :to-equal "close")
          (expect (neat-bencode-get decoded "session") :to-equal "S-5")))))

  (it "clears the connection's session when closing it"
    (let ((conn (neat-connection--make :session "S-5")))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore))
        (neat-close-session conn)
        (expect (neat-connection-session conn) :to-be nil))))

  (it "keeps the connection's session when closing another one"
    (let ((conn (neat-connection--make :session "S-5"))
          sent)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (setq sent s))))
        (neat-close-session conn "S-6")
        (expect (neat-bencode-get (car (neat-bencode-decode sent)) "session")
                :to-equal "S-6")
        (expect (neat-connection-session conn) :to-equal "S-5")))))

(describe "neat-response-error"
  (it "describes namespace-not-found, naming the ns when the server does"
    (expect (neat-response-error
             '(("status" "done" "error" "namespace-not-found")
               ("ns" . "my.typo")))
            :to-equal "namespace not found: my.typo")
    (expect (neat-response-error
             '(("status" "namespace-not-found" "done" "error")))
            :to-equal "namespace not found"))

  (it "describes unknown-session"
    (expect (neat-response-error '(("status" "error" "unknown-session" "done")))
            :to-equal "unknown session"))

  (it "describes unknown-op, naming the op when the server does"
    (expect (neat-response-error
             '(("status" "done" "error" "unknown-op") ("op" . "load-file")))
            :to-equal "unknown op: load-file"))

  (it "describes a bare error"
    (expect (neat-response-error '(("status" "error" "done")))
            :to-equal "error"))

  (it "leaves a bare error alone when the request already explained it"
    (expect (neat-response-error '(("status" "error" "done")) t) :to-be nil))

  (it "leaves an error that comes with its own explanation alone"
    (expect (neat-response-error '(("status" "error") ("err" . "boom\n")))
            :to-be nil)
    (expect (neat-response-error '(("status" "error") ("ex" . "boom")))
            :to-be nil))

  (it "is nil for ordinary statuses"
    (dolist (status '(("done") ("eval-error") ("need-input") nil))
      (expect (neat-response-error `(("status" . ,status))) :to-be nil))))

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

(describe "neat-eval-pending-p"
  :var (conn)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 1)))

  (it "is true from an eval until its done"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore))
      (expect (neat-eval-pending-p conn) :to-be nil)
      (neat-eval conn "(+ 1 2)")
      (expect (neat-eval-pending-p conn) :to-be-truthy)
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1") ("value" . "3"))))
      (expect (neat-eval-pending-p conn) :to-be-truthy)
      (neat-client-test--push-bytes
       conn (neat-bencode-encode '(("id" . "1") ("status" "done"))))
      (expect (neat-eval-pending-p conn) :to-be nil)))

  (it "counts load-file but not tooling ops"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore))
      (neat-completions conn "ma")
      (neat-lookup conn "map")
      (expect (neat-eval-pending-p conn) :to-be nil)
      (neat-load-file conn "(def x 1)")
      (expect (neat-eval-pending-p conn) :to-be-truthy)))

  (it "clears when the connection goes away"
    (let ((neat-connections (list conn))
          (neat-disconnect-functions nil))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore))
        (neat-eval conn "(Thread/sleep 10000)" :callback #'ignore))
      (neat-disconnect conn)
      (expect (neat-eval-pending-p conn) :to-be nil))))

(describe "neat-tooling-stalled-p"
  :var (conn)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 1)))

  (it "turns on when a sync request times out behind an eval"
    (setf (neat-connection-evals conn) '("5"))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'accept-process-output) #'ignore))
      (neat-completions-sync conn "ma" nil 0.05))
    (expect (neat-tooling-stalled-p conn) :to-be-truthy))

  (it "stays off when the timeout had no eval to blame"
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore)
              ((symbol-function 'accept-process-output) #'ignore))
      (neat-completions-sync conn "ma" nil 0.05))
    (expect (neat-connection-stalled conn) :to-be nil))

  (it "turns off once the evals are done"
    (setf (neat-connection-evals conn) '("5")
          (neat-connection-stalled conn) (float-time))
    (neat-client-test--push-bytes
     conn (neat-bencode-encode '(("id" . "5") ("status" "done"))))
    (expect (neat-tooling-stalled-p conn) :to-be nil))

  (it "turns off when a sync request gets answered mid-eval"
    (setf (neat-connection-evals conn) '("5")
          (neat-connection-stalled conn) (float-time))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'accept-process-output) #'ignore)
              ((symbol-function 'process-send-string)
               (neat-client-test--fake-server
                conn (lambda (_req) '((("info" . (("name" . "map")))
                                      ("status" "done")))))))
      (neat-lookup-sync conn "map"))
    (expect (neat-tooling-stalled-p conn) :to-be nil))

  (it "lets one request through as a probe every so often"
    (let ((neat-tooling-probe-interval 5))
      (setf (neat-connection-evals conn) '("5")
            (neat-connection-stalled conn) (- (float-time) 10))
      (expect (neat-tooling-stalled-p conn) :to-be nil)
      ;; The probe restarted the clock, so the next one has to wait.
      (expect (neat-tooling-stalled-p conn) :to-be-truthy)))

  (it "turns on when an async lookup times out behind an eval"
    (let (got (called nil))
      (setf (neat-connection-evals conn) '("5"))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string) #'ignore))
        (neat-lookup-async conn "map" nil 0.01
                           (lambda (info) (setq called t got info)))
        (sleep-for 0.1))
      (expect called :to-be-truthy)
      (expect got :to-be nil)
      (expect (hash-table-count (neat-connection-pending conn)) :to-equal 0)
      (expect (neat-tooling-stalled-p conn) :to-be-truthy)))

  (it "turns off when an async lookup is answered mid-eval"
    (setf (neat-connection-evals conn) '("5")
          (neat-connection-stalled conn) (float-time))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string)
               (neat-client-test--fake-server
                conn (lambda (_req) '((("info" . (("name" . "map")))
                                      ("status" "done")))))))
      (neat-lookup-async conn "map" nil 1 #'ignore))
    (expect (neat-tooling-stalled-p conn) :to-be nil))

  (it "turns off when the connection goes away"
    (let ((neat-connections (list conn))
          (neat-disconnect-functions nil))
      (setf (neat-connection-evals conn) '("5")
            (neat-connection-stalled conn) (float-time))
      (neat-disconnect conn)
      (expect (neat-connection-stalled conn) :to-be nil))))

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

  (it "leaves nothing behind when the message can't be encoded"
    (let ((conn (neat-connection--make))
          (neat-log-messages t)
          logged)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (&rest _) (error "Should not send")))
                ((symbol-function 'neat-client--log)
                 (lambda (&rest _) (setq logged t))))
        (expect (neat-send conn '((op . "eval") (ns . nil)) #'ignore)
                :to-throw 'neat-bencode-error)
        (expect (hash-table-count (neat-connection-pending conn))
                :to-equal 0)
        (expect logged :to-be nil))))

  (it "errors when the connection is not live"
    (let ((conn (neat-connection--make)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil)))
        (expect (neat-send conn '((op . "describe")))
                :to-throw 'error)))))

;;; neat-client-test.el ends here
