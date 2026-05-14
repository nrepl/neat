;;; neat-integration-test.el --- Live nREPL integration tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Live integration tests against real nREPL implementations.  Gated
;; behind NEAT_INTEGRATION because spinning up real servers adds a few
;; seconds to the suite and may need network access on the first run
;; (e.g. to fetch the Clojure nREPL dependency).
;;
;; Each entry in `neat-it--server-impls' below describes one
;; implementation.  At load time we walk the list, and for every impl
;; whose executable is on PATH we register a `describe' block that
;; runs the same set of assertions.  Implementations that aren't
;; installed locally are silently skipped, so contributors can run
;; whichever subset they have.
;;
;; Run with:
;;
;;   NEAT_INTEGRATION=1 eldev test

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'neat-bencode)
(require 'neat-client)


;;;; Server descriptors

(defconst neat-it--clojure-deps
  "{:deps {nrepl/nrepl {:mvn/version \"1.3.0\"}}}")

(defconst neat-it--server-impls
  `((:name "Clojure"
     :executable "clojure"
     :command-fn ,(lambda ()
                    (list "clojure" "-Sdeps" neat-it--clojure-deps
                          "-M" "-m" "nrepl.cmdline" "--port" "0"))
     :port-regexp "nREPL server started on port \\([0-9]+\\)"
     :startup-timeout 120)
    (:name "Babashka"
     :executable "bb"
     :command-fn ,(lambda () (list "bb" "nrepl-server" "localhost:0"))
     :port-regexp "Started nREPL server at [^:]+:\\([0-9]+\\)"
     :startup-timeout 30)
    (:name "Basilisp"
     :executable "basilisp"
     :command-fn ,(lambda () (list "basilisp" "nrepl-server"))
     ;; Basilisp prints the same banner shape as nrepl/nrepl, so we
     ;; can reuse the Clojure regex verbatim.
     :port-regexp "nREPL server started on port \\([0-9]+\\)"
     :startup-timeout 30))
  "Implementations the integration suite knows how to drive.
Each entry is a plist with :name, :executable, :command-fn (returns the
process command list), :port-regexp (matched against stdout to discover
the chosen port), and :startup-timeout (seconds to wait for the banner).")


;;;; Subprocess lifecycle

(defvar neat-it--server-process nil)
(defvar neat-it--server-port nil)
(defvar neat-it--server-output "")
(defvar neat-it--port-regexp nil)

(defun neat-it--server-filter (_proc chunk)
  "Watch the server's output CHUNK for the port banner."
  (setq neat-it--server-output (concat neat-it--server-output chunk))
  (when (and (not neat-it--server-port)
             (string-match neat-it--port-regexp neat-it--server-output))
    (setq neat-it--server-port
          (string-to-number (match-string 1 neat-it--server-output)))))

(defun neat-it--start-server (impl)
  "Boot the nREPL server described by IMPL, return its port.
Errors out if the banner doesn't appear within the impl's timeout."
  (setq neat-it--server-port nil
        neat-it--server-output ""
        neat-it--port-regexp (plist-get impl :port-regexp))
  (let* ((cmd (funcall (plist-get impl :command-fn)))
         (timeout (or (plist-get impl :startup-timeout) 60))
         (proc (make-process
                :name (format "neat-it-%s" (plist-get impl :name))
                :buffer nil
                :command cmd
                :filter #'neat-it--server-filter
                :noquery t
                :connection-type 'pipe)))
    (setq neat-it--server-process proc)
    (let ((deadline (+ (float-time) timeout)))
      (while (and (not neat-it--server-port)
                  (process-live-p proc)
                  (< (float-time) deadline))
        (accept-process-output proc 0.5)))
    (unless neat-it--server-port
      (error "Neat: %s nREPL server failed to start: %s"
             (plist-get impl :name) neat-it--server-output))
    neat-it--server-port))

(defun neat-it--stop-server ()
  "Terminate the test nREPL server, if any."
  (when (process-live-p neat-it--server-process)
    (kill-process neat-it--server-process))
  (setq neat-it--server-process nil
        neat-it--server-port nil
        neat-it--server-output ""
        neat-it--port-regexp nil))

(defun neat-it--wait-until (conn predicate &optional timeout)
  "Pump CONN's process output until PREDICATE returns non-nil or TIMEOUT elapses.
TIMEOUT defaults to 10 seconds."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output (neat-connection-process conn) 0.1))))


;;;; Suite registration

(when (getenv "NEAT_INTEGRATION")
  (dolist (impl neat-it--server-impls)
    (when (executable-find (plist-get impl :executable))
      ;; Fresh per-iteration binding so the lambdas inside the
      ;; describe block close over THIS impl, not the dolist's
      ;; shared slot.
      (let ((impl impl))
        (describe (format "integration against %s nREPL"
                          (plist-get impl :name))
          :var (conn responses)

          (before-all
            (neat-it--start-server impl))

          (after-all
            (neat-it--stop-server))

          (before-each
            (setq responses nil)
            (setq conn (neat-connect "127.0.0.1" neat-it--server-port)))

          (after-each
            (when conn
              (ignore-errors (neat-disconnect conn))
              (setq conn nil)))

          (it "describes the server's capabilities"
            (let (done)
              (neat-describe
               conn
               (lambda (r)
                 (push r responses)
                 (when (member "done" (neat-bencode-get r "status"))
                   (setq done t))))
              (neat-it--wait-until conn (lambda () done) 15)
              (expect done :to-be-truthy)
              (expect (or (neat-bencode-get
                           (neat-connection-capabilities conn) "versions")
                          (neat-bencode-get
                           (neat-connection-capabilities conn) "ops"))
                      :not :to-be nil)))

          (it "clones a session and evaluates an expression"
            (neat-clone-session conn)
            (neat-it--wait-until
             conn (lambda () (neat-connection-session conn)) 15)
            (expect (neat-connection-session conn) :not :to-be nil)

            (let ((id (neat-eval
                       conn "(+ 1 2)" nil
                       (lambda (r) (push r responses)))))
              (neat-it--wait-until
               conn (lambda ()
                      (not (gethash id (neat-connection-pending conn))))
               15)
              (let ((value-resp (cl-find-if (lambda (r)
                                              (neat-bencode-get r "value"))
                                            responses)))
                (expect value-resp :not :to-be nil)
                (expect (neat-bencode-get value-resp "value")
                        :to-equal "3"))))

          (it "captures stdout from the evaluated code"
            (neat-clone-session conn)
            (neat-it--wait-until
             conn (lambda () (neat-connection-session conn)) 15)

            (let ((id (neat-eval
                       conn "(do (println \"hi\") :ok)" nil
                       (lambda (r) (push r responses)))))
              (neat-it--wait-until
               conn (lambda ()
                      (not (gethash id (neat-connection-pending conn))))
               15)
              ;; Different implementations chunk `out' differently:
              ;; Clojure batches "hi\\n", Basilisp splits it into "hi"
              ;; and "\\n" messages.  Concatenate everything we got.
              (let ((all-out (mapconcat
                              (lambda (r) (or (neat-bencode-get r "out") ""))
                              (reverse responses)
                              ""))
                    (val-resp (cl-find-if
                               (lambda (r) (neat-bencode-get r "value"))
                               responses)))
                (expect all-out :to-match "hi")
                (expect val-resp :not :to-be nil)
                (expect (neat-bencode-get val-resp "value")
                        :to-equal ":ok")))))))))

;;; neat-integration-test.el ends here
