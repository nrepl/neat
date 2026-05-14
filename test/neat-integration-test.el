;;; neat-integration-test.el --- Live nREPL integration tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; These specs talk to a real nREPL server (started as a subprocess via
;; the Clojure CLI) and exercise the full connect/describe/clone/eval
;; path.  They're gated behind the NEAT_INTEGRATION env var because
;; the JVM cold start adds tens of seconds to the suite and they
;; require network access on the first run (to fetch the nREPL
;; dependency).
;;
;; Run with:
;;
;;   NEAT_INTEGRATION=1 eldev test

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'neat-bencode)
(require 'neat-client)

(defvar neat-it--server-process nil)
(defvar neat-it--server-port nil)
(defvar neat-it--server-output "")

(defconst neat-it--port-regexp
  "nREPL server started on port \\([0-9]+\\)"
  "Regex matched against the nREPL server's startup banner.")

(defconst neat-it--server-deps
  "{:deps {nrepl/nrepl {:mvn/version \"1.3.0\"}}}")

(defun neat-it--clojure-available-p ()
  "Return non-nil when the Clojure CLI is on PATH."
  (and (executable-find "clojure") t))

(defun neat-it--server-filter (_proc chunk)
  "Watch the server's output CHUNK for the port banner."
  (setq neat-it--server-output (concat neat-it--server-output chunk))
  (when (and (not neat-it--server-port)
             (string-match neat-it--port-regexp neat-it--server-output))
    (setq neat-it--server-port
          (string-to-number (match-string 1 neat-it--server-output)))))

(defun neat-it--start-server ()
  "Boot a real nREPL server on a free port and return that port.
Errors out if it doesn't come up within a generous timeout window."
  (setq neat-it--server-port nil
        neat-it--server-output "")
  (let ((proc (make-process
               :name "neat-it-nrepl"
               :buffer nil
               :command (list "clojure"
                              "-Sdeps" neat-it--server-deps
                              "-M" "-m" "nrepl.cmdline"
                              "--port" "0")
               :filter #'neat-it--server-filter
               :noquery t
               :connection-type 'pipe)))
    (setq neat-it--server-process proc)
    (let ((deadline (+ (float-time) 120)))
      (while (and (not neat-it--server-port)
                  (process-live-p proc)
                  (< (float-time) deadline))
        (accept-process-output proc 0.5)))
    (unless neat-it--server-port
      (error "Neat: nREPL test server failed to start: %s"
             neat-it--server-output))
    neat-it--server-port))

(defun neat-it--stop-server ()
  "Terminate the test nREPL server, if any."
  (when (process-live-p neat-it--server-process)
    (kill-process neat-it--server-process))
  (setq neat-it--server-process nil
        neat-it--server-port nil
        neat-it--server-output ""))

(defun neat-it--wait-until (conn predicate &optional timeout)
  "Pump CONN's process output until PREDICATE returns non-nil or TIMEOUT elapses.
TIMEOUT defaults to 10 seconds."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output (neat-connection-process conn) 0.1))))

(when (and (neat-it--clojure-available-p)
           (getenv "NEAT_INTEGRATION"))

  (describe "integration against a real nREPL server"
    :var (conn responses)

    (before-all
      (neat-it--start-server))

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
        (expect (or (neat-bencode-get (neat-connection-capabilities conn)
                                      "versions")
                    (neat-bencode-get (neat-connection-capabilities conn)
                                      "ops"))
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
          (expect (neat-bencode-get value-resp "value") :to-equal "3"))))

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
        (let ((out-resp (cl-find-if (lambda (r) (neat-bencode-get r "out"))
                                    responses))
              (val-resp (cl-find-if (lambda (r) (neat-bencode-get r "value"))
                                    responses)))
          (expect out-resp :not :to-be nil)
          (expect (neat-bencode-get out-resp "out") :to-match "hi")
          (expect val-resp :not :to-be nil)
          (expect (neat-bencode-get val-resp "value") :to-equal ":ok"))))))

;;; neat-integration-test.el ends here
