;;; neat-integration-test.el --- End-to-end tests against nREPL  -*- lexical-binding: t; -*-

;;; Commentary:

;; End-to-end tests that drive the client and the REPL buffer against
;; a real nREPL server (the reference implementation, started through
;; the Clojure CLI).  They cover what the unit tests can only fake with
;; canned messages: REPL rendering, the need-input round trip,
;; interrupts, error statuses and late output, as they come off a real
;; wire.
;;
;; Whether a server speaks nREPL properly is a different question, and
;; proof (https://github.com/nrepl/proof) is the tool for that one.
;;
;; Gated behind NEAT_INTEGRATION because booting the JVM adds a few
;; seconds to the suite and the first run needs network access to
;; fetch nREPL.  Skipped when `clojure' isn't on PATH.
;;
;; Run with:
;;
;;   NEAT_INTEGRATION=1 eldev test

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'neat)

(defconst neat-it--command
  '("clojure" "-Sdeps" "{:deps {nrepl/nrepl {:mvn/version \"1.3.0\"}}}"
    "-M" "-m" "nrepl.cmdline" "--port" "0")
  "Command that starts the nREPL server the suite talks to.")

(defconst neat-it--port-regexp "nREPL server started on port \\([0-9]+\\)"
  "Regexp matching the server's banner; group 1 is the port.")

(defconst neat-it--startup-timeout 120
  "Seconds to wait for the server banner.")


;;;; Server lifecycle

(defvar neat-it--server-process nil)
(defvar neat-it--server-port nil)
(defvar neat-it--server-output "")

(defun neat-it--server-filter (_proc chunk)
  "Collect the server's output CHUNK and pick the port out of the banner."
  (setq neat-it--server-output (concat neat-it--server-output chunk))
  (when (and (not neat-it--server-port)
             (string-match neat-it--port-regexp neat-it--server-output))
    (setq neat-it--server-port
          (string-to-number (match-string 1 neat-it--server-output)))))

(defun neat-it--start-server ()
  "Boot the nREPL server and return its port."
  (setq neat-it--server-port nil
        neat-it--server-output ""
        neat-it--server-process
        (make-process :name "neat-it-nrepl"
                      :buffer nil
                      :command neat-it--command
                      :filter #'neat-it--server-filter
                      :noquery t
                      :connection-type 'pipe))
  (let ((deadline (+ (float-time) neat-it--startup-timeout)))
    (while (and (not neat-it--server-port)
                (process-live-p neat-it--server-process)
                (< (float-time) deadline))
      (accept-process-output neat-it--server-process 0.5)))
  (or neat-it--server-port
      (error "Neat: nREPL server failed to start: %s"
             neat-it--server-output)))

(defun neat-it--stop-server ()
  "Terminate the test nREPL server, if any."
  (when (process-live-p neat-it--server-process)
    (kill-process neat-it--server-process))
  (setq neat-it--server-process nil
        neat-it--server-port nil
        neat-it--server-output ""))


;;;; Helpers

(defun neat-it--wait-until (predicate &optional timeout)
  "Pump process output until PREDICATE returns non-nil.
Give up after TIMEOUT seconds (default 15).  Return PREDICATE's
last value."
  (let ((deadline (+ (float-time) (or timeout 15))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (funcall predicate)))

(defun neat-it--text ()
  "Return the current buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun neat-it--type (code)
  "Type CODE at the REPL prompt and submit it."
  (goto-char (point-max))
  (insert code)
  (neat-repl-return))

(defun neat-it--send (code)
  "Submit CODE at the REPL prompt and wait for the next prompt."
  (neat-it--type code)
  (neat-it--wait-until (lambda () neat-repl--prompt-start)))


;;;; Suite

(when (and (getenv "NEAT_INTEGRATION")
           (executable-find (car neat-it--command)))
  (describe "neat against nREPL"
    :var (conn repl)

    (before-all
      (neat-it--start-server))

    (after-all
      (neat-it--stop-server))

    (before-each
      (let ((neat-repl-history-file nil))
        (setq conn (neat "127.0.0.1" neat-it--server-port)))
      (setq repl (neat-repl-buffer-for conn))
      (neat-it--wait-until
       (lambda ()
         (and (neat-connection-session conn)
              (neat-connection-capabilities conn)
              (buffer-local-value 'neat-repl--prompt-start repl)))))

    (after-each
      (when (buffer-live-p repl)
        (kill-buffer repl))
      (when (neat-connection-live-p conn)
        (neat-disconnect conn))
      (setq neat-default-connection nil
            conn nil
            repl nil))

    (it "renders a value and a prompt in the REPL's namespace"
      (with-current-buffer repl
        (neat-it--send "(+ 1 2)")
        (expect (neat-it--text) :to-match "\n3\nuser> \\'")))

    (it "renders stdout ahead of the value"
      (with-current-buffer repl
        (neat-it--send "(do (println \"hi\") :ok)")
        (expect (neat-it--text) :to-match "\nhi\n:ok\nuser> \\'")))

    (it "follows in-ns in the prompt and in tooling ops"
      (with-current-buffer repl
        (neat-it--send "(in-ns 'neat.it)")
        (expect (neat-it--text) :to-match "neat\\.it> \\'")
        (expect (neat-connection-ns conn) :to-equal "neat.it")))

    (it "answers need-input from the minibuffer"
      (spy-on 'read-from-minibuffer :and-return-value "hello")
      (with-current-buffer repl
        (neat-it--send "(read-line)")
        (expect 'read-from-minibuffer :to-have-been-called)
        (expect (neat-it--text) :to-match "\n\"hello\"\nuser> \\'")))

    (it "sends end-of-file from the stdin prompt"
      (spy-on 'read-from-minibuffer
              :and-call-fake (lambda (&rest _)
                               (setq neat-repl--stdin-eof t)
                               ""))
      (with-current-buffer repl
        (neat-it--send "(read-line)")
        (expect (neat-it--text) :to-match "\nnil\nuser> \\'")))

    (it "interrupts a running eval"
      (with-current-buffer repl
        (neat-it--type "(Thread/sleep 60000)")
        ;; Give the eval a moment to actually start running.
        (neat-it--wait-until #'ignore 1)
        (expect (neat-eval-pending-p conn) :to-be-truthy)
        (neat-repl-interrupt)
        (expect (neat-it--wait-until (lambda () neat-repl--prompt-start))
                :to-be-truthy)
        (expect (neat-eval-pending-p conn) :to-be nil)))

    (it "reports a namespace that doesn't exist"
      (with-temp-buffer
        (setq neat-ns "neat.no-such-ns")
        (insert "(+ 1 2)")
        (neat-eval-buffer))
      (with-current-buffer repl
        (expect (neat-it--wait-until
                 (lambda ()
                   (string-match-p ";; namespace not found: neat\\.no-such-ns"
                                   (neat-it--text))))
                :to-be-truthy)))

    (it "reports an unknown session and clones a new one"
      (spy-on 'y-or-n-p :and-return-value t)
      (setf (neat-connection-session conn) "neat-no-such-session")
      (with-current-buffer repl
        (neat-it--send "(+ 1 2)")
        (expect (neat-it--text) :to-match ";; unknown session\n")
        (expect (neat-it--wait-until (lambda () (neat-connection-session conn)))
                :to-be-truthy)
        (neat-it--send "(+ 40 2)")
        (expect (neat-it--text) :to-match "\n42\nuser> \\'")))

    (it "shows output that arrives after the eval is done"
      (with-current-buffer repl
        (neat-it--send "(do (future (Thread/sleep 300) (println \"late\")) :ok)")
        (expect (neat-it--wait-until
                 (lambda () (string-match-p "^late$" (neat-it--text))))
                :to-be-truthy)
        ;; Above the prompt, not tacked on after it.
        (expect (neat-it--text) :to-match "\n:ok\nlate\nuser> \\'")))))

;;; neat-integration-test.el ends here
