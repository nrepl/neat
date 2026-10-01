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

(defun neat-test--capture-eval-plist (body-fn)
  "Run BODY-FN with `neat-eval' stubbed; return the plist it was called with."
  (let (captured)
    (cl-letf (((symbol-function 'neat--require-connection)
               (lambda () 'stub-conn))
              ((symbol-function 'neat-eval)
               (lambda (_conn _code &rest plist) (setq captured plist))))
      (funcall body-fn))
    captured))

(describe "neat--eval-string source-location metadata"
  (it "computes 1-indexed line/column for the start of the form"
    (with-temp-buffer
      (insert "line1\nline2\n  (foo)\n")
      ;; Position of the open paren on line 3: after "line1\nline2\n  ".
      (let* ((pos (1+ (length "line1\nline2\n  ")))
             (plist (neat-test--capture-eval-plist
                     (lambda () (neat--eval-string "(foo)" pos)))))
        (expect (plist-get plist :line) :to-equal 3)
        (expect (plist-get plist :column) :to-equal 3))))

  (it "uses character columns, not display columns (tabs count as one)"
    (with-temp-buffer
      (insert "\t(foo)")
      (let ((plist (neat-test--capture-eval-plist
                    (lambda () (neat--eval-string "(foo)" 2)))))
        ;; "(" is at character offset 1 -> column 2.  A display-column
        ;; calculation would say 9 because of tab-width.
        (expect (plist-get plist :column) :to-equal 2))))

  (it "ignores narrowing when computing line numbers"
    (with-temp-buffer
      (insert "a\nb\nc\nd\n")
      (narrow-to-region 5 7)
      (let ((plist (neat-test--capture-eval-plist
                    (lambda () (neat--eval-string "c" 5)))))
        ;; Without ABSOLUTE=t, narrowing would yield line 1.
        (expect (plist-get plist :line) :to-equal 3))))

  (it "omits line/column when no position is given"
    (let ((plist (neat-test--capture-eval-plist
                  (lambda () (neat--eval-string "(+ 1 2)")))))
      (expect (plist-get plist :line) :to-be nil)
      (expect (plist-get plist :column) :to-be nil)))

  (it "passes the namespace from neat-buffer-ns-function"
    (with-temp-buffer
      (setq neat-ns "my.ns")
      (let ((plist (neat-test--capture-eval-plist
                    (lambda () (neat--eval-string "(+ 1 2)")))))
        (expect (plist-get plist :ns) :to-equal "my.ns"))))

  (it "honours a custom neat-buffer-ns-function override"
    (let ((neat-buffer-ns-function (lambda () "derived.ns")))
      (with-temp-buffer
        (let ((plist (neat-test--capture-eval-plist
                      (lambda () (neat--eval-string "(+ 1 2)")))))
          (expect (plist-get plist :ns) :to-equal "derived.ns"))))))

(describe "neat--eval-callback without a REPL buffer"
  (it "reports a connection that closed under the request"
    (let ((conn (neat-connection--make :host "nowhere" :port 1)))
      (spy-on 'message)
      (funcall (neat--eval-callback conn)
               '(("id" . "1") ("status" "done" "connection-closed")))
      (expect 'message :to-have-been-called-with
              "neat: connection closed")))

  (it "doesn't follow an err with a bare error"
    (let* ((conn (neat-connection--make :host "nowhere" :port 1))
           (callback (neat--eval-callback conn)))
      (spy-on 'message)
      (funcall callback '(("id" . "1") ("err" . "boom\n")))
      (funcall callback '(("id" . "1") ("status" "error" "done")))
      (expect 'message :to-have-been-called-with "neat: %s" "boom")
      (expect 'message :not :to-have-been-called-with "neat: %s" "error")))

  (it "shows an ex when nothing came on err"
    (let* ((conn (neat-connection--make :host "nowhere" :port 1))
           (callback (neat--eval-callback conn)))
      (spy-on 'message)
      (funcall callback '(("id" . "1") ("ex" . "boom") ("status" "eval-error")))
      (funcall callback '(("id" . "1") ("status" "error" "done")))
      (expect 'message :to-have-been-called-with "neat: %s" "boom")
      (expect 'message :not :to-have-been-called-with "neat: %s" "error")))

  (it "reports an error status in the echo area"
    (let ((conn (neat-connection--make :host "nowhere" :port 1)))
      (spy-on 'message)
      (funcall (neat--eval-callback conn)
               '(("id" . "1") ("ns" . "my.typo")
                 ("status" "done" "error" "namespace-not-found")))
      (expect 'message :to-have-been-called-with
              "neat: %s" "namespace not found: my.typo"))))

(describe "neat"
  :var (conn clone-callback)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 77)
          clone-callback nil)
    (spy-on 'neat-connect :and-return-value conn)
    (spy-on 'neat-describe)
    (spy-on 'neat-clone-session
            :and-call-fake (lambda (_c cb) (setq clone-callback cb)))
    (spy-on 'pop-to-buffer))

  (after-each
    (when-let* ((buf (neat-repl-buffer-for conn)))
      (kill-buffer buf)))

  (it "puts up the first prompt once the session is cloned"
    (let ((neat-repl-history-file nil)
          (neat-default-connection nil))
      (neat "h" 77)
      (funcall clone-callback '(("id" . "2") ("new-session" . "S")
                                ("status" "done")))
      (with-current-buffer (neat-repl-buffer-for conn)
        (expect (buffer-string) :to-equal "neat> "))))

  (it "doesn't put up a prompt when the connection closes first"
    (let ((neat-repl-history-file nil)
          (neat-default-connection nil))
      (neat "h" 77)
      (funcall clone-callback '(("id" . "2")
                                ("status" "done" "connection-closed")))
      (with-current-buffer (neat-repl-buffer-for conn)
        (expect (buffer-string) :to-equal "")))))

(describe "source-buffer evals and the REPL's ns"
  (it "moves the prompt only when the buffer named no ns"
    (let* ((neat-repl-history-file nil)
           (conn (neat-connection--make :host "h" :port 78))
           (repl (neat-repl-create-buffer conn))
           callback)
      (unwind-protect
          (cl-letf (((symbol-function 'neat--require-connection)
                     (lambda () conn))
                    ((symbol-function 'neat-eval)
                     (lambda (_c _code &rest plist)
                       (setq callback (plist-get plist :callback)))))
            (with-temp-buffer
              (setq neat-ns "my.ns")
              (neat--eval-string "(+ 1 2)"))
            (funcall callback '(("id" . "1") ("ns" . "my.ns") ("value" . "3")))
            (expect (buffer-local-value 'neat-repl--current-ns repl)
                    :to-be nil)
            (with-temp-buffer
              (neat--eval-string "(ns myapp.core)"))
            (funcall callback '(("id" . "2") ("ns" . "myapp.core")
                                ("value" . "nil")))
            (expect (buffer-local-value 'neat-repl--current-ns repl)
                    :to-equal "myapp.core"))
        (with-current-buffer repl (setq neat-current-connection nil))
        (kill-buffer repl)))))

(describe "neat--tooling-ns"
  (it "prefers the buffer's ns"
    (with-temp-buffer
      (setq neat-ns "my.ns")
      (let ((conn (neat-connection--make :ns "user")))
        (expect (neat--tooling-ns conn) :to-equal "my.ns"))))

  (it "falls back to the ns the REPL last reported"
    (with-temp-buffer
      (let ((conn (neat-connection--make :ns "user")))
        (expect (neat--tooling-ns conn) :to-equal "user"))))

  (it "is nil when neither knows"
    (with-temp-buffer
      (expect (neat--tooling-ns (neat-connection--make)) :to-be nil))))

(describe "tooling ops send an ns"
  :var (conn)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 1 :ns "user"))
    (spy-on 'neat-active-connection :and-return-value conn)
    (spy-on 'neat-connection-live-p :and-return-value t))

  (it "in completion-at-point"
    (spy-on 'neat-completions-sync :and-return-value nil)
    (with-temp-buffer
      (insert "ma")
      (neat-completion-at-point)
      (expect 'neat-completions-sync
              :to-have-been-called-with conn "ma" "user"
              neat-completion-timeout)))

  (it "in eldoc"
    (spy-on 'neat-lookup)
    (with-temp-buffer
      (insert "(map ")
      (neat-eldoc-function #'ignore)
      (expect (nth 2 (spy-calls-args-for 'neat-lookup 0)) :to-equal "user")))

  (it "in eldoc, trying again without it when it's turned down"
    (let (shown)
      (spy-on 'neat-lookup
              :and-call-fake
              (lambda (_c _sym ns cb)
                (funcall cb (if ns
                                '(("id" . "1")
                                  ("status" "done" "error" "namespace-not-found"))
                              '(("id" . "2") ("info" . (("doc" . "Maps.")))
                                ("status" "done"))))))
      (with-temp-buffer
        (insert "(map ")
        (neat-eldoc-function (lambda (str &rest _) (setq shown str))))
      (expect (mapcar (lambda (args) (nth 2 args))
                      (reverse (spy-calls-all-args 'neat-lookup)))
              :to-equal '("user" nil))
      (expect shown :to-equal "Maps.")))

  (it "in the doc lookup"
    (spy-on 'neat-lookup-sync :and-return-value '(("name" . "map")))
    (spy-on 'neat--render-doc)
    (with-temp-buffer
      (insert "map")
      (neat-show-doc-at-point)
      (expect 'neat-lookup-sync
              :to-have-been-called-with conn "map" "user" neat-lookup-timeout)))

  (it "in xref find-definitions"
    (spy-on 'neat-lookup-sync :and-return-value nil)
    (with-temp-buffer
      (xref-backend-definitions 'neat "map")
      (expect 'neat-lookup-sync
              :to-have-been-called-with conn "map" "user"
              neat-lookup-timeout))))

(describe "neat-eldoc-function"
  (it "tells eldoc when there's nothing to show"
    (let ((conn (neat-connection--make :host "h" :port 1))
          (calls nil))
      (spy-on 'neat-active-connection :and-return-value conn)
      (spy-on 'neat-connection-live-p :and-return-value t)
      (spy-on 'neat-lookup-async
              :and-call-fake (lambda (_c _sym _ns _timeout cb) (funcall cb nil)))
      (with-temp-buffer
        (insert "(map ")
        (neat-eldoc-function (lambda (&rest args) (push args calls))))
      (expect calls :to-equal '((nil))))))

(describe "features gated on describe"
  :var (conn)
  (before-each
    ;; A server that advertises only eval, like Basilisp does for the
    ;; tooling ops.
    (setq conn (neat-connection--make
                :host "h" :port 1
                :capabilities '(("ops" . (("eval") ("clone"))))))
    (spy-on 'neat-active-connection :and-return-value conn)
    (spy-on 'neat-connection-live-p :and-return-value t)
    (spy-on 'neat-completions-sync)
    (spy-on 'neat-lookup-sync)
    (spy-on 'neat-lookup)
    (spy-on 'neat-interrupt))

  (it "keeps completion-at-point quiet without completions"
    (with-temp-buffer
      (insert "ma")
      (expect (neat-completion-at-point) :to-be nil)
      (expect 'neat-completions-sync :not :to-have-been-called)))

  (it "keeps eldoc quiet without lookup"
    (with-temp-buffer
      (insert "(map ")
      (expect (neat-eldoc-function #'ignore) :to-be nil)
      (expect 'neat-lookup :not :to-have-been-called)))

  (it "steps the xref backend aside without lookup"
    (with-temp-buffer
      (expect (neat--xref-backend) :to-be nil)))

  (it "tells the user the doc lookup isn't supported"
    (with-temp-buffer
      (insert "map")
      (expect (neat-show-doc-at-point) :to-throw 'user-error)
      (expect 'neat-lookup-sync :not :to-have-been-called)))

  (it "tells the user interrupt isn't supported"
    (expect (neat-interrupt-eval) :to-throw 'user-error)
    (expect 'neat-interrupt :not :to-have-been-called)))

(describe "tooling while an eval is in flight"
  :var (conn)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 1 :evals '("5")))
    (spy-on 'neat-active-connection :and-return-value conn)
    (spy-on 'neat-connection-live-p :and-return-value t)
    (spy-on 'neat-completions-sync)
    (spy-on 'neat-lookup))

  (it "goes ahead while the server keeps answering"
    ;; nREPL answers tooling ops mid-eval, so nothing gets held back.
    (with-temp-buffer
      (insert "ma")
      (neat-completion-at-point)
      (expect 'neat-completions-sync :to-have-been-called)))

  (it "skips completion-at-point once requests stall"
    (setf (neat-connection-stalled conn) (float-time))
    (with-temp-buffer
      (insert "ma")
      (expect (neat-completion-at-point) :to-be nil)
      (expect 'neat-completions-sync :not :to-have-been-called)))

  (it "keeps eldoc to one lookup in flight"
    (let (pending-callback)
      (spy-on 'neat-lookup-async
              :and-call-fake (lambda (_c _sym _ns _timeout cb)
                               (setq pending-callback cb)))
      (with-temp-buffer
        (insert "(map ")
        (expect (neat-eldoc-function #'ignore) :to-be-truthy)
        (expect (neat-eldoc-function #'ignore) :to-be nil)
        (expect 'neat-lookup-async :to-have-been-called-times 1)
        ;; Once the first one is answered the next can go.
        (funcall pending-callback nil)
        (expect (neat-eldoc-function #'ignore) :to-be-truthy)
        (expect 'neat-lookup-async :to-have-been-called-times 2)
        (funcall pending-callback nil))))

  (it "doesn't leave eldoc stuck when the ns or the send fails"
    (with-temp-buffer
      (insert "(map ")
      (let ((neat-buffer-ns-function (lambda () (error "No ns here"))))
        (expect (neat-eldoc-function #'ignore) :to-throw 'error))
      (expect (gethash conn neat--eldoc-in-flight) :to-be nil)
      (spy-on 'neat-lookup-async
              :and-call-fake (lambda (&rest _) (error "Can't send")))
      (expect (neat-eldoc-function #'ignore) :to-throw 'error)
      (expect (gethash conn neat--eldoc-in-flight) :to-be nil)))

  (it "skips eldoc once requests stall"
    (setf (neat-connection-stalled conn) (float-time))
    (with-temp-buffer
      (insert "(map ")
      (expect (neat-eldoc-function #'ignore) :to-be nil)
      (expect 'neat-lookup :not :to-have-been-called))))

(describe "neat-interrupt-eval"
  :var (conn callback)
  (before-each
    (setq conn (neat-connection--make :host "h" :port 1))
    (spy-on 'neat-active-connection :and-return-value conn)
    (spy-on 'neat-interrupt
            :and-call-fake (lambda (_c _s _id cb) (setq callback cb)))
    (spy-on 'message))

  (it "says so when there was nothing to interrupt"
    (neat-interrupt-eval)
    (funcall callback '(("id" . "2") ("status" "session-idle" "done")))
    (expect 'message :to-have-been-called-with "Neat: nothing to interrupt"))

  (it "says so when there's no session to interrupt in"
    (neat-interrupt-eval)
    (funcall callback '(("id" . "2") ("status" "session-ephemeral" "done")))
    (expect 'message :to-have-been-called-with
            "Neat: can't interrupt an eval with no session"))

  (it "says so when the eval named isn't the one running"
    (neat-interrupt-eval)
    (funcall callback '(("id" . "2") ("status" "interrupt-id-mismatch" "done")))
    (expect 'message :to-have-been-called-with
            "Neat: that eval isn't the one running"))

  (it "says so when the interrupt fails some other way"
    (neat-interrupt-eval)
    (funcall callback '(("id" . "2") ("status" "error" "done")))
    (expect 'message :to-have-been-called-with "Neat: the interrupt failed"))

  (it "stays quiet when the interrupt goes through"
    (neat-interrupt-eval)
    (funcall callback '(("id" . "2") ("status" "done")))
    (expect 'message :not :to-have-been-called))

  (it "says so when the server turns out not to know the op"
    (neat-interrupt-eval)
    (funcall callback '(("id" . "2") ("status" "error" "unknown-op" "done")))
    (expect 'message :to-have-been-called-with
            "Neat: the server doesn't support interrupt")))

(describe "neat--lookup-file-path"
  (it "returns plain paths unchanged"
    (expect (neat--lookup-file-path "/tmp/foo.clj")
            :to-equal "/tmp/foo.clj"))

  (it "strips a file:// URL prefix"
    (expect (neat--lookup-file-path "file:///tmp/foo.clj")
            :to-equal "/tmp/foo.clj"))

  (it "strips a bare file: prefix"
    (expect (neat--lookup-file-path "file:/tmp/foo.clj")
            :to-equal "/tmp/foo.clj"))

  (it "returns nil for jar URLs"
    (expect (neat--lookup-file-path "jar:file:/p/clj.jar!/clojure/core.clj")
            :to-be nil))

  (it "returns nil for other scheme URLs"
    (expect (neat--lookup-file-path "http://example/foo.clj") :to-be nil))

  (it "returns nil for a nil input"
    (expect (neat--lookup-file-path nil) :to-be nil)))

(describe "neat--xref-location-from-info"
  (it "returns an xref location for a resolvable file"
    (let* ((tmp (make-temp-file "neat-xref-"))
           (info `(("file" . ,tmp) ("line" . 4) ("column" . 3))))
      (unwind-protect
          (let ((loc (neat--xref-location-from-info info)))
            (expect loc :not :to-be nil)
            (expect (xref-location-group loc) :to-equal tmp))
        (delete-file tmp))))

  (it "returns nil when the file doesn't exist on disk"
    (let ((info '(("file" . "/no/such/file.clj") ("line" . 1))))
      (expect (neat--xref-location-from-info info) :to-be nil)))

  (it "returns nil for jar URLs"
    (let ((info '(("file" . "jar:file:/p/clj.jar!/clojure/core.clj")
                  ("line" . 10))))
      (expect (neat--xref-location-from-info info) :to-be nil)))

  (it "treats a 0 column as missing rather than going negative"
    (let* ((tmp (make-temp-file "neat-xref-"))
           (info `(("file" . ,tmp) ("line" . 1) ("column" . 0))))
      (unwind-protect
          (let ((loc (neat--xref-location-from-info info)))
            (expect loc :not :to-be nil))
        (delete-file tmp)))))

(describe "neat--connections-entry"
  (it "marks the default connection with *"
    (let* ((proc (make-pipe-process :name "neat-test-entry-default"
                                    :noquery t))
           (conn (neat-connection--make
                  :host "h" :port 7888 :process proc
                  :session "abcdef0123456789"))
           (neat-default-connection conn))
      (unwind-protect
          (let* ((entry (neat--connections-entry conn))
                 (cells (cadr entry)))
            (expect (aref cells 0) :to-equal "*")
            (expect (aref cells 1) :to-equal "h:7888")
            (expect (aref cells 2) :to-equal "abcdef01...")
            (expect (aref cells 3) :to-equal "live"))
        (when (process-live-p proc) (delete-process proc)))))

  (it "shows -- and `closed' for a connection with no session and a dead proc"
    (let* ((proc (make-pipe-process :name "neat-test-entry-dead"
                                    :noquery t))
           (conn (neat-connection--make :host "h" :port 9 :process proc)))
      (delete-process proc)
      (let* ((entry (neat--connections-entry conn))
             (cells (cadr entry)))
        (expect (aref cells 0) :to-equal "")     ; not default
        (expect (aref cells 2) :to-equal "--")
        (expect (aref cells 3) :to-equal "closed")))))

(describe "neat--render-doc"
  (it "renders ns/name, arglist, doc, and source location"
    (neat--render-doc '(("name" . "map") ("ns" . "clojure.core")
                        ("arglists-str" . "([f] [f coll])")
                        ("doc" . "Returns a lazy sequence...")
                        ("file" . "/tmp/core.clj") ("line" . 42)))
    (with-current-buffer (get-buffer "*neat-doc: clojure.core/map*")
      (let ((text (buffer-string)))
        (expect text :to-match "clojure.core/map")
        (expect text :to-match "(\\[f\\] \\[f coll\\])")
        (expect text :to-match "Returns a lazy sequence")
        (expect text :to-match "Defined at /tmp/core.clj:42"))
      (kill-buffer)))

  (it "tolerates a missing namespace"
    (neat--render-doc '(("name" . "foo") ("doc" . "bar")))
    (with-current-buffer (get-buffer "*neat-doc: foo*")
      (expect (buffer-string) :to-match "foo")
      (kill-buffer)))

  (it "tolerates an empty docstring"
    (neat--render-doc '(("name" . "foo") ("ns" . "u")
                        ("arglists-str" . "([])") ("doc" . "")))
    (with-current-buffer (get-buffer "*neat-doc: u/foo*")
      (expect (buffer-string) :to-match "u/foo")
      (kill-buffer))))

(describe "neat--candidate-with-type"
  (it "returns the candidate string with type as a text property"
    (let ((s (neat--candidate-with-type
              '(("candidate" . "map") ("type" . "function")))))
      (expect s :to-equal "map")
      (expect (get-text-property 0 'neat-completion-type s)
              :to-equal "function")))

  (it "returns a bare string when the server reports no type"
    (let ((s (neat--candidate-with-type '(("candidate" . "foo")))))
      (expect s :to-equal "foo")
      (expect (get-text-property 0 'neat-completion-type s) :to-be nil)))

  (it "returns nil for an entry with no candidate"
    (expect (neat--candidate-with-type '(("type" . "function"))) :to-be nil)))

(describe "neat--completion-annotation"
  (it "returns the propertized type prefixed with a space"
    (let ((cand (propertize "map" 'neat-completion-type "function")))
      (expect (neat--completion-annotation cand) :to-equal " function")))

  (it "returns nil for a candidate carrying no type"
    (expect (neat--completion-annotation "plain") :to-be nil)))

(describe "neat--mode-line-info"
  (it "returns nil when no connection is active"
    (let ((neat-default-connection nil))
      (with-temp-buffer
        (expect (neat--mode-line-info) :to-be nil))))

  (it "returns [host:port] for a live connection"
    (let* ((proc (make-pipe-process :name "neat-test-ml-live" :noquery t))
           (conn (neat-connection--make :host "h" :port 42 :process proc))
           (neat-default-connection conn))
      (unwind-protect
          (expect (neat--mode-line-info) :to-equal "[h:42]")
        (when (process-live-p proc) (delete-process proc)))))

  (it "returns [closed] for a connection whose process is dead"
    (let* ((proc (make-pipe-process :name "neat-test-ml-dead" :noquery t))
           (conn (neat-connection--make :host "h" :port 42 :process proc))
           (neat-default-connection conn))
      (delete-process proc)
      (expect (neat--mode-line-info) :to-equal "[closed]"))))

(provide 'neat-test)
;;; neat-test.el ends here
