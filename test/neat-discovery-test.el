;;; neat-discovery-test.el --- Tests for .nrepl-port discovery  -*- lexical-binding: t; -*-

;;; Commentary:

;; Specs for `neat-discover-port' and `neat-discover-port-file'.  Each
;; spec builds a temp directory tree, drops a port-file (or doesn't),
;; and binds `default-directory' so the discovery walk has somewhere
;; predictable to start.

;;; Code:

(require 'buttercup)
(require 'neat)

(defun neat-discovery-test--with-tree (port-file-contents body)
  "Run BODY inside a temp tree with a nested subdir.
If PORT-FILE-CONTENTS is non-nil, drop a .nrepl-port at the root with
that content.  `default-directory' is bound to the nested subdir, so
BODY exercises the upward walk."
  (let* ((root (make-temp-file "neat-discovery-" t))
         (sub (expand-file-name "src/main" root)))
    (unwind-protect
        (progn
          (make-directory sub t)
          (when port-file-contents
            (with-temp-file (expand-file-name ".nrepl-port" root)
              (insert port-file-contents)))
          (let ((default-directory (file-name-as-directory sub))
                (buffer-file-name nil))
            (funcall body root sub)))
      (delete-directory root t))))

(describe "neat-discover-port-file"
  (it "finds the file walking up from a nested directory"
    (neat-discovery-test--with-tree
     "7888\n"
     (lambda (root _sub)
       (let ((found (neat-discover-port-file)))
         (expect found :not :to-be nil)
         (expect (file-equal-p found
                               (expand-file-name ".nrepl-port" root))
                 :to-be-truthy)))))

  (it "returns nil when there's no port file anywhere up the tree"
    ;; Walk up from /tmp/something so we don't accidentally hit a real
    ;; .nrepl-port the developer happens to have lying around.
    (let ((default-directory temporary-file-directory)
          (buffer-file-name nil)
          (neat-port-file-name (format ".neat-test-no-such-%d" (random))))
      (expect (neat-discover-port-file) :to-be nil)))

  (it "honours `neat-port-file-name'"
    (let* ((root (make-temp-file "neat-discovery-" t))
           (custom-name ".my-custom-port"))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name custom-name root)
              (insert "9000"))
            (let ((default-directory (file-name-as-directory root))
                  (buffer-file-name nil)
                  (neat-port-file-name custom-name))
              (expect (neat-discover-port-file) :not :to-be nil)))
        (delete-directory root t)))))

(describe "neat-discover-port"
  (it "parses a plain numeric port"
    (neat-discovery-test--with-tree
     "7888\n"
     (lambda (_root _sub)
       (expect (neat-discover-port) :to-equal 7888))))

  (it "trims surrounding whitespace before parsing"
    (neat-discovery-test--with-tree
     "  4321  \n"
     (lambda (_root _sub)
       (expect (neat-discover-port) :to-equal 4321))))

  (it "returns nil for an empty file"
    (neat-discovery-test--with-tree
     ""
     (lambda (_root _sub)
       (expect (neat-discover-port) :to-be nil))))

  (it "returns nil for non-numeric content"
    (neat-discovery-test--with-tree
     "not-a-port"
     (lambda (_root _sub)
       (expect (neat-discover-port) :to-be nil))))

  (it "returns nil when no port file exists"
    (let ((default-directory temporary-file-directory)
          (buffer-file-name nil)
          (neat-port-file-name (format ".neat-test-no-such-%d" (random))))
      (expect (neat-discover-port) :to-be nil))))

;;; neat-discovery-test.el ends here
