;;; test-helper.el --- Test helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared setup for the Buttercup suites.  Eldev arranges the load path
;; so neat-*.el files are findable; this file just centralises the
;; common requires.

;;; Code:

(require 'buttercup)
(require 'neat-bencode)
(require 'neat-client)

(provide 'test-helper)
;;; test-helper.el ends here
