;;; maak.scm --- fontsloth task runner -*- scheme -*-

;;; Commentary:

;; Tasks for fontsloth.  Run `maak` with no arguments to run the tests,
;; or `maak -l` to list all tasks.
;;
;; The Emacs executable can be overridden with the EMACS environment
;; variable.  Test fonts are checked in under test/fonts/, so no system
;; fonts or window system are required.

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak maak)
  #:export (default all compile test test-file coverage lint checkdoc
            package-lint clean))

(define emacs (or (getenv "EMACS") "emacs"))

(define source-files
  '("fontsloth-bbox.el"
    "fontsloth-cache.el"
    "fontsloth--common-types.el"
    "fontsloth-coords.el"
    "fontsloth.el"
    "fontsloth-geometry.el"
    "fontsloth-glyph.el"
    "fontsloth-layout.el"
    "fontsloth-layout-lb-tables.el"
    "fontsloth-layout-linebreak.el"
    "fontsloth-line.el"
    "fontsloth-log.el"
    "fontsloth-otf--mac-names.el"
    "fontsloth-otf-.el"
    "fontsloth-otf-cff-dict.el"
    "fontsloth-otf-cff.el"
    "fontsloth-otf-glyf.el"
    "fontsloth-otf-kern.el"
    "fontsloth-otf-typo.el"
    "fontsloth-otf.el"
    "fontsloth-point.el"
    "fontsloth-raster.el"
    "fontsloth-woff.el"))

(define (test-files)
  "Return the test files in this directory, sorted."
  (sort (filter (lambda (f) (string-suffix? "-test.el" f))
                (scandir "."))
        string<?))

(define (compile)
  "Byte-compile the fontsloth sources."
  (let ((files (string-join source-files " ")))
    ($ (list (format #f "~a --batch -L . -f batch-byte-compile ~a"
                     emacs files)))))

(define (test)
  "Run the complete ERT suite in batch mode."
  (let* ((files (test-files))
         (loads (string-join (map (lambda (f) (string-append "-l " f))
                                  files)
                             " ")))
    (when (null? files)
      (error "No test files found"))
    ($ (list (format #f "~a --batch -L . -l ert ~a --eval '(ert-run-tests-batch-and-exit)'"
                     emacs loads)))))

(define (test-file file)
  "Run one ERT test file, e.g. `maak test-file -- fontsloth-test.el'."
  ($ (list (format #f "~a --batch -L . -l ert -l ~a --eval '(ert-run-tests-batch-and-exit)'"
                   emacs file))))

(define (coverage)
  "Report per-file test coverage, one Emacs process per source file."
  (let ((tests (string-join (test-files) " ")))
    (for-each
     (lambda (source)
       ($ (list (format #f "~a --batch -L . -l scripts/coverage.el -- ~a ~a"
                        emacs source tests))))
     source-files)))

(define (checkdoc)
  "Run checkdoc over the fontsloth sources."
  (for-each
   (lambda (file)
     ($ (list (format #f "~a --batch -L . --eval '(progn (require (quote checkdoc)) (checkdoc-file \"~a\"))'"
                      emacs file))))
   source-files))

(define (package-lint)
  "Run package-lint over the fontsloth sources."
  (for-each
   (lambda (file)
     ($ (list (format #f "~a --batch -L . --eval '(progn (require (quote package-lint)) (package-lint-file \"~a\"))'"
                      emacs file))))
   source-files))

(define (lint)
  "Run checkdoc and package-lint."
  (checkdoc)
  (package-lint))

(define (clean)
  "Remove byte-compiled files."
  ($ (list "rm -f *.elc test/*.elc")))

(define (all)
  "Compile and test."
  (compile)
  (test))

(define (default)
  "Run the test suite."
  (test))
