;;; coverage.el --- Per-file test coverage for fontsloth -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;   emacs --batch -L . -l scripts/coverage.el -- SOURCE TEST...
;;
;; Instruments SOURCE with testcover, runs every TEST file, and reports
;; the percentage of instrumented forms that were evaluated.  Instrumenting
;; one file per Emacs process keeps a file that testcover cannot handle from
;; aborting the whole report; such files print SKIP instead.
;;
;; This is intentionally a reporting tool, not a gate.  Run `maak test`
;; first; the suite is what decides whether the build passes.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'testcover)

(defun coverage--report (source symbols)
  "Print the coverage of SYMBOLS instrumented from SOURCE."
  (let ((hits 0) (total 0))
    (dolist (symbol symbols)
      (let ((vector (get symbol 'edebug-coverage)))
        (when vector
          (cl-loop for value across vector do
                   (cl-incf total)
                   when (eq value 'edebug-ok-coverage) do (cl-incf hits)))))
    (princ (format "COVERAGE %s %d/%d (%.1f%%)\n"
                   source hits total
                   (if (> total 0) (/ (* 100.0 hits) total) 0.0)))))

(let* ((args (if (equal (car command-line-args-left) "--")
                 (cdr command-line-args-left)
               command-line-args-left))
       (source (car args))
       (tests (cdr args)))
  (cond
   ((null source)
    (princ "usage: emacs --batch -L . -l scripts/coverage.el -- SOURCE TEST...\n"))
   ((null tests)
    (princ (format "SKIP %s no tests\n" source)))
   (t
    (condition-case err
        (progn
          (testcover-start source)
          (let ((symbols (mapcar #'car edebug-form-data)))
            (dolist (test tests)
              (load-file test))
            (ert-run-tests-batch)
            (coverage--report source symbols)))
      (error
       (princ (format "SKIP %s testcover could not instrument it: %S\n"
                      source err)))))))

(provide 'coverage)
;;; coverage.el ends here
