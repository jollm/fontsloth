;;; fontsloth-otf-cff-test.el --- Tests for CFF1 charstrings -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.20.0
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, ttf, otf

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

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; fontsloth-otf-cff-test.el:
;; Tests for the CFF1 charstring interpreter, using synthetic charstrings
;; and the geometry outliner.
;;

;;; Code:

(require 'ert)
(require 'stream)
(require 'fontsloth-geometry)
(require 'fontsloth-otf)
(require 'fontsloth-otf-cff)

(defun fontsloth-otf-cff-test--bytes (&rest values)
  "Return a unibyte string containing VALUES."
  (apply #'unibyte-string values))

(defun fontsloth-otf-cff-test--int (n)
  "Encode N as a CFF int1 operand byte."
  (cl-assert (<= -107 n 107))
  (+ n 139))

(defun fontsloth-otf-cff-test--outline (&rest values)
  "Run the charstring VALUES through a geometry outliner and return it."
  (let ((geom (fontsloth-geometry-create 1.0 1000)))
    (fontsloth-otf-cff-parse-char-string
     (stream (apply #'fontsloth-otf-cff-test--bytes values))
     nil 0 geom)
    geom))

(defun fontsloth-otf-cff-test--lines (geom)
  "Return all lines in GEOM in insertion order."
  (append (reverse (fontsloth-geometry-v-lines geom))
          (reverse (fontsloth-geometry-m-lines geom))))

(defun fontsloth-otf-cff-test--line-coords (line)
  "Return the endpoint coordinates of LINE as a list."
  (let ((coords (fontsloth-line-coords line)))
    (list (fontsloth-coords-x0 coords) (fontsloth-coords-y0 coords)
          (fontsloth-coords-x1 coords) (fontsloth-coords-y1 coords))))

(ert-deftest fontsloth-otf-cff-test-arguments-stack ()
  "Check the bounded CFF argument stack."
  (let ((stack (fontsloth-otf-cff-arguments-stack-create :max-len 2)))
    (should (fontsloth-otf-cff-arguments-stack-empty-p stack))
    (fontsloth-otf-cff-arguments-stack-push stack 1.0)
    (fontsloth-otf-cff-arguments-stack-push stack 2.0)
    (should (= (fontsloth-otf-cff-arguments-stack-len stack) 2))
    (should (= (fontsloth-otf-cff-arguments-stack-pop stack) 2.0))
    (fontsloth-otf-cff-arguments-stack-reverse stack)
    (should (= (fontsloth-otf-cff-arguments-stack-pop stack) 1.0))
    (should (fontsloth-otf-cff-arguments-stack-empty-p stack))
    (fontsloth-otf-cff-arguments-stack-push stack 3.0)
    (fontsloth-otf-cff-arguments-stack-clear stack)
    (should (fontsloth-otf-cff-arguments-stack-empty-p stack))
    (fontsloth-otf-cff-arguments-stack-push stack 1.0)
    (fontsloth-otf-cff-arguments-stack-push stack 2.0)
    (should-error (fontsloth-otf-cff-arguments-stack-push stack 3.0))))

(ert-deftest fontsloth-otf-cff-test-subroutine-bias ()
  "Check subroutine bias and index conversion."
  (should (= (fontsloth-otf-cff--calc-subtroutine-bias 1000) 107))
  (should (= (fontsloth-otf-cff--calc-subtroutine-bias 2000) 1131))
  (should (= (fontsloth-otf-cff--calc-subtroutine-bias 40000) 32768))
  (should (= (fontsloth-otf-cff--conv-subr-index 0 107) 107))
  (should (= (fontsloth-otf-cff--conv-subr-index -5 1131) 1126)))

(ert-deftest fontsloth-otf-cff-test-rmoveto-rlineto ()
  "Check rmoveto followed by rlineto and endchar."
  (let* ((geom (fontsloth-otf-cff-test--outline
                (fontsloth-otf-cff-test--int 10)
                (fontsloth-otf-cff-test--int 20)
                21                    ; rmoveto
                (fontsloth-otf-cff-test--int 50)
                (fontsloth-otf-cff-test--int 10)
                5                     ; rlineto
                14))                  ; endchar
         (lines (fontsloth-otf-cff-test--lines geom)))
    (should (= (length lines) 2))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 0))
                   '(10.0 20.0 60.0 30.0)))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 1))
                   '(60.0 30.0 10.0 20.0)))))

(ert-deftest fontsloth-otf-cff-test-hmoveto-hlineto ()
  "Check hmoveto and the alternating hlineto operator."
  (let* ((geom (fontsloth-otf-cff-test--outline
                (fontsloth-otf-cff-test--int 10)
                22                    ; hmoveto
                (fontsloth-otf-cff-test--int 20)
                (fontsloth-otf-cff-test--int 30)
                6                     ; hlineto
                14))
         (lines (fontsloth-otf-cff-test--lines geom)))
    (should (= (length lines) 2))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 0))
                   '(30.0 0.0 30.0 30.0)))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 1))
                   '(30.0 30.0 10.0 0.0)))))

(ert-deftest fontsloth-otf-cff-test-vmoveto-vlineto ()
  "Check vmoveto and the alternating vlineto operator."
  (let* ((geom (fontsloth-otf-cff-test--outline
                (fontsloth-otf-cff-test--int 10)
                4                     ; vmoveto
                (fontsloth-otf-cff-test--int 20)
                (fontsloth-otf-cff-test--int 30)
                7                     ; vlineto
                14))
         (lines (fontsloth-otf-cff-test--lines geom)))
    ;; The second hlineto-style step is horizontal and is skipped by
    ;; the geometry builder, so only the vertical line and the closing
    ;; line remain.
    (should (= (length lines) 2))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 0))
                   '(0.0 10.0 0.0 30.0)))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 1))
                   '(30.0 30.0 0.0 10.0)))))

(ert-deftest fontsloth-otf-cff-test-rrcurveto-flat ()
  "Check a curve that is flat enough to become one line."
  (let* ((geom (fontsloth-otf-cff-test--outline
                (fontsloth-otf-cff-test--int 0)
                (fontsloth-otf-cff-test--int 0)
                21
                (fontsloth-otf-cff-test--int 10)
                (fontsloth-otf-cff-test--int 10)
                (fontsloth-otf-cff-test--int 10)
                (fontsloth-otf-cff-test--int 10)
                (fontsloth-otf-cff-test--int 10)
                (fontsloth-otf-cff-test--int 10)
                8                     ; rrcurveto
                14))
         (lines (fontsloth-otf-cff-test--lines geom)))
    (should (= (length lines) 2))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 0))
                   '(0.0 0.0 30.0 30.0)))))

(ert-deftest fontsloth-otf-cff-test-rrcurveto-subdivides ()
  "Check that a curve above max-area is subdivided."
  (let* ((geom (fontsloth-otf-cff-test--outline
                (fontsloth-otf-cff-test--int 0)
                (fontsloth-otf-cff-test--int 0)
                21
                (fontsloth-otf-cff-test--int 0)
                (fontsloth-otf-cff-test--int 100)
                (fontsloth-otf-cff-test--int 100)
                (fontsloth-otf-cff-test--int 0)
                (fontsloth-otf-cff-test--int 0)
                (fontsloth-otf-cff-test--int -100)
                8                     ; rrcurveto
                14))
         (lines (fontsloth-otf-cff-test--lines geom)))
    ;; Above max-area the curve is split at its midpoint.
    (should (= (length lines) 2))
    ;; Segment order follows the LIFO stack traversal.
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 0))
                   '(50.0 75.0 100.0 0.0)))
    (should (equal (fontsloth-otf-cff-test--line-coords (elt lines 1))
                   '(0.0 0.0 50.0 75.0)))))

(ert-deftest fontsloth-otf-cff-test-missing-move-to ()
  "Check that drawing before a move-to is rejected."
  (should-error
   (fontsloth-otf-cff-parse-char-string
    (stream (fontsloth-otf-cff-test--bytes
             (fontsloth-otf-cff-test--int 10)
             (fontsloth-otf-cff-test--int 10)
             5 14))
    nil 0 (fontsloth-geometry-create 1.0 1000))))

(ert-deftest fontsloth-otf-cff-test-missing-endchar ()
  "Check that a charstring without endchar is rejected."
  (should-error
   (fontsloth-otf-cff-parse-char-string
    (stream (fontsloth-otf-cff-test--bytes
             (fontsloth-otf-cff-test--int 10)
             (fontsloth-otf-cff-test--int 10)
             21))
    nil 0 (fontsloth-geometry-create 1.0 1000))))

(defun fontsloth-otf-cff-test--outline-with-metadata (metadata &rest values)
  "Run charstring VALUES with METADATA through a geometry outliner."
  (let ((geom (fontsloth-geometry-create 1.0 1000)))
    (fontsloth-otf-cff-parse-char-string
     (stream (apply #'fontsloth-otf-cff-test--bytes values))
     metadata 0 geom)
    geom))

(defun fontsloth-otf-cff-test--curve-case (op operands)
  "Check a curve operator OP with OPERANDS produces at least one line."
  (let* ((values (append (list (fontsloth-otf-cff-test--int 0)
                              (fontsloth-otf-cff-test--int 0)
                              21)
                         (mapcar #'fontsloth-otf-cff-test--int operands)
                         (list op 14)))
         (geom (apply #'fontsloth-otf-cff-test--outline values))
         (lines (fontsloth-otf-cff-test--lines geom)))
    (should (>= (length lines) 1))))

(ert-deftest fontsloth-otf-cff-test-alternating-curves ()
  "Check hh, vv, hv, and vh curve operators."
  (fontsloth-otf-cff-test--curve-case 27 '(10 10 10 10)) ; hhcurveto
  (fontsloth-otf-cff-test--curve-case 26 '(10 10 10 10)) ; vvcurveto
  (fontsloth-otf-cff-test--curve-case 31 '(10 10 10 10)) ; hvcurveto
  (fontsloth-otf-cff-test--curve-case 30 '(10 10 10 10))) ; vhcurveto

(ert-deftest fontsloth-otf-cff-test-flex-operators ()
  "Check flex, hflex, flex1, and hflex1 operators."
  (let ((prefix (list (fontsloth-otf-cff-test--int 0)
                      (fontsloth-otf-cff-test--int 0)
                      21))
        (suffix '(14)))
    (dolist (case (list (cons 7 '(12 34))  ; hflex
                        (cons 13 '(12 35)) ; flex
                        (cons 9 '(12 36))  ; hflex1
                        (cons 11 '(12 37)))) ; flex1
      (let* ((values (append prefix
                             (make-list (car case)
                                        (fontsloth-otf-cff-test--int 10))
                             (cdr case)
                             suffix))
             (geom (apply #'fontsloth-otf-cff-test--outline values))
             (lines (fontsloth-otf-cff-test--lines geom)))
        (should (> (length lines) 1))))))

(ert-deftest fontsloth-otf-cff-test-hint-masks ()
  "Check that hint and counter masks are skipped without truncating the stack."
  (dolist (op '(19 20))
    (let* ((values (append (list (fontsloth-otf-cff-test--int 0)
                                 (fontsloth-otf-cff-test--int 0)
                                 21
                                 (fontsloth-otf-cff-test--int 10)
                                 (fontsloth-otf-cff-test--int 20)
                                 op
                                 0 ; one mask byte: one stem
                                 (fontsloth-otf-cff-test--int 50)
                                 (fontsloth-otf-cff-test--int 10)
                                 5 ; rlineto
                                 14)))
           (geom (apply #'fontsloth-otf-cff-test--outline values)))
      (should (= (length (fontsloth-otf-cff-test--lines geom)) 2)))))

(ert-deftest fontsloth-otf-cff-test-local-subr ()
  "Check callsubr with a single local subroutine."
  (let* ((subr (list (fontsloth-otf-cff-test--int 50)
                     (fontsloth-otf-cff-test--int 10)
                     5 11))        ; rlineto, return
         (metadata `((local-subrs . ((data . ,(vector
                                               (apply #'fontsloth-otf-cff-test--bytes
                                                      subr)))))))
         (values (list (fontsloth-otf-cff-test--int 0)
                       (fontsloth-otf-cff-test--int 0)
                       21
                       (fontsloth-otf-cff-test--int -107)
                       10 ; callsubr
                       14))
         (geom (apply #'fontsloth-otf-cff-test--outline-with-metadata
                      metadata values)))
    (should (= (length (fontsloth-otf-cff-test--lines geom)) 2))))

(ert-deftest fontsloth-otf-cff-test-global-subr ()
  "Check callgsubr with a single global subroutine."
  (let* ((subr (list (fontsloth-otf-cff-test--int 50)
                     (fontsloth-otf-cff-test--int 10)
                     5 11))
         (metadata `((global-subrs . ((index . ((count . 1)))
                                      (data . ,(vector
                                                (apply #'fontsloth-otf-cff-test--bytes
                                                       subr)))))))
         (values (list (fontsloth-otf-cff-test--int 0)
                       (fontsloth-otf-cff-test--int 0)
                       21
                       (fontsloth-otf-cff-test--int -107)
                       29 ; callgsubr
                       14))
         (geom (apply #'fontsloth-otf-cff-test--outline-with-metadata
                      metadata values)))
    (should (= (length (fontsloth-otf-cff-test--lines geom)) 2))))

(provide 'fontsloth-otf-cff-test)

;;; fontsloth-otf-cff-test.el ends here
