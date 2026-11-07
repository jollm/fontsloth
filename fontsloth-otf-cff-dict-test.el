;;; fontsloth-otf-cff-dict-test.el --- Test a CFF dictionary parser -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.20.0
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, glyph, glyf, ttf, otf

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

;; Part of fontsloth

;; fontsloth-otf-cff-dict-test (this file): Tests for a CFF dictionary parser

;;; Code:

(require 'ert)
(require 'fontsloth-otf-cff-dict)

(ert-deftest fontsloth-otf-cff-dict-test-parse-number ()
  (should (equal (fontsloth-otf-cff-dict-parse-number #xfa [#xfa #x7c] 0)
                 '(1000 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number #xfe [#xfe #x7c] 0)
                 '(-1000 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number #x1c [#x1c #x27 #x10] 0)
                 '(10000 2)))
  (should (equal (fontsloth-otf-cff-dict-parse-number #x1c [#x1c #xd8 #xf0] 0)
                 '(-10000 2)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  #x1d [#x1d #x00 #x01 #x86 #xa0] 0)
                 '(100000 4)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                       #x1d [#x1d #xff #xfe #x79 #x60] 0)
                 '(-100000 4))))

(ert-deftest fontsloth-otf-cff-dict-test-parse-number-encodings ()
  "Check every CFF dict number encoding."
  (should (equal (fontsloth-otf-cff-dict-parse-number 32 [] 0) '(-107 0)))
  (should (equal (fontsloth-otf-cff-dict-parse-number 246 [] 0) '(107 0)))
  (should (equal (fontsloth-otf-cff-dict-parse-number 247 [247 0] 0)
                 '(108 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number 250 [250 255] 0)
                 '(1131 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number 251 [251 0] 0)
                 '(-108 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number 254 [254 255] 0)
                 '(-1131 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  28 [28 #xff #xff] 0)
                 '(-1 2)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  29 [29 #xff #xff #xff #xfe] 0)
                 '(-2 4)))
  ;; 255 is a 16.16 fixed-point number.
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  255 [255 #x00 #x01 #x80 #x00] 0)
                 '(1.5 4)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  255 [255 #xff #xfe #x80 #x00] 0)
                 '(-1.5 4)))
  ;; Floats are not interpreted, but their size must be reported.
  (should (equal (fontsloth-otf-cff-dict-parse-number 30 [30 #x1f] 0)
                 '(0 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  30 [30 #x12 #x3f] 0)
                 '(0 2))))

(ert-deftest fontsloth-otf-cff-dict-test-read-numbers ()
  "Check unsigned and signed byte readers."
  (should (= (fontsloth-otf-cff-dict--read-num 2 [0 #x01 #x02] 1) 258))
  (should (= (fontsloth-otf-cff-dict--read-num 4 [0 #x01 #x02 #x03 #x04] 1)
             16909060))
  (should (= (fontsloth-otf-cff-dict--as-sint 8 #xff) -1))
  (should (= (fontsloth-otf-cff-dict--as-sint 8 #x7f) 127))
  (should (= (fontsloth-otf-cff-dict--as-sint 16 #x8000) -32768))
  (should (= (fontsloth-otf-cff-dict--read-sint 16 [#x80 #x00] 0)
             -32768)))

(ert-deftest fontsloth-otf-cff-dict-test-parse-skip ()
  "Check the number of bytes to skip after each operand/operator."
  (should (= (fontsloth-otf-cff-dict-parse-skip 28 [] 0) 2))
  (should (= (fontsloth-otf-cff-dict-parse-skip 29 [] 0) 4))
  (should (= (fontsloth-otf-cff-dict-parse-skip 255 [] 0) 4))
  (should (= (fontsloth-otf-cff-dict-parse-skip 247 [] 0) 1))
  (should (= (fontsloth-otf-cff-dict-parse-skip 254 [] 0) 1))
  (should (= (fontsloth-otf-cff-dict-parse-skip 246 [] 0) 0))
  (should (= (fontsloth-otf-cff-dict-parse-skip 5 [] 0) 0))
  (should (= (fontsloth-otf-cff-dict-parse-skip 30 [30 #x12 #x3f] 0) 1)))

(ert-deftest fontsloth-otf-cff-dict-test-one-byte-op-p ()
  "Check the one-byte operator classification."
  (dotimes (b 28)
    (should (fontsloth-otf-cff-dict-parser-one-byte-op-p b)))
  (dolist (b '(28 29 30 32 100 246 254))
    (should-not (fontsloth-otf-cff-dict-parser-one-byte-op-p b)))
  (should (fontsloth-otf-cff-dict-parser-one-byte-op-p 31))
  (should (fontsloth-otf-cff-dict-parser-one-byte-op-p 255)))

(ert-deftest fontsloth-otf-cff-dict-test-parser-next ()
  "Check advancing over operands and operators."
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [139 140 5])))
    (should (= (fontsloth-otf-cff-dict-parser-next parser) 5))
    (should (= (fontsloth-otf-cff-dict-parser-offset parser) 3))
    (should (= (fontsloth-otf-cff-dict-parser-operands-offset parser) 0)))
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [12 30])))
    (should (= (fontsloth-otf-cff-dict-parser-next parser) 1230))
    (should (= (fontsloth-otf-cff-dict-parser-offset parser) 2)))
  ;; Skip a float operand before the operator.
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [30 #x12 #x3f 5])))
    (should (= (fontsloth-otf-cff-dict-parser-next parser) 5))
    (should (= (fontsloth-otf-cff-dict-parser-offset parser) 4))))

(ert-deftest fontsloth-otf-cff-dict-test-parse-operands ()
  "Check collecting operands up to the next operator."
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [139 247 0 5]
                 :operands (make-vector 4 0))))
    (fontsloth-otf-cff-dict-parse-operands parser)
    (should (= (fontsloth-otf-cff-dict-parser-operands-len parser) 2))
    (should (= (elt (fontsloth-otf-cff-dict-parser-operands parser) 0) 0))
    (should (= (elt (fontsloth-otf-cff-dict-parser-operands parser) 1) 108))))

(ert-deftest fontsloth-otf-cff-dict-test-parse-offset-and-range ()
  "Check single-offset and length/start range parsing."
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [141 5] :operands (make-vector 4 0))))
    (should (= (fontsloth-otf-cff-dict-parse-offset parser) 2)))
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [141 139 5] :operands (make-vector 4 0))))
    (should (equal (fontsloth-otf-cff-dict-parse-range parser) '(0 2))))
  (let ((parser (fontsloth-otf-cff-dict-parser-create
                 :data [141 5] :operands (make-vector 4 0))))
    (should-not (fontsloth-otf-cff-dict-parse-range parser))))

(provide 'fontsloth-otf-cff-dict-test)
;;; fontsloth-otf-cff-dict-test.el ends here
