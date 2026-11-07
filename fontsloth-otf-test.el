;;; fontsloth-otf-test.el --- Tests for OTF/TTF table parsing -*- lexical-binding: t -*-

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

;; fontsloth-otf-test.el:
;; Tests for OTF/TTF table specs that can run on synthetic bytes.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-otf)

(defun fontsloth-otf-test--u16 (bytes index value)
  "Store VALUE as a big-endian uint16 in BYTES at INDEX."
  (aset bytes index (logand (ash value -8) #xff))
  (aset bytes (1+ index) (logand value #xff)))

(ert-deftest fontsloth-otf-test-format0 ()
  "Check the cmap format 0 spec."
  (let ((table (bindat-unpack fontsloth-otf--format0-spec
                              [0 8 0 0 5 6])))
    (should (= (alist-get 'length table) 8))
    (should (= (alist-get 'language table) 0))
    (should (equal (alist-get 'data table) [5 6]))))

(ert-deftest fontsloth-otf-test-format6 ()
  "Check the cmap format 6 spec."
  (let ((table (bindat-unpack fontsloth-otf--format6-spec
                              [0 12 0 0 0 65 0 2 0 7 0 8])))
    (should (= (alist-get 'length table) 12))
    (should (= (alist-get 'first-code table) 65))
    (should (= (alist-get 'entry-count table) 2))
    (should (equal (alist-get 'data table) [7 8]))))

(ert-deftest fontsloth-otf-test-format12 ()
  "Check the cmap format 12 spec."
  (let* ((bytes [0 0 0 0 0 26 0 0 0 0 0 0 0 1
                 0 0 0 65 0 0 0 66 0 0 0 7])
         (table (bindat-unpack fontsloth-otf--format12-spec bytes)))
    (should (= (alist-get 'length table) 26))
    (should (= (alist-get 'count table) 1))
    (let* ((group (aref (alist-get 'groups table) 0)))
      (should (= (alist-get 'start-char-code group) 65))
      (should (= (alist-get 'end-char-code group) 66))
      (should (= (alist-get 'start-glyph-id group) 7)))))

(ert-deftest fontsloth-otf-test-calc-glyph-id-offset ()
  "Check format 4 glyph index arithmetic."
  (let ((bytes (make-vector 16 0)))
    (fontsloth-otf-test--u16 bytes 12 7)
    (fontsloth-otf-test--u16 bytes 14 9)
    ;; idRangeOffset 0 uses the char code directly.
    (should (= (fontsloth-otf--calc-glyph-id-offset bytes 65 0 65 10 0)
               65))
    ;; A non-zero offset adds 2*segment + offset + 2*(char-start).
    (should (= (fontsloth-otf--calc-glyph-id-offset bytes 65 0 65 10 2)
               7))
    (should (= (fontsloth-otf--calc-glyph-id-offset bytes 65 1 65 10 2)
               9))
    (should (= (fontsloth-otf--calc-glyph-id-offset bytes 66 0 65 10 2)
               9))))

(defun fontsloth-otf-test--format4-bytes ()
  "Return a format 4 cmap table and the synthetic font bytes for it."
  (list (vector 0 22 0 0 0 2 0 0 0 0 0 0
                0 65 0 0 0 65 255 196 0 0)
        ;; Same table with idRangeOffset pointing at a glyph id array.
        (vector 0 28 0 0 0 2 0 0 0 0 0 0
                0 66 0 0 0 65 0 0 0 4
                0 0 0 7 0 8)))

(defun fontsloth-otf-test--with-cmap (bytes fn)
  "Call FN with the current cmap globals bound to BYTES."
  (let* ((tables (make-hash-table :test 'equal))
         (directory (make-hash-table :test 'equal)))
    (puthash "cmap" nil directory)
    (puthash "table-directory" directory tables)
    (let ((fontsloth-otf--current-tables tables)
          (fontsloth-otf--current-font-bytes bytes))
      (funcall fn))))

(ert-deftest fontsloth-otf-test-format4-delta ()
  "Check format 4 parsing with an idDelta."
  (pcase-let ((`(,bytes . ,_) (fontsloth-otf-test--format4-bytes)))
    (fontsloth-otf-test--with-cmap
     bytes
     (lambda ()
       (let ((table (bindat-unpack fontsloth-otf--format4-spec bytes)))
         (should (= (alist-get 'seg-count table) 1))
         (should (equal (alist-get 'end-code table) [65]))
         (should (equal (alist-get 'start-code table) [65]))
         (should (equal (alist-get 'id-delta table) [-60]))
         (should (equal (alist-get 'glyph-index-map table) '((65 . 5)))))))))

(ert-deftest fontsloth-otf-test-format4-range-offset ()
  "Check format 4 parsing with an idRangeOffset."
  (pcase-let ((`(_ ,bytes) (fontsloth-otf-test--format4-bytes)))
    (fontsloth-otf-test--with-cmap
     bytes
     (lambda ()
       (let ((table (bindat-unpack fontsloth-otf--format4-spec bytes)))
         (should (equal (alist-get 'id-range-offset table) [4]))
         (should (equal (alist-get 'glyph-index-map table)
                        '((65 . 7) (66 . 8)))))))))

(defun fontsloth-otf-test--u32 (bytes index value)
  "Store VALUE as a big-endian uint32 in BYTES at INDEX."
  (fontsloth-otf-test--u16 bytes index (logand (ash value -16) #xffff))
  (fontsloth-otf-test--u16 bytes (+ index 2) (logand value #xffff)))

(defun fontsloth-otf-test--with-tables (tables fn)
  "Call FN with the current font tables bound to TABLES."
  (let ((fontsloth-otf--current-tables tables))
    (funcall fn)))

(ert-deftest fontsloth-otf-test-simple-glyf-flag ()
  "Check glyph flag bit decoding."
  (should (fontsloth-otf--simple-glyf-flag 'on-curve-point 1))
  (should-not (fontsloth-otf--simple-glyf-flag 'on-curve-point 2))
  (should (fontsloth-otf--simple-glyf-flag 'x-short-vector 2))
  (should (fontsloth-otf--simple-glyf-flag 'y-short-vector 4))
  (should (fontsloth-otf--simple-glyf-flag 'repeat 8))
  (should (fontsloth-otf--simple-glyf-flag 'x-is-same-or-pos-x-short-vec 16))
  (should (fontsloth-otf--simple-glyf-flag 'y-is-same-or-pos-y-short-vec 32))
  (should (fontsloth-otf--simple-glyf-flag 'overlap-simple 64))
  (should (fontsloth-otf--simple-glyf-flag 'reserved 128)))

(ert-deftest fontsloth-otf-test-prev-coord ()
  "Check previous-coordinate lookup used while decoding glyf coords."
  (should (= (fontsloth-otf--prev-coord [10 20] 0) 0))
  (should (= (fontsloth-otf--prev-coord [10 20] 1) 10))
  (should (= (fontsloth-otf--prev-coord [10 20] 2) 20)))

(ert-deftest fontsloth-otf-test-glyph-data-range ()
  "Check missing-glyph detection and glyph data ranges."
  (should-not (fontsloth-otf--has-missing-char? [0 0]))
  (should (fontsloth-otf--has-missing-char? [0 10]))
  (should (= (fontsloth-otf--glyph-data-range 0 [0 10 25]) 10))
  (should (= (fontsloth-otf--glyph-data-range 1 [0 10 25]) 15))
  (should-not (fontsloth-otf--glyph-data-range 1 [0 10 10])))

(ert-deftest fontsloth-otf-test-loca ()
  "Check short and long loca formats."
  (let ((tables (make-hash-table :test 'equal)))
    (puthash "maxp" '((num-glyphs . 2)) tables)
    (puthash "head" '((index-to-loc-format . 0)) tables)
    (fontsloth-otf-test--with-tables
     tables
     (lambda ()
       (should (equal (alist-get 'glyph-index-to-location
                                 (bindat-unpack fontsloth-otf--loca-spec
                                                [0 0 0 10 0 20]))
                      [0 20 40]))))
    (puthash "head" '((index-to-loc-format . 1)) tables)
    (let ((bytes (make-vector 12 0)))
      (fontsloth-otf-test--u32 bytes 0 0)
      (fontsloth-otf-test--u32 bytes 4 10)
      (fontsloth-otf-test--u32 bytes 8 20)
      (fontsloth-otf-test--with-tables
       tables
       (lambda ()
         (should (equal (alist-get 'glyph-index-to-location
                                   (bindat-unpack fontsloth-otf--loca-spec
                                                  bytes))
                        [0 10 20])))))))

(ert-deftest fontsloth-otf-test-hmtx ()
  "Check hmtx parsing with the numberOfHMetrics truncation rule."
  (let* ((tables (make-hash-table :test 'equal))
         (bytes (make-vector 10 0)))
    (puthash "maxp" '((num-glyphs . 3)) tables)
    (puthash "hhea" '((num-of-long-hor-metrics . 2)) tables)
    (fontsloth-otf-test--u16 bytes 0 600)
    (fontsloth-otf-test--u16 bytes 2 #xfffb)
    (fontsloth-otf-test--u16 bytes 4 650)
    (fontsloth-otf-test--u16 bytes 6 0)
    (fontsloth-otf-test--u16 bytes 8 10)
    (fontsloth-otf-test--with-tables
     tables
     (lambda ()
       (let ((hmtx (bindat-unpack fontsloth-otf--hmtx-spec bytes)))
         (should (= (length (alist-get 'hmetrics hmtx)) 2))
         (should (= (alist-get 'advance-width
                               (elt (alist-get 'hmetrics hmtx) 0))
                    600))
         (should (= (alist-get 'left-side-bearing
                               (elt (alist-get 'hmetrics hmtx) 0))
                    -5))
         (should (= (alist-get 'advance-width
                               (elt (alist-get 'hmetrics hmtx) 1))
                    650))
         (should (equal (alist-get 'left-side-bearing hmtx) [10])))))))

(ert-deftest fontsloth-otf-test-glyf-simple-and-missing ()
  "Check a simple glyf glyph and a missing glyph."
  (let* ((tables (make-hash-table :test 'equal))
         (bytes (make-vector 19 0)))
    (puthash "maxp" '((num-glyphs . 2)) tables)
    (puthash "loca" '((glyph-index-to-location . [0 19 19])) tables)
    (fontsloth-otf-test--u16 bytes 0 1)     ; numberOfContours
    (fontsloth-otf-test--u16 bytes 2 #xffff) ; xMin = -1
    (fontsloth-otf-test--u16 bytes 4 0)     ; yMin
    (fontsloth-otf-test--u16 bytes 6 10)    ; xMax
    (fontsloth-otf-test--u16 bytes 8 10)    ; yMax
    (fontsloth-otf-test--u16 bytes 10 0)    ; endPts[0]
    (fontsloth-otf-test--u16 bytes 12 0)    ; instructionLength
    (aset bytes 14 1)                       ; on-curve, long coords
    (fontsloth-otf-test--u16 bytes 15 0)    ; x
    (fontsloth-otf-test--u16 bytes 17 0)    ; y
    (fontsloth-otf-test--with-tables
     tables
     (lambda ()
       (let* ((glyf (bindat-unpack fontsloth-otf--glyf-spec bytes))
              (glyphs (alist-get 'glyphs glyf))
              (glyph (elt glyphs 0))
              (data (alist-get 'data glyph)))
         (should (= (length glyphs) 2))
         (should (= (alist-get 'number-of-contours glyph) 1))
         (should (= (alist-get 'x-min glyph) -1))
         (should (= (alist-get 'x-max glyph) 10))
         (should (= (alist-get 'num-points data) 1))
         (should (equal (alist-get 'flags data) [1]))
         (should (equal (alist-get 'x-coords data) [0]))
         (should (equal (alist-get 'y-coords data) [0]))
         (should (equal (elt glyphs 1) '((missing . missing-char)))))))))

(provide 'fontsloth-otf-test)
;;; fontsloth-otf-test.el ends here
