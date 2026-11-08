;;; fontsloth-otf-var.el --- OTF/TTF variation support -*- lexical-binding: t -*-

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

;; fontsloth-otf-var.el:
;; Parses the fvar table and the ItemVariationStore used by CFF2, GVAR,
;; HVAR, MVAR, COLR, BASE, GDEF, GPOS, and JSTF.
;;
;; The variation store query here takes already-normalized axis coordinates
;; (each coordinate in [-1, 1]); use `fontsloth-otf-var-normalize' to map
;; user-space values onto that range.
;;

;;; Code:

(require 'bindat)
(require 'cl-lib)

(require 'fontsloth-otf-)

(bindat-defmacro fontsloth-otf-var--fixed ()
  "Fixed signed 32-bit number with 16 fractional bits."
  (let ((bl (make-symbol "bitlen")))
    `(let ((,bl 32))
       (struct :pack-var v
               (v sint ,bl nil :pack-val v)
               :unpack-val (/ v 65536.0)))))

(bindat-defmacro fontsloth-otf-var--f2dot14 ()
  "Fixed signed 16-bit number with 14 fractional bits."
  (let ((bl (make-symbol "bitlen")))
    `(let ((,bl 16))
       (struct :pack-var v
               (v sint ,bl nil :pack-val v)
               :unpack-val (/ v 16384.0)))))

(defvar fontsloth-otf-var--fvar-axis-type-cache
  (make-hash-table :test #'eql)
  "Memoized fvar axis type objects keyed by axis size.")

(defun fontsloth-otf-var--fvar-axis-type (axis-size)
  "Return the memoized bindat type for an fvar axis of AXIS-SIZE bytes."
  (or (gethash axis-size fontsloth-otf-var--fvar-axis-type-cache)
      (puthash axis-size
               (bindat-type
                 (tag str 4)
                 (min-value fontsloth-otf-var--fixed)
                 (default-value fontsloth-otf-var--fixed)
                 (max-value fontsloth-otf-var--fixed)
                 (flags uint 16)
                 (name-id uint 16)
                 (_ fill (max 0 (- axis-size 20))))
               fontsloth-otf-var--fvar-axis-type-cache)))

(defvar fontsloth-otf-var--fvar-instance-type-cache
  (make-hash-table :test #'equal)
  "Memoized fvar instance type objects keyed by (size . axis-count).")

(defun fontsloth-otf-var--fvar-instance-type (instance-size axis-count)
  "Return the memoized bindat type for an fvar instance.
INSTANCE-SIZE is the instance record size and AXIS-COUNT is the number
of axis coordinates."
  (let ((key (cons instance-size axis-count)))
    (or (gethash key fontsloth-otf-var--fvar-instance-type-cache)
        (puthash key
                 (bindat-type
                   (subfamily-name-id uint 16)
                   (flags uint 16)
                   (coordinates vec axis-count
                                fontsloth-otf-var--fixed)
                   (postscript-name-id
                    unit
                    (if (> instance-size (+ 4 (* axis-count 4)))
                        (bindat--unpack-u16)
                      nil))
                   (_ fill (max 0 (- instance-size
                                     (+ 4 (* axis-count 4)
                                        (if (> instance-size
                                               (+ 4 (* axis-count 4)))
                                            2 0))))))
                 fontsloth-otf-var--fvar-instance-type-cache))))

(defvar fontsloth-otf-var--region-axis-type
  (bindat-type
    (start-coord fontsloth-otf-var--f2dot14)
    (peak-coord fontsloth-otf-var--f2dot14)
    (end-coord fontsloth-otf-var--f2dot14))
  "The bindat type for one axis in a variation region.")

(defvar fontsloth-otf-var--region-type-cache
  (make-hash-table :test #'eql)
  "Memoized variation-region type objects keyed by axis count.")

(defun fontsloth-otf-var--region-type (axis-count)
  "Return the memoized bindat type for a variation region.
AXIS-COUNT is the number of axes in the region."
  (or (gethash axis-count fontsloth-otf-var--region-type-cache)
      (puthash axis-count
               (bindat-type
                 (coords vec axis-count type
                         fontsloth-otf-var--region-axis-type))
               fontsloth-otf-var--region-type-cache)))

(defvar fontsloth-otf-var--delta-type-cache
  (make-hash-table :test #'equal)
  "Memoized ItemVariationData delta type objects.")

(defun fontsloth-otf-var--delta-type (has-long-words word-delta-count
                                                  byte-delta-count)
  "Return the memoized bindat type for one item's delta set.
HAS-LONG-WORDS selects 32/16-bit vs 16/8-bit deltas; WORD-DELTA-COUNT
and BYTE-DELTA-COUNT are the two delta counts."
  (let ((key (list has-long-words word-delta-count byte-delta-count)))
    (or (gethash key fontsloth-otf-var--delta-type-cache)
        (puthash key
                 (bindat-type
                   (word-deltas
                    vec word-delta-count type
                    (if has-long-words
                        (bindat-type (_ sint 32 nil))
                      (bindat-type (_ sint 16 nil))))
                   (byte-deltas
                    vec byte-delta-count type
                    (if has-long-words
                        (bindat-type (_ sint 16 nil))
                      (bindat-type (_ sint 8 nil)))))
                 fontsloth-otf-var--delta-type-cache))))

(defvar fontsloth-otf-var--fvar-spec
  (bindat-type
    (table-start unit bindat-idx)
    (major-version uint 16)
    (minor-version uint 16)
    (axes-array-offset uint 16)
    (reserved uint 16)
    (axis-count uint 16)
    (axis-size uint 16)
    (instance-count uint 16)
    (instance-size uint 16)
    (axes
     type
     (fontsloth-otf--with-offset
      (+ table-start axes-array-offset)
      nil
      (bindat-type
        (axes vec axis-count type
              (fontsloth-otf-var--fvar-axis-type axis-size)))))
    (instances
     type
     (fontsloth-otf--with-offset
      (+ table-start axes-array-offset (* axis-count axis-size))
      nil
      (bindat-type
        (instances vec instance-count
                   type (fontsloth-otf-var--fvar-instance-type
                         instance-size axis-count))))))
  "Bindat spec for the fvar table.
see URL https://learn.microsoft.com/en-us/typography/opentype/spec/fvar")

(defvar fontsloth-otf-var--item-variation-store-spec
  (bindat-type
    (table-start unit bindat-idx)
    (format uint 16)
    (region-list-offset uint 32)
    (count uint 16)
    (offsets vec count uint 32)
    (regions
     type
     (fontsloth-otf--with-offset
      (+ table-start region-list-offset)
      nil
      (bindat-type
        (axis-count uint 16)
        (region-count uint 16)
        (regions vec region-count
                 type (fontsloth-otf-var--region-type axis-count)))))
    (variation-data
     vec count type
     (fontsloth-otf--with-offset
      (+ table-start (aref offsets bindat--i))
      nil
      (bindat-type
        (item-count uint 16)
        (word-delta-flags uint 16)
        (region-index-count uint 16)
        (region-indices vec region-index-count uint 16)
        (has-long-words
         unit (/= 0 (logand word-delta-flags #x8000)))
        (word-delta-count
         unit (logand word-delta-flags #x7fff))
        (byte-delta-count
         unit (max 0 (- region-index-count word-delta-count)))
        (deltas
         vec item-count
         type (fontsloth-otf-var--delta-type
               has-long-words word-delta-count byte-delta-count))))))
  "Bindat spec for the OpenType ItemVariationStore.
see URL
`https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#item-variation-store'")

(defun fontsloth-otf-var-normalize (axis value)
  "Normalize VALUE against fvar AXIS.
AXIS is an fvar axis alist with min-value, default-value, and max-value."
  (let ((min (alist-get 'min-value axis))
        (default (alist-get 'default-value axis))
        (max (alist-get 'max-value axis)))
    (cond ((< value default)
           (if (= min default) 0.0
             (/ (- value default) (- default min))))
          ((> value default)
           (if (= max default) 0.0
             (/ (- value default) (- max default))))
          (t 0.0))))

(defun fontsloth-otf-var--region-scalar (region coords)
  "Return the scalar for REGION at normalized COORDS."
  (let ((coords-vector (alist-get 'coords region))
        (scalar 1.0))
    (cl-loop for axis across coords-vector
             for i from 0
             for value = (if (and coords (< i (length coords)))
                             (aref coords i)
                           0.0)
             for start = (alist-get 'start-coord axis)
             for peak = (alist-get 'peak-coord axis)
             for end = (alist-get 'end-coord axis)
             do
             (setq scalar
                   (* scalar
                      (cond
                       ((= peak 0.0) 1.0)
                       ((or (< value start) (> value end)) 0.0)
                       ((= value peak) 1.0)
                       ((< value peak)
                        (if (= start peak) 0.0
                          (/ (- value start) (- peak start))))
                       (t (if (= end peak) 0.0
                            (/ (- end value) (- end peak)))))))
             until (= scalar 0.0))
    scalar))

(defun fontsloth-otf-var-region-scalars (store outer-index coords)
  "Return the scalar for each region in STORE's OUTER-INDEX data.
COORDS is a vector of normalized axis coordinates, or nil for the
default instance."
  (let* ((data (aref (alist-get 'variation-data store) outer-index))
         (indices (alist-get 'region-indices data))
         (regions (alist-get 'regions (alist-get 'regions store))))
    (mapcar (lambda (region-index)
              (fontsloth-otf-var--region-scalar
               (aref regions region-index) coords))
            (append indices nil))))

(defun fontsloth-otf-var-item-delta (store outer-index inner-index coords)
  "Return the variation delta from STORE.
OUTER-INDEX selects an ItemVariationData table, INNER-INDEX selects an
item, and COORDS is a vector of normalized axis coordinates."
  (let* ((data (aref (alist-get 'variation-data store) outer-index))
         (region-indices (alist-get 'region-indices data))
         (deltas (aref (alist-get 'deltas data) inner-index))
         (word-deltas (alist-get 'word-deltas deltas))
         (byte-deltas (alist-get 'byte-deltas deltas))
         (word-delta-count (alist-get 'word-delta-count data))
         (result 0.0))
    (cl-loop for region-index across region-indices
             for i from 0
             for delta = (if (< i word-delta-count)
                             (aref word-deltas i)
                           (aref byte-deltas (- i word-delta-count)))
             do
             (cl-incf result
                      (* delta
                         (fontsloth-otf-var--region-scalar
                          (aref (alist-get 'regions (alist-get 'regions store))
                                region-index)
                          coords))))
    result))

(provide 'fontsloth-otf-var)
;;; fontsloth-otf-var.el ends here
