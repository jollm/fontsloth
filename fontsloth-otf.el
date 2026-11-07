;;; fontsloth-otf.el --- Elisp otf/ttf bindat parser -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.16.0
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, bindat, ttf, otf, parsing

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

;; Part of fontsloth: the slowest font renderer in the world written in pure
;; elisp.  inspired by fontdue

;; fontsloth-otf (this file): uses bindat to parse otf/ttf files

;; To use this module by itself, load and enable it as follows:
;;   (use-package fontsloth-otf)
;;

;;; Code:

(defvar bindat--v)
(require 'bindat)
(require 'cl-lib)

(require 'fontsloth-log)
(require 'fontsloth-otf--mac-names)
(require 'fontsloth-otf-)
(require 'fontsloth-otf-cff)
(require 'fontsloth-otf-glyf)
(require 'fontsloth-otf-kern)
(require 'fontsloth-otf-typo)
(require 'fontsloth-woff)

(defvar fontsloth-otf--header-spec
  (bindat-type
    (sfnt-version str 4)
    (num-tables uint 16)
    (search-range uint 16)
    (entry-selector uint 16)
    (range-shift uint 16))
  "Bindat spec for the OTF/TTF table directory header.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tabledirectory")

(defvar fontsloth-otf--maybe-woff-header-spec
  (bindat-type
    (--sig str 4)
    (_ unit (progn (setq bindat-idx 0) nil))
    (_ type
       (if (equal "wOFF" --sig)
           fontsloth-woff--header-spec
         fontsloth-otf--header-spec))))

(defvar fontsloth-otf--table-props-spec
  (bindat-type
    (tag str 4)
    (checksum uint 32)
    (offset uint 32)
    (length uint 32))
  "Bindat spec for a single entry in the OTF/TTF table directory.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tabledirectory")

(defvar fontsloth-otf--tables-spec
  (bindat-type
    (header type fontsloth-otf--maybe-woff-header-spec)
    (table-props repeat (alist-get 'num-tables header)
                 type (if (equal "wOFF" (alist-get 'signature header))
                          fontsloth-woff--table-props-spec
                        fontsloth-otf--table-props-spec)))
  "Bindat spec for the OTF/TTF table directory, including the header.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tabledirectory")

(bindat-defmacro ttf-fixed ()
  "Fixed signed 32 bit integer with 16 fractional bits."
  (let ((bl (make-symbol "bitlen")))
    `(let ((,bl 32))
       (struct :pack-var v
               (v sint ,bl nil :pack-val v ; TODO pack correctly
                  )
               :unpack-val
               (+ (/ v (ash 1 16))
                  (/ (* 1.0 (logand v #xffff)) #x10000))))))

(bindat-defmacro v16.16 ()
  "Version16Dot16 OTF/TTF version number format."
  (let ((bl (make-symbol "bitlen")))
    `(let ((,bl 32))
       (struct :pack-var v
               (v sint ,bl nil :pack-val v ; TODO pack correctly
                  )
               :unpack-val
               (+ (/ v (ash 1 16))
                  (let ((frac (logand v #xffff)))
                    (if (= 0 frac) frac
                      (/ frac (expt 10.0 (1+ (truncate (log frac 10))))))))))))

(bindat-defmacro f-2.14 ()
  "Fixed signed 16 bit integer with 14 fractional bits."
  (let ((bl (make-symbol "bitlen")))
    `(let ((,bl 16))
       (struct :pack-var v
               (v sint ,bl nil :pack-val v ; TODO pack correctly
                  )
               :unpack-val
               (+ (/ v (ash 1 14))
                  (/ (* 1.0 (logand v #x3fff)) #x4000))))))

(defvar fontsloth-otf--head-spec
  (bindat-type
    (major-version uint 16)
    (minor-version uint 16)
    (font-revision ttf-fixed)
    (checksum-adjustment uint 32)
    (magic-number uint 32)
    (flags uint 16)
    (units-per-em uint 16)
    (created uint 64)
    (modified uint 64)
    (x-min sint 16 nil)
    (y-min sint 16 nil)
    (x-max sint 16 nil)
    (y-max sint 16 nil)
    (mac-style uint 16)
    (lowest-rec-ppem uint 16)
    (font-direction-hint uint 16)
    (index-to-loc-format sint 16 nil)
    (glyph-data-format uint 16))
  "Bindat spec for the OTF/TTF head table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/head")

(defvar fontsloth-otf--maxp-spec
  (bindat-type
    (version v16.16)
    (num-glyphs uint 16)
    (max-points uint 16)
    (max-contours uint 16)
    (max-composite-points uint 16)
    (max-composite-contours uint 16)
    (max-zones uint 16)
    (max-twilight-points uint 16)
    (max-storage uint 16)
    (max-function-defs uint 16)
    (max-instruction-defs uint 16)
    (max-stack-elements uint 16)
    (max-size-of-instructions uint 16)
    (max-component-elements uint 16)
    (max-component-depth uint 16))
  "Bindat spec for the OTF/TTF maxp table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/maxp")

(defvar fontsloth-otf--hhea-spec
  (bindat-type
    (version ttf-fixed)
    (ascent sint 16 nil)
    (descent sint 16 nil)
    (line-gap sint 16 nil)
    (advance-width-max uint 16)
    (min-left-side-bearing sint 16 nil)
    (min-right-side-bearing sint 16 nil)
    (x-max-extent sint 16 nil)
    (caret-slope-rise sint 16 nil)
    (caret-slope-run sint 16 nil)
    (caret-offset sint 16 nil)
    (reserved-0 uint 16) (reserved-1 uint 16)
    (reserved-2 uint 16) (reserved-3 uint 16)
    (metric-data-format sint 16 nil)
    (num-of-long-hor-metrics uint 16))
  "Bindat spec for the OTF/TTF hhea table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/hhea")

(defvar fontsloth-otf--current-font-bytes nil)
(defvar fontsloth-otf--current-tables nil)
(defvar fontsloth-otf--current-font nil
  "The explicitly current `fontsloth-otf-font', if any.
Bind this with `fontsloth-otf-with-font' rather than setting it directly.")

(cl-defstruct
    (fontsloth-otf-font
     (:constructor fontsloth-otf-font-create)
     (:copier nil))
  "A loaded OTF/TTF font's parsed tables and source bytes."
  (tables nil :type hash-table
          :documentation "tag -> parsed table data")
  (bytes nil :type string
         :documentation "raw font file bytes, kept for WOFF and hashing"))

(defsubst fontsloth-otf--font-tables (font)
  "Return FONT's table map, or the dynamically current map when FONT is nil."
  (if font (fontsloth-otf-font-tables font) fontsloth-otf--current-tables))

(defsubst fontsloth-otf--font-bytes (font)
  "Return FONT's raw bytes, or the dynamically current bytes when FONT is nil."
  (if font (fontsloth-otf-font-bytes font) fontsloth-otf--current-font-bytes))

(defmacro fontsloth-otf-with-font (font &rest body)
  "Evaluate BODY with FONT as the current font.
When FONT is nil, keep the existing dynamic font context."
  (declare (indent 1) (debug (form body)))
  (let ((f (make-symbol "font")))
    `(let* ((,f ,font)
            (fontsloth-otf--current-font (or ,f fontsloth-otf--current-font))
            (fontsloth-otf--current-tables
             (if ,f (fontsloth-otf-font-tables ,f)
               fontsloth-otf--current-tables))
            (fontsloth-otf--current-font-bytes
             (if ,f (fontsloth-otf-font-bytes ,f)
               fontsloth-otf--current-font-bytes)))
       ,@body)))

(defun fontsloth-otf--get-table-value (field tag &optional font)
  "Get FIELD from TAG in FONT's tables, or the current tables when FONT is nil.
FIELD the table field
TAG the table tag
FONT an optional `fontsloth-otf-font'"
  (alist-get field (gethash tag (fontsloth-otf--font-tables font))))

(defvar fontsloth-otf--hmtx-spec
  (cl-flet ((num-hor-metrics ()
              (fontsloth-otf--get-table-value 'num-of-long-hor-metrics "hhea"))
            (num-glyphs ()
              (fontsloth-otf--get-table-value 'num-glyphs "maxp")))
    (bindat-type
      (hmetrics vec (num-hor-metrics)
        type (bindat-type
               (advance-width uint 16)
               (left-side-bearing sint 16 nil)))
      (left-side-bearing vec (- (num-glyphs) (num-hor-metrics))
        sint 16 nil)))
  "Bindat spec for the OTF/TTF hmtx table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/hmtx")

(defvar fontsloth-otf--loca-spec
  (bindat-type
    (glyph-index-to-location
     vec (1+ (fontsloth-otf--get-table-value 'num-glyphs "maxp"))
     type (if (eq 0
                  (fontsloth-otf--get-table-value 'index-to-loc-format "head"))
              (bindat-type
                :pack-var v
                (loc uint 16 :pack-val (ash v -1))
                :unpack-val (ash loc 1))
            (bindat-type uint 32))))
  "Bindat spec for the TrueType loca table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/loca")

(defun fontsloth-otf--has-missing-char? (glyph-locations)
  "Test whether the first glyph in glyf is the missing character.
GLYPH-LOCATIONS sequence of glyph locations from the loca table"
  (not (eq (elt glyph-locations 0)
           (elt glyph-locations 1))))

(defun fontsloth-otf--glyph-data-range (index glyph-locations)
  "Calculate data range of glyph `index' in glyf given glyph-locations.
INDEX the glyph index
GLYPH-LOCATIONS sequence of glyph locations from the loca table"
  (let ((range (- (elt glyph-locations (1+ index))
                  (elt glyph-locations index))))
    (unless (>= 0 range) range)))

(defun fontsloth-otf--simple-glyf-flag (sym byte)
  "Return flag in BYTE matching SYM.

SYM ::= one of
  on-curve-point
  x-short-vector
  y-short-vector
  repeat
  x-is-same-or-pos-x-short-vec
  y-is-same-or-pos-y-short-vec
  overlap-simple
  reserved"
  (cl-case sym
    (on-curve-point (eq 1 (logand 1 byte)))
    (x-short-vector (eq 2 (logand 2 byte)))
    (y-short-vector (eq 4 (logand 4 byte)))
    (repeat (eq 8 (logand 8 byte)))
    (x-is-same-or-pos-x-short-vec (eq 16 (logand 16 byte)))
    (y-is-same-or-pos-y-short-vec (eq 32 (logand 32 byte)))
    (overlap-simple (eq 64 (logand 64 byte)))
    (reserved (eq 128 (logand 128 byte)))))

;; TODO compute-x and compute-y could be done with a macro or wrapper fn

(defun fontsloth-otf--prev-coord (prev idx)
  "Return the previous coord given seq PREV and IDX."
  (if (= 0 idx) 0
    (elt prev (1- idx))))

;; TODO: determine how to make bindat vec with dynamically determined
;; types efficient
(defun fontsloth-otf--make-simple-glyf-flags-vec (num-points)
  "Make a simple glyf flags vector with NUM-POINTS flags."
  (cl-loop for i from 0 below num-points
           with flag-repeat-counter = 0
           with flag-to-repeat = nil
           with res = (make-vector num-points 0)
           do (if (>= 0 flag-repeat-counter)
                  (let* ((flag (bindat--unpack-u8))
                         (repeat (when (fontsloth-otf--simple-glyf-flag 'repeat flag)
                                   (bindat--unpack-u8))))
                    (when repeat
                      (setf flag-repeat-counter repeat
                            flag-to-repeat flag))
                    (aset res i flag))
                (aset res i flag-to-repeat)
                (cl-decf flag-repeat-counter))
           finally return res))

;; TODO: determine how to make bindat vec with dynamically determined
;; types efficient
(defun fontsloth-otf--make-simple-glyf-coords-vec
    (flags num-points short-vec is-same)
  "Given FLAGS, make a simple glyf coords vector with NUM-POINTS coords.

SHORT-VEC ::= the flag to check for short-vector
IS-SAME ::= the flag to check for is-same-or-pos-short-vec"
  (cl-loop for i from 0 below num-points
           for flag = (elt flags i)
           for short-vector = (fontsloth-otf--simple-glyf-flag short-vec flag)
           for is-same-or-pos-short-vec = (fontsloth-otf--simple-glyf-flag
                                           is-same flag)
           for dx = (if short-vector
                        (if is-same-or-pos-short-vec
                            (bindat--unpack-u8)
                          (- (bindat--unpack-u8)))
                      (if is-same-or-pos-short-vec
                          0
                        (let ((n (bindat--unpack-u16))
                              (max (ash 1 (1- 16))))
                          (if (>= n max) (- n (+ max max)) n))))
           with res = (make-vector num-points 0)
           do (aset res i (+ dx (fontsloth-otf--prev-coord res i)))
           finally return res))

(defun fontsloth-otf--make-simple-glyf-data-spec (num-contours range)
  "Given number of contours make a bindat spec to parse simple glyph data.
NUM-CONTOURS number of contours for the glyph, positive for simple data
RANGE length in bytes from loca for data, excluding header size"
  (let ((fill-to))
    (bindat-type
      (_ unit (progn (setf fill-to (+ bindat-idx range)) nil))
      (end-pts vec num-contours uint 16)
      (instruction-length uint 16)
      (instructions vec instruction-length uint 8)
      (num-points unit (1+ (elt end-pts (1- num-contours))))
      (flags unit (fontsloth-otf--make-simple-glyf-flags-vec num-points))
      (x-coords unit (fontsloth-otf--make-simple-glyf-coords-vec
                      flags num-points
                      'x-short-vector 'x-is-same-or-pos-x-short-vec))
      (y-coords unit (fontsloth-otf--make-simple-glyf-coords-vec
                      flags num-points
                      'y-short-vector 'y-is-same-or-pos-y-short-vec))
      (_ fill (- fill-to bindat-idx)))))

(defvar fontsloth-otf--component-flag-spec
  (bindat-type
    :pack-var f
    (word uint 16 :pack-val f)           ; TODO pack properly
    :unpack-val
    `((args-are-words . ,(= 1 (logand 1 word)))
      (args-are-xy . ,(= 2 (logand 2 word)))
      (round-xy-to-grid . ,(= 4 (logand 4 word)))
      (we-have-a-scale . ,(= 8 (logand 8 word)))
      (more-components . ,(= 32 (logand 32 word)))
      (we-have-an-xy-scale . ,(= 64 (logand 64 word)))
      (we-have-a-two-by-two . ,(= 128 (logand 128 word)))
      (we-have-instructions . ,(= 256 (logand 256 word)))
      (use-my-metrics . ,(= 512 (logand 512 word)))
      (overlap-compound . ,(= 1024 (logand 1024 word))))))

(defun fontsloth-otf--make-component-argument-spec (flags)
  "Given FLAGS, make a bindat spec to for a component-argument."
  (pcase `(,(alist-get 'args-are-words flags)
           ,(alist-get 'args-are-xy flags))
    ('(t t) (bindat-type sint 16 nil))
    ('(t nil) (bindat-type uint 16))
    ('(nil t) (bindat-type sint 8 nil))
    ('(nil nil) (bindat-type uint 8))))

(defun fontsloth-otf--make-transform-option-spec (flags)
  "Given FLAGS, make a bindat spec to for a transform-option."
  (pcase `(,(alist-get 'we-have-a-scale flags)
           ,(alist-get 'we-have-an-xy-scale flags)
           ,(alist-get 'we-have-a-two-by-two flags))
    ('(nil nil nil) (bindat-type
                      (a unit 1.0) (b unit 0.0)
                      (c unit 0.0) (d unit 1.0)))
    ('(t nil nil) (bindat-type
                    (a f-2.14) (b unit 0.0)
                    (c unit 0.0) (d unit a)))
    ('(nil t nil) (bindat-type
                    (a f-2.14) (b unit 0.0)
                    (c unit 0.0) (d f-2.14)))
    ('(nil nil t) (bindat-type
                    (a f-2.14) (b f-2.14)
                    (c f-2.14) (d f-2.14)))))

(defun fontsloth-otf--make-composite-glyf-data-spec (range)
  "Given RANGE make a bindat spec to parse composite glyph data.
RANGE length in bytes from loca for data, excluding header size"
  (let ((component-start) (fill-to))
    (bindat-type
      (_ unit (progn (setf component-start bindat-idx
                           fill-to (+ bindat-idx range)) nil))
      (flags type fontsloth-otf--component-flag-spec)
      (glyph-id uint 16)
      (argument1 type (fontsloth-otf--make-component-argument-spec flags))
      (argument2 type (fontsloth-otf--make-component-argument-spec flags))
      (transform-option type (fontsloth-otf--make-transform-option-spec flags))
      (next-component type (if (alist-get 'more-components flags)
                               (fontsloth-otf--make-composite-glyf-data-spec
                                (- range (- bindat-idx component-start)))
                             (bindat-type (_ unit nil))))
      ;; TODO: we-have-instructions
      (_ fill (- fill-to bindat-idx)))))

(defvar fontsloth-otf--glyf-spec
  (let ((loca) (glyf-header-size 10))
    (bindat-type
      (_ unit
         (progn
           (setf loca (fontsloth-otf--get-table-value
                       'glyph-index-to-location "loca")) nil))
      (glyphs vec (fontsloth-otf--get-table-value 'num-glyphs "maxp")
              type
              (if-let (range (fontsloth-otf--glyph-data-range bindat--i loca))
                  (bindat-type
                    (number-of-contours sint 16 nil)
                    (x-min sint 16 nil)
                    (y-min sint 16 nil)
                    (x-max sint 16 nil)
                    (y-max sint 16 nil)
                    (data type
                          (let ((range (- range glyf-header-size)))
                            (if (> 0 number-of-contours)
                                (fontsloth-otf--make-composite-glyf-data-spec
                                 range)
                              (fontsloth-otf--make-simple-glyf-data-spec
                               number-of-contours range)))))
                (bindat-type
                  (missing unit 'missing-char))))))
  "Bindat spec for the TrueType glyf table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/glyf")

(defvar fontsloth-otf--format0-spec
  (let ((header-size 6))                ; includes format uint 16
    (bindat-type
      (length uint 16)
      (language uint 16)
      (data vec (max 0 (- length header-size)) uint 8)))
  "Bindat spec for the Format 0 section of the cmap table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-0-byte-encoding-table")

(defvar fontsloth-otf--format6-spec
  (bindat-type
    (length uint 16)
    (language uint 16)
    (first-code uint 16)
    (entry-count uint 16)
    (data vec entry-count uint 16))
  "Bindat spec for the Format 6 section of the cmap table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-6-trimmed-table-mapping")

(defun fontsloth-otf--calc-glyph-id-offset
    (bytes char-code segment start id-range-offset-start offset)
  "Calculate the format 4 glyphid index for the given `char-code'.
BYTES font with uncompressed cmap table
CHAR-CODE a format 4 char code
SEGMENT the format 4 segment
START the start char code of the current `segment'
ID-RANGE-OFFSET-START the absolute offset of the format 4 index data in cmap
OFFSET the current id range offset for `segment'"
  (if (= 0 offset)
      char-code
    (let* ((current-range-offset (* 2 segment))
           (glyphid-offset
            (+ id-range-offset-start
               current-range-offset
               offset
               (* 2 (- char-code start))))
           (glyph-id (bindat-unpack
                      '((glyph-index u16)) bytes glyphid-offset)))
      (alist-get 'glyph-index glyph-id))))

(defun fontsloth-otf--glyph-index-map (end-code
                         start-code
                         id-delta id-range-offset-start id-range-offset)
  "Compute the format 4 char -> glyph index mapping for each segment in cmap.
END-CODE sequence of char end-codes from cmap
START-CODE sequence of char start-codes from cmap
ID-DELTA sequence of index deltas from cmap
ID-RANGE-OFFSET-START the absolute offset of the format 4 index data in cmap
ID-RANGE-OFFSET sequence of id range offsets from cmap"
  (cl-loop for end across end-code
           for start across start-code
           for delta across id-delta
           for offset across id-range-offset
           for i from 0
           for bytes = (fontsloth-woff--maybe-decompress-table
                        (gethash "cmap"
                                 (gethash "table-directory" fontsloth-otf--current-tables))
                        fontsloth-otf--current-font-bytes)
           append
           (cl-loop for c from start to end
                    collect
                    `(,c . ,(let ((glyph-id-offset
                                   (fontsloth-otf--calc-glyph-id-offset
                                    bytes c i start id-range-offset-start offset)))
                              (mod (+ glyph-id-offset delta) #x10000))))))

(defvar fontsloth-otf--format4-spec
  (bindat-type
    (length uint 16)
    (language uint 16)
    (seg-count-x2 uint 16)
    (seg-count unit (ash seg-count-x2 -1))
    (search-range uint 16)
    (entry-selector uint 16)
    (range-shift uint 16)
    (end-code vec seg-count uint 16)
    (reserved uint 16)
    (start-code vec seg-count uint 16)
    (id-delta vec seg-count sint 16 nil)
    (id-range-offset-start unit bindat-idx)
    (id-range-offset vec seg-count uint 16)
    (glyph-index-map
     unit (fontsloth-otf--glyph-index-map end-code start-code id-delta
                                          id-range-offset-start
                                          id-range-offset)))
  "Bindat spec for the Format 4 section of the cmap table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values")

(defvar fontsloth-otf--format12-spec
  (bindat-type
    (reserved uint 16)
    (length uint 32)
    (lang uint 32)
    (count uint 32)
    (groups vec count type (bindat-type
                             (start-char-code uint 32)
                             (end-char-code uint 32)
                             (start-glyph-id uint 32))))
  "Bindat spec for the Format 12 section of the cmap table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-12-segmented-coverage")

(defvar fontsloth-otf--cmap-spec
  (bindat-type
    (start unit bindat-idx)
    (version uint 16)
    (num-tables uint 16)
    (encodings vec num-tables
               type (bindat-type
                      (platform-id uint 16)
                      (encoding-id uint 16)
                      (offset uint 32)))
    (sub-tables
     vec num-tables
     type (let* ((te (elt encodings bindat--i))
                 (offset (+ start (alist-get 'offset te)))
                 (dup? (seq-find (lambda (e) (= (alist-get 'offset te)
                                                (alist-get 'offset e)))
                                 (seq-subseq encodings 0 bindat--i))))
            (if dup? (bindat-type (duplicate unit dup?))
              (fontsloth-otf--with-offset
               offset nil
               (bindat-type
                 (format uint 16)
                 (_ type
                    (cl-case format
                      (4 fontsloth-otf--format4-spec)
                      (0 fontsloth-otf--format0-spec)
                      (6 fontsloth-otf--format6-spec)
                      (12 fontsloth-otf--format12-spec)
                      (t (bindat-type (unknown-format fill 0)))))))))))
  "Bindat spec for the OTF/TTF cmap table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/cmap")

(bindat-defmacro pascal-str ()
  "A pascal string, starts with non-inclusive length."
  `(struct :pack-var v
           (length uint 8 :pack-val (length v))
           (name str length :pack-val v)
           :unpack-val name))

(defvar fontsloth-otf--post-spec
  (let ((num-pascal-names 0))
    (cl-flet ((pack-idx (idx) (if (consp idx) (+ 258 (cdr idx)) idx))
              (unpack-idx (idx) (if (>= 257 idx) idx
                                  (progn (cl-incf num-pascal-names)
                                         `(pstr . ,(- idx 258))))))
      (bindat-type
        (version v16.16)
        (italic-angle ttf-fixed)
        (underline-position sint 16 nil)
        (underline-thickness sint 16 nil)
        (is-fixed-pitch uint 32)
        (min-mem-type-42 uint 32)
        (max-mem-type-42 uint 32)
        (min-mem-type-1 uint 32)
        (max-mem-type-1 uint 32)
        (name-mapping
          type (progn
                 (setf num-pascal-names 0)
                 (cond ((= 2.0 version)
                        (bindat-type
                          :pack-var v
                          (num-glyphs uint 16 :pack-val (length v))
                          (glyph-name-index vec num-glyphs uint 16
                                            :pack-val (seq-map #'pack-idx v))
                          :unpack-val (apply #'vector
                                             (seq-map #'unpack-idx
                                                      glyph-name-index))))
                       ((= 3.0 version)
                        (bindat-type unit 'no-name-information-provided))
                       (t (bindat-type unit 'unhandled-name-format)))))
        (names vec num-pascal-names pascal-str))))
  "Bindat spec for the OTF/TTF post table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/post")

(defun fontsloth-otf--index-table-props (table-props-list)
  "Convert the `table-props-list' into a map.
TABLE-PROPS-LIST the list of table props to index"
  (let ((m (make-hash-table :test 'equal)))
    (dolist (tprops table-props-list)
      (puthash (alist-get 'tag tprops) tprops m))
    m))

(defun fontsloth-otf--read-font-bytes (path)
  "Read file at PATH as a unibyte string of raw bytes."
  (let ((file-name-handler-alist nil)
        (auto-mode-alist nil))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (buffer-string))))

(defun fontsloth-otf--parse-tables (bytes coll-index)
  "Parse font BYTES into a tag -> table map.
COLL-INDEX is currently ignored (font collections are not supported)."
  (ignore coll-index)
  (let* ((fontsloth-otf--current-tables (make-hash-table :test 'equal))
         (fontsloth-otf--current-font-bytes bytes)
         (fontsloth-otf--current-font
          (fontsloth-otf-font-create :tables fontsloth-otf--current-tables
                                     :bytes bytes))
         (header+table-props
          (bindat-unpack fontsloth-otf--tables-spec bytes))
         ;; sfnt-ver to check if there is either TrueType or CFF data
         (sfnt-ver (bindat-get-field header+table-props 'header 'sfnt-version))
         (props (fontsloth-otf--index-table-props
                 (bindat-get-field header+table-props 'table-props))))
    (cl-flet ((unpack-table (tag spec &optional local-offset)
                (let ((props (gethash tag props)))
                  (bindat-unpack spec (fontsloth-woff--maybe-decompress-table
                                       props fontsloth-otf--current-font-bytes)
                                 (+ (or local-offset 0)
                                    (alist-get 'offset props)))))
              (put-table (tag data)
                (puthash tag data fontsloth-otf--current-tables)))
      (put-table "table-directory" props)
      (put-table "head" (unpack-table "head" fontsloth-otf--head-spec))
      (put-table "maxp" (unpack-table "maxp" fontsloth-otf--maxp-spec))
      (put-table "hhea" (unpack-table "hhea" fontsloth-otf--hhea-spec))
      (put-table "hmtx" (unpack-table "hmtx" fontsloth-otf--hmtx-spec))
      (put-table "cmap" (unpack-table "cmap" fontsloth-otf--cmap-spec))
      (put-table "post" (unpack-table "post" fontsloth-otf--post-spec))
      (cond ((string-equal "   " sfnt-ver)
             (put-table "loca" (unpack-table "loca" fontsloth-otf--loca-spec))
             (put-table "glyf" (unpack-table "glyf" fontsloth-otf--glyf-spec)))
            ((string-equal "OTTO" sfnt-ver)
             (fontsloth:info
              fontsloth-log
              "Fontsloth-otf: cannot yet fully handle OpenType CFF")
             (put-table "CFF " (unpack-table "CFF " fontsloth-otf-cff--spec)))
            (t (error "Fontsloth-otf: Unknown sfnt-ver %s" sfnt-ver)))
      (when (gethash "kern" props)
        (put-table "kern" (unpack-table "kern" fontsloth-otf-kern-spec)))
      ;; (when (gethash "GDEF" props)
      ;;   (put-table "GDEF" (unpack-table "GDEF" fontsloth-otf-typo-gdef-spec)))
      (when (gethash "GPOS" props)
        (put-table "GPOS" (unpack-table "GPOS" fontsloth-otf-typo-gpos-spec))
        (put-table
         "GPOS"
         (cons `(mappings . , (fontsloth-otf-typo-gpos-build-kern-mappings))
               (gethash "GPOS" fontsloth-otf--current-tables))))
      fontsloth-otf--current-tables)))

(cl-defun fontsloth-otf-load-font-data (ttf-path &key (coll-index 0))
  "Read `ttf-path' into a `fontsloth-otf-font' suitable for rendering.
TTF-PATH the path to a ttf file
:COLL-INDEX the collection index if this file is a collection, default 0"
  (let* ((bytes (fontsloth-otf--read-font-bytes ttf-path))
         (tables (fontsloth-otf--parse-tables bytes coll-index)))
    (fontsloth-otf-font-create :tables tables :bytes bytes)))

(cl-defun fontsloth-otf-load-font (ttf-path &key (coll-index 0))
  "Read `ttf-path' into an abstract representation suitable for rendering.
Returns the table map and makes it the current dynamic font context.
Use `fontsloth-otf-load-font-data' to get an explicit
`fontsloth-otf-font' instead.
TTF-PATH the path to a ttf file
:COLL-INDEX the collection index if this file is a collection, default 0"
  (let ((font (fontsloth-otf-load-font-data ttf-path :coll-index coll-index)))
    (setq fontsloth-otf--current-font font
          fontsloth-otf--current-tables (fontsloth-otf-font-tables font)
          fontsloth-otf--current-font-bytes (fontsloth-otf-font-bytes font))
    fontsloth-otf--current-tables))

(defun fontsloth-otf-num-glyphs (&optional font)
  "If a font is loaded, find its specified number of glyphs.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  (when-let ((maxp (gethash "maxp" (fontsloth-otf--font-tables font))))
    (alist-get 'num-glyphs maxp)))

(defun fontsloth-otf-units-per-em (&optional font)
  "If a font is loaded, find its units-per-em.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  (when-let ((head (gethash "head" (fontsloth-otf--font-tables font))))
    (alist-get 'units-per-em head)))

(defun fontsloth-otf-char-to-glyph-map (&optional font)
  "Get the font's code-point -> glyph id mapping.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  (when-let* ((cmap (gethash "cmap" (fontsloth-otf--font-tables font)))
              (sub-tables (alist-get 'sub-tables cmap))
              (map (make-hash-table
                    :size (ash (fontsloth-otf-num-glyphs font) 1)
                    :test 'eq)))
    (cl-loop for tbl across sub-tables do
             (cl-case (alist-get 'format tbl)
               (4 (cl-loop for (c . g) in (alist-get 'glyph-index-map tbl) do
                           (puthash c g map)))
               (0 (cl-loop for c from 0
                           for g across (alist-get 'data tbl) do
                           (puthash c g map)))
               (6 (let ((first-c (alist-get 'first-code tbl)))
                    (cl-loop for c from first-c
                             for g across (alist-get 'data tbl) do
                             (puthash c g map))))
               (12 (cl-loop for ((_ . sc) (_ . ec) (_ . sg)) across
                            (alist-get 'groups tbl) do
                            (cl-loop for c from sc
                                     for g from sg
                                     while (<= c ec) do
                                     (puthash c g map))))
               (_))
             finally return map)))

(defun fontsloth-otf-glyph-id-for-code-point (code-point &optional font)
  "Return the font's glyph index for a given code point or nil if not found.
CODE-POINT a character code point
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  (when-let ((glyph-index-map (fontsloth-otf-char-to-glyph-map font)))
    (gethash code-point glyph-index-map)))

(defun fontsloth-otf-glyph-name (glyph-id &optional font)
  "Return the name the font specifies for the glyph or nil if none is given.
GLYPH-ID the glyph-id
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  (when-let* ((post (gethash "post" (fontsloth-otf--font-tables font)))
              (name-map (alist-get 'name-mapping post)))
    (when (vectorp name-map)
      (let ((idx (elt name-map glyph-id)))
        (if (consp idx)
            (elt (alist-get 'names post) (cdr idx))
          (elt fontsloth-otf--mac-names idx))))))

(defun fontsloth-otf-ascender (&optional font)
  "Return the font's horizontal face ascender.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  ;; TODO: parse OS/2 table
  ;; TODO: support variation axes
  (when-let ((hhea (gethash "hhea" (fontsloth-otf--font-tables font))))
    (alist-get 'ascent hhea)))

(defun fontsloth-otf-descender (&optional font)
  "Return the font's horizontal face descender.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  ;; TODO: parse OS/2 table
  ;; TODO: support variation axes
  (when-let ((hhea (gethash "hhea" (fontsloth-otf--font-tables font))))
    (alist-get 'descent hhea)))

(defun fontsloth-otf-line-gap (&optional font)
  "Return the font's horizontal face line gap.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  ;; TODO: parse OS/2 table
  ;; TODO: support variation axes
  (when-let ((hhea (gethash "hhea" (fontsloth-otf--font-tables font))))
    (alist-get 'line-gap hhea)))

(defun fontsloth-otf-glyph-hor-advance (glyph-id &optional font)
  "Return the horizontal advance for GLYPH-ID.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  ;; Todo: support font variations
  (when-let* ((hmtx (gethash "hmtx" (fontsloth-otf--font-tables font)))
              (metrics (alist-get 'hmetrics hmtx))
              (num-glyphs (fontsloth-otf-num-glyphs font)))
    (let ((num-metrics
           (fontsloth-otf--get-table-value 'num-of-long-hor-metrics "hhea"
                                           font)))
      (unless (<= num-glyphs glyph-id)
        ;; 'As an optimization, the number of records can be less than the
        ;; number of glyphs, in which case the advance width value of the last
        ;; record applies to all remaining glyph IDs.'
        (if (< glyph-id num-metrics)
            (alist-get 'advance-width (elt metrics glyph-id))
          (alist-get 'advance-width (elt metrics (1- num-metrics))))))))

(defun fontsloth-otf-glyph-ver-advance (glyph-id &optional font)
  "Return the vertical advance for a GLYPH-ID.
FONT is ignored; vertical metrics are not implemented yet."
  (ignore glyph-id font)
  ;; TODO: parse vmtx
  ;; TODO: support font variations
  (error "Glyph-ver-advance not yet implemented"))

(defun fontsloth-otf-find-hkern-mappings (&optional font)
  "Try to find indexed horizontal pair kern mappings for the current font.
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  (when-let ((table (or (gethash "kern" (fontsloth-otf--font-tables font))
                        (gethash "GPOS" (fontsloth-otf--font-tables font)))))
    (alist-get 'mappings table)))

;;; generics for glyph outline construction (not table specific):

(cl-defgeneric fontsloth-otf-move-to (outliner x y)
  "Move to the start of a contour.
OUTLINER contour outliner implementation
X x coord of the start point
Y y coord of the start point"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline move-to %s %s" x y))

(cl-defgeneric fontsloth-otf-line-to (outliner x y)
  "Append a line-to segment to OUTLINER's contour.
X x coord of the line end point
Y y coord of the line end point"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline line-to %s %s" x y))

(cl-defgeneric fontsloth-otf-quad-to (outliner x1 y1 x y)
  "Append a quad-to segment to OUTLINER's contour.
X1 x coord of control point
Y1 y coord of control point
X x coord of curve end
Y y coord of curve end"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline quad-to %s %s %s %s"
                     x1 y1 x y))

(cl-defgeneric fontsloth-otf-curve-to (outliner x1 y1 x2 y2 x y)
  "Append a curve-to segment to OUTLINER's contour.
X1 x coord of control point
Y1 y coord of control point
X2 x coord of control point
Y2 y coord of control point
X x coord of curve end
Y y coord of curve end"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline curve-to %s %s %s %s %s %s"
                     x1 y1 x2 y2 x y))

(cl-defgeneric fontsloth-otf-close-contour (outliner)
  "End OUTLINER's contour."
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline close contour"))

;;; glyph outlining fns

(defun fontsloth-otf-outline-glyph (glyph-id outliner &optional font)
  "Outline a glyph using a caller provided outliner.
The caller is expected to define methods for the following
generics to dispatch on their outliner type:
`fontsloth-otf-move-to'
`fontsloth-otf-line-to'
`fontsloth-otf-quad-to'
`fontsloth-otf-curve-to'
`fontsloth-otf-close-contour'
GLYPH-ID the id of the glyph to outline
OUTLINER the caller's outliner implementation
FONT an optional `fontsloth-otf-font'; defaults to the current context."
  ;; TODO gvar
  (fontsloth-otf-with-font font
    (if-let ((glyf (gethash "glyf" (fontsloth-otf--font-tables font))))
        (fontsloth-otf-glyf-outline (alist-get 'glyphs glyf) glyph-id outliner)
      (when-let ((cff (gethash "CFF " (fontsloth-otf--font-tables font))))
        (fontsloth-otf-cff-outline cff glyph-id outliner)))))

(provide 'fontsloth-otf)
;;; fontsloth-otf.el ends here
