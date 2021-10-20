;;; fontsloth-otf.el --- Elisp otf/ttf bindat parser -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.12.1
;; Package-Requires: ((cl-lib "1.0") (names "20151201.0") (emacs "26.1"))
;; Keywords: true-type, font, bindat, ttf, otf, parsing

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
;; elisp. inspired by fontdue

;; fontsloth-otf (this file): uses bindat to parse otf/ttf files

;; To use this module by itself, load and enable it as follows:
;;   (use-package fontsloth-otf)
;;

;;; Code:

(require 'bindat)
(require 'cl-lib)

(require 'fontsloth-log)
(require 'fontsloth-otf--mac-names)
(require 'fontsloth-otf--outline-glyf)


(defvar fontsloth-otf--header-spec
  '((sfnt-version str 4)
    (num-tables u16)
    (search-range u16)
    (entry-selector u16)
    (range-shift u16))
  "Bindat spec for the OTF/TTF table directory header.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tabledirectory")

(defvar fontsloth-otf--table-props-spec
  '((tag str 4)
    (checksum u32)
    (offset u32)
    (length u32))
  "Bindat spec for a single entry in the OTF/TTF table directory.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/otff#tabledirectory")

(defvar fontsloth-otf--tables-spec
  '((header struct fontsloth-otf--header-spec)
    (table-props repeat (header num-tables)
                 (struct fontsloth-otf--table-props-spec)))
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

(defun fontsloth-otf--get-table-value (field tag)
  "Get a value from the named table in the current context.
FIELD the table field
TAG the table tag"
  (alist-get field (gethash tag fontsloth-otf--current-tables)))

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

(defvar fontsloth-otf--simple-glyf-flag-spec
  (bindat-type
    :pack-var f
    (byte uint 8 :pack-val f)           ; TODO pack properly
    :unpack-val
    `((on-curve-point . ,(= 1 (logand 1 byte)))
      (x-short-vector . ,(= 2 (logand 2 byte)))
      (y-short-vector . ,(= 4 (logand 4 byte)))
      (repeat . ,(= 8 (logand 8 byte)))
      (x-is-same-or-pos-x-short-vec
       . ,(= 16 (logand 16 byte)))
      (y-is-same-or-pos-y-short-vec
       . ,(= 32 (logand 32 byte)))
      (overlap-simple . ,(= 64 (logand 64 byte)))
      (reserved . ,(= 128 (logand 128 byte))))))

;; TODO compute-x and compute-y could be done with a macro or wrapper fn

(defun fontsloth-otf--prev-coord (prev idx)
  (if (= 0 idx) 0
    (elt prev (1- idx))))

(defun fontsloth-otf--compute-x-type-from-flag (flag idx prev-x)
  "Given the point flags at point `idx', make a bindat type for the x coord.
FLAG the point flags for point at `idx'
IDX the point index
PREV-X sequence of previous x coords"
  (let* ((x-short-vector (alist-get 'x-short-vector flag))
         (x-is-same-or-pos-x-short-vec
          (alist-get 'x-is-same-or-pos-x-short-vec flag)))
    (let ((x-type
           (if x-short-vector
               (if x-is-same-or-pos-x-short-vec
                   (bindat-type u8)
                 (bindat-type :pack-var v (b u8 :pack-val (* -1 v))
                              :unpack-val (* -1 b)))
             (if x-is-same-or-pos-x-short-vec
                 (bindat-type unit 0)
               (bindat-type sint 16 nil)))))
      (bindat-type :pack-var v
                   (dx type x-type
                       :pack-val (- v (fontsloth-otf--prev-coord prev-x idx)))
                   :unpack-val (+ dx (fontsloth-otf--prev-coord prev-x idx))))))

(defun fontsloth-otf--compute-y-type-from-flag (flag idx prev-y)
  "Given the point flags at point `idx', make a bindat type for the y coord.
FLAG the point flags for point at `idx'
IDX the point index
PREV-Y sequence of previous y coords"
  (let ((y-short-vector (alist-get 'y-short-vector flag))
        (y-is-same-or-pos-y-short-vec
         (alist-get 'y-is-same-or-pos-y-short-vec flag)))
    (let ((y-type
           (if y-short-vector
               (if y-is-same-or-pos-y-short-vec
                   (bindat-type u8)
                 (bindat-type :pack-var v (b u8 :pack-val (* -1 v))
                              :unpack-val (* -1 b)))
             (if y-is-same-or-pos-y-short-vec
                 (bindat-type unit 0)
               (bindat-type sint 16 nil)))))
      (bindat-type :pack-var v
                   (dy type y-type
                       :pack-val (- v (fontsloth-otf--prev-coord prev-y idx)))
                   :unpack-val (+ dy (fontsloth-otf--prev-coord prev-y idx))))))

(defun fontsloth-otf--make-simple-glyf-data-spec (num-contours range)
  "Given number of contours make a bindat spec to parse simple glyph data.
NUM-CONTOURS number of contours for the glyph, positive for simple data
RANGE length in bytes from loca for data, excluding header size"
  (let ((flag-repeat-counter) (flag-to-repeat) (fill-to))
    (bindat-type
      (_ unit (progn (setf flag-repeat-counter 0
                           flag-to-repeat nil
                           fill-to (+ bindat-idx range)) nil))
      (end-pts vec num-contours uint 16)
      (instruction-length uint 16)
      (instructions vec instruction-length uint 8)
      (num-points unit (1+ (elt end-pts (1- num-contours))))
      (flags vec num-points type
             (if (< 0 flag-repeat-counter)
                 (bindat-type
                   (flag unit flag-to-repeat)
                   (_ unit (progn (cl-decf flag-repeat-counter) nil)))
               (bindat-type
                 (flag type fontsloth-otf--simple-glyf-flag-spec)
                 (repeat type (if (alist-get 'repeat flag)
                                  (bindat-type uint 8)
                                (bindat-type unit nil)))
                 (_ unit (when repeat
                           (setf flag-repeat-counter repeat
                                 flag-to-repeat flag) nil)))))
      (x-coords vec num-points type
                (fontsloth-otf--compute-x-type-from-flag
                 (car (elt flags bindat--i))
                 bindat--i bindat--v))
      (y-coords vec num-points type
                (fontsloth-otf--compute-y-type-from-flag
                 (car (elt flags bindat--i))
                 bindat--i bindat--v))
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
                                (bindat-type vec range uint 8)
                              (fontsloth-otf--make-simple-glyf-data-spec
                               number-of-contours range)))))
                (bindat-type
                  (missing unit 'missing-char))))))
  "Bindat spec for the TrueType glyf table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/glyf")

(defvar fontsloth-otf--format0-spec
  (let ((header-size 8))                ; includes format uint 16
    (bindat-type
      (length uint 16)
      (version uint 16)
      (language uint 16)
      (data fill (- length header-size))))
  "Bindat spec for the Format 0 section of the cmap table.
see URL https://docs.microsoft.com/en-us/typography/opentype/spec/cmap#format-0-byte-encoding-table")

(defun fontsloth-otf--calc-glyph-id-offset (char-code segment start id-range-offset-start offset)
  "Calculate the format 4 glyphid index for the given `char-code'.
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
                      '((glyph-index u16)) fontsloth-otf--current-font-bytes glyphid-offset)))
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
           append
           (cl-loop for c from start to end
                    collect
                    `(,c . ,(let ((glyph-id-offset
                                   (fontsloth-otf--calc-glyph-id-offset
                                    c i start id-range-offset-start offset)))
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

(defvar fontsloth-otf--cmap-spec
  (bindat-type
    (version uint 16)
    (num-tables uint 16)
    (encodings vec num-tables
               type (bindat-type
                      (platform-id uint 16)
                      (encoding-id uint 16)
                      (offset uint 32)))
    (sub-tables
     vec num-tables
     type (bindat-type
            (format uint 16)
            (fmt-table type
                       (cond ((= 4 format) fontsloth-otf--format4-spec)
                             ((= 0 format) fontsloth-otf--format0-spec)
                             ;; TODO fill with appropriate length
                             (t (bindat-type (unknown-format fill 0))))))))
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

(cl-defun fontsloth-otf-load-font (ttf-path &key (coll-index 0))
  "Read `ttf-path' into an abstract representation suitable for rendering.
TTF-PATH the path to a ttf file
:COLL-INDEX the collection index if this file is a collection, default 0"
  (setq fontsloth-otf--current-tables (make-hash-table :test 'equal))
  (setq fontsloth-otf--current-font-bytes (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally ttf-path)
                              (buffer-string)))
  (let* ((header+table-props (bindat-unpack fontsloth-otf--tables-spec
                                            fontsloth-otf--current-font-bytes))
         ;; sfnt-ver to check if there is either TrueType or CFF data
         (sfnt-ver (bindat-get-field header+table-props 'header 'sfnt-version))
         (props (fontsloth-otf--index-table-props
                 (bindat-get-field header+table-props 'table-props))))
    (cl-flet ((unpack-table (tag spec &optional local-offset)
                (bindat-unpack spec fontsloth-otf--current-font-bytes
                               (+ (or local-offset 0)
                                  (alist-get 'offset (gethash tag props)))))
              (put-table (tag data)
                (puthash tag data fontsloth-otf--current-tables)))
      (put-table "table-directory" header+table-props)
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
              "fontsloth-otf: cannot yet fully handle OpenType CFF"))
            (t (fontsloth:error "fontsloth-otf: unknown sfnt-ver %s" sfnt-ver)))
      fontsloth-otf--current-tables)))

(defun fontsloth-otf-num-glyphs ()
  "If a font is loaded, find its specified number of glyphs."
  (when-let ((maxp (gethash "maxp" fontsloth-otf--current-tables)))
    (alist-get 'num-glyphs maxp)))

(defun fontsloth-otf-units-per-em ()
  "If a font is loaded, find its units-per-em."
  (when-let ((head (gethash "head" fontsloth-otf--current-tables)))
    (alist-get 'units-per-em head)))

(defun fontsloth-otf-char-to-glyph-map ()
  "Get the font's code-point -> glyph id mapping."
  ;; TODO handle other formats
  ;; TODO hold somewhere a reference to the format 4 table after first lookup
  (cl-flet ((format4? (table) (= 4 (alist-get 'format table))))
    (when-let* ((cmap (gethash "cmap" fontsloth-otf--current-tables))
                (sub-tables (bindat-get-field cmap 'sub-tables))
                (format4-table (cadar (seq-filter #'format4? sub-tables))))
      (alist-get 'glyph-index-map format4-table))))

(defun fontsloth-otf-glyph-id-for-code-point (code-point)
  "Return the font's glyph index for a given code point or nil if not found.
CODE-POINT a character code point"
  (when-let ((glyph-index-map (fontsloth-otf-char-to-glyph-map)))
    (alist-get code-point glyph-index-map)))

(defun fontsloth-otf-glyph-name (glyph-id)
  "Return the name the font specifies for the glyph or nil if none is given.
GLYPH-ID the glyph-id"
  (when-let* ((post (gethash "post" fontsloth-otf--current-tables))
              (name-map (alist-get 'name-mapping post)))
    (when (vectorp name-map)
      (let ((idx (elt name-map glyph-id)))
        (if (consp idx)
            (elt (alist-get 'names post) (cdr idx))
          (elt fontsloth-otf--mac-names idx))))))

(defun fontsloth-otf-ascender ()
  "Return the font's horizontal face ascender."
  ;; TODO: parse OS/2 table
  ;; TODO: support variation axes
  (when-let ((hhea (gethash "hhea" fontsloth-otf--current-tables)))
    (alist-get 'ascent hhea)))

(defun fontsloth-otf-descender ()
  "Return the font's horizontal face descender."
  ;; TODO: parse OS/2 table
  ;; TODO: support variation axes
  (when-let ((hhea (gethash "hhea" fontsloth-otf--current-tables)))
    (alist-get 'descent hhea)))

(defun fontsloth-otf-line-gap ()
  "Return the font's horizontal face line gap."
  ;; TODO: parse OS/2 table
  ;; TODO: support variation axes
  (when-let ((hhea (gethash "hhea" fontsloth-otf--current-tables)))
    (alist-get 'line-gap hhea)))

(defun fontsloth-otf-glyph-hor-advance (glyph-id)
  "Return the horizontal advance for a glyph."
  ;; TODO: support font variations
  (when-let* ((hmtx (gethash "hmtx" fontsloth-otf--current-tables))
              (metrics (alist-get 'hmetrics hmtx))
              (num-glyphs (fontsloth-otf-num-glyphs)))
    (let ((num-metrics
           (fontsloth-otf--get-table-value 'num-of-long-hor-metrics "hhea")))
      (unless (<= num-glyphs glyph-id)
        ;; 'As an optimization, the number of records can be less than the
        ;; number of glyphs, in which case the advance width value of the last
        ;; record applies to all remaining glyph IDs.'
        (if (< glyph-id num-metrics)
            (alist-get 'advance-width (elt metrics glyph-id))
          (alist-get 'advance-width (elt metrics (1- num-metrics))))))))

(defun fontsloth-otf-glyph-ver-advance (glyph-id)
  "Return the vertical advance for a glyph."
  (ignore glyph-id)
  ;; TODO: parse vmtx
  ;; TODO: support font variations
  (error "glyph-ver-advance not yet implemented"))

;;; generics for glyph outline construction (not table specific):

(cl-defgeneric fontsloth-otf-move-to (outliner x y)
  "Move to the start of a contour.
OUTLINER contour outliner implementation
X x coord of the start point
Y y coord of the start point"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline move-to %s %s" x y))

(cl-defgeneric fontsloth-otf-line-to (outliner x y)
  "Append a line-to segment to the contour.
X x coord of the line end point
Y y coord of the line end point"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline line-to %s %s" x y))

(cl-defgeneric fontsloth-otf-quad-to (outliner x1 y1 x y)
  "Append a quad-to segment to the contour.
X1 x coord of control point
Y1 y coord of control point
X x coord of curve end
Y y coord of curve end"
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline quad-to %s %s %s %s"
                     x1 y1 x y))

(cl-defgeneric fontsloth-otf-curve-to (outliner)
  "Append a curve-to segment to the contour.")

(cl-defgeneric fontsloth-otf-close-contour (outliner)
  "End a contour."
  (ignore outliner)
  (fontsloth:verbose fontsloth-log "Noop outline close contour"))

;;; glyph outlining fns

(defun fontsloth-otf-outline-glyph (glyph-id outliner)
  "Outline a glyph using a caller provided outliner.
The caller is expected to define methods for the following
generics to dispatch on their outliner type:
`fontsloth-otf-move-to'
`fontsloth-otf-line-to'
`fontsloth-otf-quad-to'
`fontsloth-otf-curve-to'
`fontsloth-otf-close-contour'
GLYPH-ID the id of the glyph to outline
OUTLINER the caller's outliner implementation"
  ;; TODO gvar and cff, composite glyphs
  (when-let ((glyf (gethash "glyf" fontsloth-otf--current-tables)))
    (fontsloth-otf--glyf-outline (alist-get 'glyphs glyf) glyph-id outliner)))

(provide 'fontsloth-otf)
;;; fontsloth-otf.el ends here
