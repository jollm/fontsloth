;;; fontsloth-otf-typo.el --- Advanced typography table portion of otf/ttf bindat parser -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.2
;; Homepage: https://github.com/jollm/fontsloth
;; Package-Requires: ((emacs "26.1"))
;; Keywords: data, font, bindat, ttf, otf, parsing

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

;; Part of fontsloth.

;; fontsloth-otf-typo.el: uses bindat to parse the advanced typography
;; portions of otf/ttf files. if it appears to be bananas it kind of is


;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'map)
(require 'seq)

(require 'fontsloth-log)

(defsubst fontsloth-otf--offset-spec (offset spec)
  "Offset a bindat SPEC to OFFSET index.
OFFSET is the true offset, not relative.

This is helpful because everything in GGG-land is done with offsets.
XXX: sometimes you will have to set the offset back :()
(.e.g. if you want keep reading from the previous place)
this needs to understand how better to work with bindat-idx,
maybe a with-offset would be a nice addition"
  (bindat-type type (progn (setq bindat-idx offset) spec)))

(defvar fontsloth-otf--glyph-range-spec
  (bindat-type
    (range-count uint 16)
    (ranges
     vec range-count
     type (bindat-type
            (start-glyph-id uint 16)
            (end-glyph-id uint 16)
            ;; this is the class id in a class def and the start coverage index
            ;; in a coverage table
            (index uint 16))))
  "A spec for glyph ranges used by class defs and coverage tables.")

(defvar fontsloth-otf--gdef-glyph-class-def-spec
  (bindat-type
    (format uint 16)
    (_ type (cl-case format
              (1 (bindat-type
                   (start-glyph-id uint 16)
                   (glyph-count uint 16)
                   (class-value-array vec glyph-count uint 16)))
              (2 fontsloth-otf--glyph-range-spec))))
  "A spec for a class def used by GPOS and GDEF.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#class-definition-table'")

(defvar fontsloth-otf--coverage-spec
  (bindat-type
    (format uint 16)
    (_ type (cl-case format
              (1 (bindat-type
                   (glyph-count uint 16)
                   (glyphs vec glyph-count uint 16)))
              (2 fontsloth-otf--glyph-range-spec))))
  "A spec for a GPOS Coverage table.
Note that this is very similar to class def table but not quite!
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#coverage-table'")

(defvar fontsloth-otf--gdef-spec
  (bindat-type
    (major-version uint 16)
    (minor-version uint 16)
    (glyph-class-def-offset uint 16)
    (attach-list-offset uint 16)
    (lig-caret-offset uint 16)
    (mark-attach-class-def-offset uint 16)
    (mark-glyph-sets-def-offset
     type (if (= 2 minor-version) (bindat-type uint 16)
            (bindat-type unit nil)))
    (item-var-store-offset
     type (if (= 3 minor-version) (bindat-type uint 32)
            (bindat-type unit nil)))
    (glyph-class-def type
                     (if (/= 0 glyph-class-def-offset)
                       (let ((bindat-idx glyph-class-def-offset))
                         fontsloth-otf--gdef-glyph-class-def-spec)
                       (bindat-type unit nil)))
    (mark-attach-class-def type
                           (if (/= 0 mark-attach-class-def-offset)
                               (let ((bindat-idx mark-attach-class-def-offset))
                                 fontsloth-otf--gdef-glyph-class-def-spec)
                             (bindat-type unit nil))))
  "A spec for OTF GDEF.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gdef'")

(defvar fontsloth-otf--lang-sys-spec
  (bindat-type
    (lookup-order uint 16)
    (req-feature-idx uint 16)
    (feature-count uint 16)
    (feature-indices vec feature-count uint 16))
  "A spec for a GPOS script language.")

(defvar fontsloth-otf--script-list-spec
  (bindat-type
    (script-list-offset unit bindat-idx)
    (count uint 16)
    (script-offsets vec count type
                    (bindat-type
                      (tag str 4)
                      (offset uint 16)))
    (scripts vec count type
             (progn (setf bindat-idx
                          (+ script-list-offset
                             (alist-get 'offset
                                        (elt script-offsets bindat--i))))
                    (bindat-type
                      (tag unit
                           (alist-get 'tag (elt script-offsets bindat--i)))
                      (default-lang-sys-offset uint 16)
                      (lang-sys-count uint 16)
                      (default-lang-sys type
                        (fontsloth-otf--offset-spec
                         (+ default-lang-sys-offset
                            (alist-get 'offset (elt script-offsets bindat--i))
                            script-list-offset)
                         fontsloth-otf--lang-sys-spec))
                      ;; TODO: read all lang-sys
                      ))))
  "A spec for a GPOS script list.")

(defvar fontsloth-otf--feature-list-spec
  (bindat-type
    (feature-list-offset unit bindat-idx)
    (count uint 16)
    (feature-offsets vec count type (bindat-type (tag str 4) (offset uint 16)))
    (features vec count type
              (progn (setf bindat-idx
                           (+ feature-list-offset
                              (alist-get 'offset
                                         (elt feature-offsets bindat--i))))
                     (bindat-type
                       (tag unit
                            (alist-get 'tag (elt feature-offsets bindat--i)))
                       (thing uint 16)
                       (lookup-count uint 16)
                       (lookup-indices vec lookup-count uint 16)))))
  "A spec for a GPOS feature list.")

(defun fontsloth-otf--make-value-record-spec (format)
  "Given FORMAT, make a GPOS value record spec.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#valueRecord'
Currently only format 4 x-advance is supported.
TODO: support more formats"
  (cl-case format
    (0 (bindat-type unit nil))
    (4 (bindat-type (x-advance sint 16 nil)))
    (t (bindat-type unit nil))))

(defun fontsloth-otf--make-pair-set-spec (value-format-1 value-format-2)
  "Given VALUE-FORMAT-1 and VALUE-FORMAT2, return a GPOS pair set spec.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-2-pair-adjustment-positioning-subtable'"
  (bindat-type
    (count uint 16)
    (pair-values vec count type
                 (bindat-type
                   (second-glyph-id uint 16)
                   (value-1 type (fontsloth-otf--make-value-record-spec
                                  value-format-1))
                   (value-2 type (fontsloth-otf--make-value-record-spec
                                  value-format-2))))))

(defun fontsloth-otf--make-pair-pos-spec (offset format)
  "Given pair pos table OFFSET and FORMAT, return a GPOS pair pos spec.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-2-pair-adjustment-positioning-subtable"
  (cl-case format
    (1 (bindat-type
         (coverage-offset uint 16)
         (_mark-offset unit bindat-idx)
         (coverage type (fontsloth-otf--offset-spec
                         (+ offset coverage-offset)
                         fontsloth-otf--coverage-spec))
         (_ unit (progn (setq bindat-idx _mark-offset) nil))
         (value-format-1 uint 16)
         (value-format-2 uint 16)
         (pair-set-count uint 16)
         (pair-set-offsets
          vec pair-set-count uint 16)
         (pair-sets vec pair-set-count type
                    (fontsloth-otf--offset-spec
                     (+ offset (elt pair-set-offsets bindat--i))
                     (fontsloth-otf--make-pair-set-spec
                      value-format-1 value-format-2)))))
    (2 (bindat-type
         (coverage-offset uint 16)
         (_mark-offset unit bindat-idx)
         (coverage type (fontsloth-otf--offset-spec
                         (+ offset coverage-offset)
                         fontsloth-otf--coverage-spec))
         (_ unit (progn (setq bindat-idx _mark-offset) nil))
         (value-format-1 uint 16)
         (value-format-2 uint 16)
         (class-def-1-offset uint 16)
         (mark-offset unit bindat-idx)
         (class-def-1 type
                      (fontsloth-otf--offset-spec
                       (+ offset class-def-1-offset)
                       fontsloth-otf--gdef-glyph-class-def-spec))
         (_ unit (progn (setq bindat-idx mark-offset) nil))
         (class-def-2-offset uint 16)
         (mark-offset unit bindat-idx)
         (class-def-2 type
                      (if (= 0 class-def-2-offset)
                          (bindat-type unit nil)
                        (fontsloth-otf--offset-spec
                         (+ offset class-def-2-offset)
                         fontsloth-otf--gdef-glyph-class-def-spec)))
         (_ unit (progn (setq bindat-idx mark-offset) nil))
         (class1-count uint 16)
         (class2-count uint 16)
         (class-records
          vec class1-count type
          (bindat-type
            vec class2-count type
            (bindat-type
              (value-1 type
                       (fontsloth-otf--make-value-record-spec
                        value-format-1))
              (value-2 type
                       (fontsloth-otf--make-value-record-spec
                        value-format-2)))))))))

(defvar fontsloth-otf--pair-pos-spec
  (bindat-type
    (offset unit bindat-idx)
    (pair-pos-format uint 16)
    (_ type (fontsloth-otf--make-pair-pos-spec offset pair-pos-format)))
  "A spec for a GPOS pair positioning lookup subtable.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-2-pair-adjustment-positioning-subtable")

(defvar fontsloth-otf--mark-array-spec
  (bindat-type
    (mark-base-offset unit bindat-idx)
    (mark-count uint 16)
    (mark-records vec mark-count type
                  (bindat-type
                    (mark-class uint 16)
                    (anchor-offset uint 16))))
  "A spec for a GPOS mark array.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#markArrayTable'")

(defvar fontsloth-otf--anchor-spec
  (bindat-type
    (anchor-format uint 16)
    (x-coord uint 16)
    (y-coord uint 16)
    (anchor-point type (if (< 1 anchor-format)
                           (bindat-type uint 16)
                         (bindat-type unit nil)))
    (x-device-offset type (if (< 2 anchor-format)
                           (bindat-type uint 16)
                         (bindat-type unit nil)))
    (y-device-offset type (if (< 2 anchor-format)
                              (bindat-type uint 16)
                            (bindat-type unit nil))))
  "A spec for a GPOS anchor table.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#anchor-tables'")

(defvar fontsloth-otf--base-array-spec
  (bindat-type
    (base-offset unit bindat-idx)
    (base-count uint 16)
    (base-anchor-offsets vec base-count uint 16)
    (anchors vec base-count type (fontsloth-otf--offset-spec
                                  (+ base-offset
                                     (elt base-anchor-offsets bindat--i))
                                  fontsloth-otf--anchor-spec)))
  "A spec for a GPOS base array.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos#mark-to-base-attachment-positioning-format-1-mark-to-base-attachment-point'")

(defun fontsloth-otf--make-lookup-subtable-spec (lookup-type)
  "Given LOOKUP-TYPE, return a spec for a GSUB/GPOS lookup subtable.
See URL `https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lulTbl'
Currently only GPOS type 2 pair positioning adjustment and GPOS type 4
mark-to-base attachement are supported.
TODO: support more types"
  (cl-case lookup-type
    (2 fontsloth-otf--pair-pos-spec)
    (4 (bindat-type
         (offset-base unit bindat-idx)
         (format uint 16)
         (mark-coverage-offset uint 16)
         (base-coverage-offset uint 16)
         (class-count uint 16)
         (mark-array-offset uint 16)
         (base-array-offset uint 16)
         (mark-coverage type (fontsloth-otf--offset-spec
                              (+ offset-base mark-coverage-offset)
                              fontsloth-otf--coverage-spec))
         (base-coverage type (fontsloth-otf--offset-spec
                              (+ offset-base base-coverage-offset)
                              fontsloth-otf--coverage-spec))
         (mark-array type (fontsloth-otf--offset-spec
                           (+ offset-base mark-array-offset)
                           fontsloth-otf--mark-array-spec))
         (base-array type (fontsloth-otf--offset-spec
                           (+ offset-base base-array-offset)
                           fontsloth-otf--base-array-spec))))
    (t (bindat-type unit nil))))

(defvar fontsloth-otf--lookup-flag-spec
  (bindat-type
    :pack-var f
    (word uint 16 :pack-val f)           ; TODO pack properly
    :unpack-val
    `((right-to-left . ,(= 1 (logand 1 word)))
      (ignore-base-glyphs . ,(= 2 (logand 2 word)))
      (ignore-ligatures . ,(= 4 (logand 4 word)))
      (ignore-marks . ,(= 8 (logand 8 word)))
      (mark-attachment-type . ,(ash (logand #xff00 word) -8))))
  "A spec to unpack the GSUB/GPOS lookup flag.")

(defvar fontsloth-otf--lookup-table-spec
  (bindat-type
    (lookup-offset unit bindat-idx)
    (lookup-type uint 16)
    (flag type fontsloth-otf--lookup-flag-spec)
    (range-count uint 16)
    (range-offsets vec range-count uint 16)
    (subtables
     type
     (bindat-type
       vec range-count type
       (fontsloth-otf--offset-spec
        (+ lookup-offset
           (elt range-offsets bindat--i))
        (fontsloth-otf--make-lookup-subtable-spec lookup-type)))))
  "A spec for a GSUB/GPOS lookup table.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lulTbl'")

(defvar fontsloth-otf--lookup-list-spec
  (bindat-type
    (lookup-list-offset unit bindat-idx)
    (count uint 16)
    (lookup-offsets vec count type (bindat-type (offset uint 16)))
    (lookups vec count type
             (fontsloth-otf--offset-spec
              (+ lookup-list-offset
                 (alist-get 'offset
                            (elt lookup-offsets bindat--i)))
              fontsloth-otf--lookup-table-spec)))
  "A spec for a GPOS lookup-list.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/chapter2#lookup-list-table'")

(defvar fontsloth-otf--gpos-spec
  (bindat-type
    (table-offset unit bindat-idx)
    (major-version uint 16)
    (minor-version uint 16)
    (script-list-offset uint 16)
    (feature-list-offset uint 16)
    (lookup-list-offset uint 16)
    (variations-offset type (if (<= 1 minor-version)
                                (bindat-type uint 32)
                              (bindat-type unit nil)))
    (script-list
     type (fontsloth-otf--offset-spec
           (+ table-offset script-list-offset)
           fontsloth-otf--script-list-spec))
    (feature-list
     type (fontsloth-otf--offset-spec
           (+ table-offset feature-list-offset)
           fontsloth-otf--feature-list-spec))
    (lookup-list
     type (fontsloth-otf--offset-spec
           (+ table-offset lookup-list-offset)
           fontsloth-otf--lookup-list-spec)))
  "A spec for a GPOS table.
see URL `https://docs.microsoft.com/en-us/typography/opentype/spec/gpos'")

(defun fontsloth-otf-gpos-get-script-table (script)
  "Given SCRIPT try to find the corresponding GPOS script table.
If SCRIPT is nil it will be the default script."
  (when-let* ((script (or script "DFLT"))
              (script-list (fontsloth-otf--get-table-value 'script-list "GPOS"))
              (scripts (alist-get 'scripts script-list)))
    (seq-find (lambda (l) (equal script (alist-get 'tag l))) scripts)))

(defun fontsloth-otf-gpos-get-lang-sys-table (script lang)
  "Given SCRIPT and LANG try to find the corresponding GPOS lang sys table.
If SCRIPT is nil it will be the default script.
If LANG is nil it will be the default lang sys."
  (when-let ((script (fontsloth-otf-gpos-get-script-table script))
             (lang (or lang "DFLT")))
    (if (equal "DFLT" lang)
        (alist-get 'default-lang-sys script)
      (when-let ((langs (alist-get 'langs script)))
        (seq-find (lambda (l) (equal lang (alist-get 'tag l))) langs)))))

(defun fontsloth-otf-gpos-get-feature-table (script lang feature)
  "Given SCRIPT, LANG, and FEATURE try to find a GPOS feature table.
If SCRIPT is nil it will be the default script.
If LANG is nil it will be the default lang sys."
  (when-let* ((lang-sys (fontsloth-otf-gpos-get-lang-sys-table script lang))
              (feature-indices (alist-get 'feature-indices lang-sys))
              (feature-list (fontsloth-otf--get-table-value 'feature-list "GPOS"))
              (features (alist-get 'features feature-list))
              (feature-idx (seq-find (lambda (i)
                                       (when-let ((f (elt features i)))
                                         (equal feature (alist-get 'tag f))))
                                     feature-indices)))
    (elt features feature-idx)))

(defun fontsloth-otf-gpos-get-lookup-tables (script lang feature type)
  "Given SCRIPT and LANG find all FEATURE's GPOS lookup tables of TYPE.
If SCRIPT is nil it will be the default script.
If LANG is nil it will be the default lang sys."
  (when-let* ((feature (fontsloth-otf-gpos-get-feature-table script lang feature))
              (lookup-indices (alist-get 'lookup-indices feature))
              (lookup-list (fontsloth-otf--get-table-value 'lookup-list "GPOS"))
              (lookups (alist-get 'lookups lookup-list)))
    (cl-loop for i in (seq-filter (lambda (i)
                                    (when-let ((l (elt lookups i)))
                                      (equal type (alist-get 'lookup-type l))))
                                  lookup-indices)
             collect (elt lookups i))))

(defsubst fontsloth-otf-gpos-kerning-tables (script lang)
  "Find all GPOS \"kern\" feature lookup tables for SCRIPT and LANG.
If SCRIPT is nil it will be the default script.
If LANG is nil it will be the default lang sys."
  ;; TODO: enumerate the lookup type enumeration
  (fontsloth-otf-gpos-get-lookup-tables script lang "kern" 2))

(defun fontsloth-otf--bin-search (arr value)
  "Binary search (presumably ascendingly sorted) array ARR for VALUE.
Return nil if value is not found."
  (cl-loop with imin = 0
           with imax = (1- (length arr))
           for imid = (ash (+ imin imax) -1)
           then (progn (if (< val value)
                           (setq imin (1+ imid))
                         (setq imax (1- imid)))
                       (ash (+ imin imax) -1))
           while (<= imin imax)
           for val = (elt arr imid)
           when (= val value) return imid))

(defun fontsloth-otf--range-search (ranges value)
  "Search GPOS RANGES for VALUE and return a matching range or nil."
  (cl-loop with imin = 0
           with imax = (1- (length ranges))
           for imid = (ash (+ imin imax) -1)
           then (progn (if (< start value)
                           (setq imin (1+ imid))
                         (setq imax (1- imid)))
                       (ash (+ imin imax) -1))
           while (<= imin imax)
           for range = (elt ranges imid)
           for start = (alist-get 'start-glyph-id range)
           when (= start value) return range
           finally return (when (< 0 imin)
                            (let ((range (elt ranges (1- imin))))
                              (when (<= value (alist-get 'end-glyph-id range))
                                range)))))

(defun fontsloth-otf-gpos-get-coverage-index (coverage glyph-id)
  "Given a GPOS coverage table try to find a coverage index/range for GLYPH-ID.
Return nil if not found."
  (cl-case (alist-get 'format coverage)
    (1 (fontsloth-otf--bin-search (alist-get 'glyphs coverage) glyph-id))
    (2 (when-let ((range
                   (fontsloth-otf--range-search
                    (alist-get 'ranges coverage) glyph-id)))
         (- (+ (alist-get 'index range) glyph-id)
            (alist-get 'start-glyph-id range))))))

(defun fontsloth-otf-gpos-get-glyph-class (class-def glyph-id)
  "Given a GPOS class def table try to find a class for glyph-id.
Return class 0 if not found."
  (or (cl-case (alist-get 'format class-def)
        (1 (let ((start-glyph-id (alist-get 'start-glyph-id class-def)))
             (when (and (<= start-glyph-id glyph-id)
                        (< glyph-id (+ (alist-get 'glyph-count class-def)
                                       start-glyph-id)))
               (elt (alist-get 'class-value-array class-def)
                    (- glyph-id start-glyph-id)))))
        (2 (when-let ((range
                       (fontsloth-otf--range-search
                        (alist-get 'ranges class-def) glyph-id)))
             (alist-get 'index range))))
      0))

(defun fontsloth-otf-gpos-hkern (left-glyph-id right-glyph-id)
  "Try to kern a pair LEFT-GLYPH-ID/RIGHT-GLYPH-ID using default lang kerning.
Return a x-advance adjustment or 0."
  (or (when-let* ((kerning (fontsloth-otf-gpos-kerning-tables nil "DFLT")))
        (seq-some
         (lambda (k)
           (cl-loop
            for st across (alist-get 'subtables k)
            for coverage = (alist-get 'coverage st)
            for coverage-index = (fontsloth-otf-gpos-get-coverage-index
                                  coverage left-glyph-id)
            for value =
            (when coverage-index
              (cl-case (alist-get 'pair-pos-format st)
                (1 (let* ((pair-sets (alist-get 'pair-sets st))
                          (pair-set (elt pair-sets coverage-index))
                          (pairs (alist-get 'pair-values pair-set)))
                     (cl-loop for pair across pairs when
                              (= right-glyph-id
                                 (alist-get 'second-glyph-id pair))
                              return (or (map-nested-elt
                                          pair
                                          '(value-1 x-advance)) 0))))
                (2 (let* ((class1 (fontsloth-otf-gpos-get-glyph-class
                                   (alist-get 'class-def-1 st)
                                   left-glyph-id))
                          (class2 (fontsloth-otf-gpos-get-glyph-class
                                   (alist-get 'class-def-2 st)
                                   right-glyph-id))
                          (class-records (alist-get 'class-records st))
                          (pair (elt (elt class-records class1) class2)))
                     (or (map-nested-elt
                          pair
                          '(value-1 x-advance)) 0)))))
            when value return value))
         kerning))
      0))

(defun fontsloth-otf--gpos-map-pair-kerns (mappings left-is subtable)
  "Given left of pair LEFT-IS map from SUBTABLE pair sets into MAPPINGS."
  (let* ((pair-sets (alist-get 'pair-sets subtable)))
    (cl-loop
     for (left . idx) in left-is do
     (let* ((pair-set (elt pair-sets idx))
            (pairs (alist-get 'pair-values pair-set)))
       (cl-loop
        for p across pairs do
        (let* ((right (alist-get 'second-glyph-id p))
               (id (logior (ash left 16) right))
               (v (map-nested-elt p '(value-1 x-advance))))
          (when v
            (unless (map-contains-key mappings id)
              (puthash id v mappings)))))))))

(defun fontsloth-otf--gpos-map-class-kerns (mappings left-is subtable)
  "Given left of pair LEFT-IS map from SUBTABLE class 2 as right into MAPPINGS."
  (cl-loop
   for (left . _) in left-is do
   (let* ((c1 (fontsloth-otf-gpos-get-glyph-class
               (alist-get 'class-def-1 subtable) left))
          (class-records (alist-get 'class-records subtable))
          (c2s (elt class-records c1))
          ;; can it be format 1? that would be surprising
          (ranges (map-nested-elt subtable '(class-def-2 ranges))))
     (cl-loop
      for r across ranges do
      (cl-loop for right from (alist-get 'start-glyph-id r)
               to (alist-get 'end-glyph-id r)
               for id = (logior (ash left 16) right)
               for p = (elt c2s (alist-get 'index r))
               for v = (map-nested-elt p '(value-1 x-advance))
               when v do
               (unless (map-contains-key mappings id)
                 (puthash id v mappings)))))))

(defun fontsloth-otf--n-choose-k (n k)
  "Just an n choose k calc."
  (if (= 0 k) 1
    (/ (* n (fontsloth-otf--n-choose-k (1- n) (1- k))) k)))

(defun fontsloth-otf-gpos-build-kern-mappings ()
  "Try to index all active default lang kerning pairs into a flat map.
This is very expensive, but the result can be cached along with the font."
  ;; TODO: improve efficiency
  ;; the slowest portion is map-class-kerns
  ;; it may help to index glyph class defs
  ;; (and possibly then reverse index the covered glyphs by class)
  ;; map-pair-kerns should be O(num-pairs) which is the best given the task
  ;; map-class-kerns binary searches each left for its class
  (when-let* ((kerning (fontsloth-otf-gpos-kerning-tables nil "DFLT"))
              (num-glyphs (fontsloth-otf-num-glyphs))
              (size-estimate (fontsloth-otf--n-choose-k num-glyphs 2))
              (mappings (make-hash-table :test 'eq :size size-estimate)))
    (dolist (k kerning)
      (cl-loop for st across (alist-get 'subtables k)
               for coverage = (alist-get 'coverage st)
               for left-is =
               (cl-case (alist-get 'format coverage)
                 (1 (let ((glyphs (alist-get 'glyphs coverage)))
                      (cl-loop for gid across glyphs
                               for i from 0  collect `(,gid . ,i))))
                 (2 (let ((ranges (alist-get 'ranges coverage)))
                      (cl-loop for r across ranges append
                               (let ((sid (alist-get 'start-glyph-id r))
                                     (eid (alist-get 'end-glyph-id r))
                                     (idx (alist-get 'index r)))
                                 (cl-loop for gid from sid to eid collect
                                          `(,gid . ,(- (+ idx gid) sid))))))))
               do
               (cl-case (alist-get 'pair-pos-format st)
                 (1 (fontsloth-otf--gpos-map-pair-kerns mappings left-is st))
                 (2 (fontsloth-otf--gpos-map-class-kerns mappings left-is st)))))
    ;; there will be many zeros because of the way GPOS pair pos works
    ;; zeroes effectively mask later pairs that would otherwise be active
    (cl-loop for k being the hash-keys of mappings do
             (when (= 0 (gethash k mappings))
               (remhash k mappings)))
    (let* ((new-size (truncate (/ (map-length mappings) .8)))
           (result (make-hash-table :test 'eq :size new-size)))
      (cl-loop for k being the hash-keys of mappings do
               (puthash k (gethash k mappings) result))
      result)))

(provide 'fontsloth-otf-typo)
;;; fontsloth-otf-typo.el ends here
