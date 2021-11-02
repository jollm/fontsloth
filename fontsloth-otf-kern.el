;;; fontsloth-otf-kern.el --- Kern table portion of an Elisp otf/ttf bindat parser -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
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

;; fontsloth-otf-typo (this file): uses bindat to parse kern table portion of
;; ttf files


;;; Code:

(require 'bindat)
(require 'cl-lib)

(defun fontsloth-otf-kern--format0-mappings (pairs)
  "Index TTF kern table format0 PAIRS."
  (let ((mappings (make-hash-table :test 'eq)))
    (cl-loop for p across pairs do
      (puthash (alist-get 'id p) (alist-get 'value p) mappings))
    mappings))

(defun fontsloth-otf-kern--format3-mappings
    (glyph-count
     left-hand-classes right-hand-classes
     left-hand-classes-count right-hand-classes-count
     indices kerning-values)
  "Index TTF kern table format3 classes.
GLYPH-COUNT number of glyphs
LEFT-HAND-CLASSES sequence of left glyph classes
RIGHT-HAND-CLASSES sequence of right glyph classes
LEFT-HAND-CLASSES-COUNT number of left glyph classes
RIGHT-HAND-CLASSES-COUNT number of right glyph classes
INDICES kerning indices by class pair
KERNING-VALUES the actual kerning values"
  (let ((mappings (make-hash-table :test 'eq)))
    (dotimes (left glyph-count)
      (dotimes (right glyph-count)
        (let ((left-class
               (elt left-hand-classes left))
              (right-class
               (elt right-hand-classes right)))
          (unless (and (< left-class
                          left-hand-classes-count)
                       (< right-class
                          right-hand-classes-count))
            (let* ((index (+ right-class
                             (* left-class
                                right-hand-classes-count)))
                   (index (elt indices index))
                   (id (logior (ash left 16) right))
                   (value (elt kerning-values index)))
              (puthash id value mappings))))))
    mappings))

(defvar fontsloth-otf-kern--format0-spec
  (bindat-type
    (num-pairs uint 16)
    (_ fill 6)
    (pairs vec num-pairs type
           (bindat-type
             (left uint 16)
             (right uint 16)
             (id unit (logior (ash left 16) right))
             (value sint 16 nil)))
    (mappings unit (fontsloth-otf-kern--format0-mappings pairs)))
  "A spec for kern table format0 pairwise kerning.
see URL `https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html'")

(defvar fontsloth-otf-kern--format3-spec
  (bindat-type
    (glyph-count uint 16)
    (kerning-values-count uint 8)
    (left-hand-classes-count uint 8)
    (right-hand-classes-count uint 8)
    (_ fill 1)
    (indices-count unit (* left-hand-classes-count
                           right-hand-classes-count))
    (kerning-values vec kerning-values-count
                    sint 16 nil)
    (left-hand-classes vec glyph-count uint 8)
    (right-hand-classes vec glyph-count uint 8)
    (indices vec indices-count uint 8)
    (mappings
     unit
     (fontsloth-otf-kern--format3-mappings
      glyph-count
      left-hand-classes right-hand-classes
      left-hand-classes-count
      right-hand-classes-count
      indices kerning-values)))
  "A spec for kern table format3 class based kerning.
see URL `https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html'")

(defvar fontsloth-otf-kern-spec
  (bindat-type
    (major-version uint 16)
    (minor-version type
                   (if (= 0 major-version)
                       (bindat-type unit nil)
                     (bindat-type uint 16)))
    (num-sub-tables type
                    (if (= 0 major-version)
                        (bindat-type uint 16)
                      (bindat-type uint 32)))
    (sub-tables type
                (if (= 0 major-version)
                    (bindat-type vec num-sub-tables type
                                 (bindat-type
                                   (version uint 16)
                                   (length uint 16)
                                   (format uint 8)
                                   (coverage uint 8)
                                   (tuple-index unit 0)))
                  (bindat-type vec num-sub-tables type
                               (bindat-type
                                 (length uint 32)
                                 (coverage uint 8)
                                 (format uint 8)
                                 (tuple-index uint 16)))))
    ;; only read the first subtable for now
    (_ type
       (cl-case (alist-get 'format (elt sub-tables 0))
         (0 fontsloth-otf-kern--format0-spec)
         (3 fontsloth-otf-kern--format3-spec)
         (t (bindat-type (mappings unit nil))))))
  "A spec for a TTF kern table.
see URL `https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html'")

(provide 'fontsloth-otf-kern)
;;; fontsloth-otf-kern.el ends here
