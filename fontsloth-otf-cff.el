;;; fontsloth-otf-cff.el --- Outlining glyphs using cff -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
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

;; fontsloth-otf-cff (this file): Glyph outline implementation that relies on
;; the Adobe OTF CFF 1/2 table

;;; Code:

(require 'bindat)
(require 'cl-lib)

(defvar fontsloth-otf-cff--index-spec
  (bindat-type
    (index-start unit bindat-idx)
    (count uint 16)
    (offset-size uint 8)
    (offset-array-start unit bindat-idx)
    (offsets-len unit (* (1+ count) offset-size))
    (offsets vec offsets-len uint 8)))

(defun fontsloth-otf-cff--with-offset-fn (o rewind s)
  "Offset spec S by O and go back to start if REWIND is t (the fn version).

This function is intended to be used within macros."
  (let ((start (cl-gensym "start")))
    `(let ((,start))
       (bindat-type
         (_ unit (progn (setq ,start bindat-idx bindat-idx ,o) nil))
         (_ type  ,s)
         (_ unit (progn (when ,rewind (setq bindat-idx ,start)) nil))))))

(defmacro fontsloth-otf-cff--with-offset (o rewind s)
  "Offset spec S by O and go back to start if REWIND is t (the macro version)."
  (fontsloth-otf-cff--with-offset-fn o rewind s))

(defmacro fontsloth-otf-cff--make-dict-spec (s)
  "Let S read dictionary data in a lexical context of the dict index.

This assumes that bindat-idx is already positioned at the index start."
  `(bindat-type
     (index type fontsloth-otf-cff--index-spec)
     (_ type (pcase-let (((map offset-array-start count
                               offset-size offsets-len offsets) index))
               ,(fontsloth-otf-cff--with-offset-fn
                 '(+ offset-array-start offsets-len) nil s)))))

(defvar fontsloth-otf-cff--name-dict-spec
  (fontsloth-otf-cff--make-dict-spec
   (bindat-type
     (names vec count str (- (elt offsets (1+ bindat--i))
                             (elt offsets bindat--i)))
     (names-end unit bindat-idx))))

(defvar fontsloth-otf-cff--top-dict-spec
  (fontsloth-otf-cff--make-dict-spec
   (bindat-type
     (data vec count type
           (bindat-type vec (- (elt offsets (1+ bindat--i))
                               (elt offsets bindat--i)) uint 8))
     (top-dict-end unit bindat-idx))))

(defvar fontsloth-otf-cff--spec
  (bindat-type
    (major-version uint 8)
    (minor-version uint 8)
    (header-size uint 8)
    (abs-offset uint 8)
    (_ type (cl-case major-version
              (1 (bindat-type
                   (_ fill (- header-size 4))
                   (name-dict type fontsloth-otf-cff--name-dict-spec)
                   (top-dict type fontsloth-otf-cff--top-dict-spec)))))))


(provide 'fontsloth-otf-cff)
;;; fontsloth-otf-cff.el ends here
