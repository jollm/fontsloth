;;; fontsloth-glyph.el --- Fns for fontsloth-glyph type -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
;; Homepage: https://github.com/jollm/fontsloth
;; Package-Requires: ((emacs "27.1"))
;; Keywords: data, font, ttf, otf

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

;; fontsloth-glyph.el (this file): Fns for fontsloth-glyph type

;;; Code:

(declare-function fontsloth-glyph-outline-bounds-create "fontsloth-geometry")
(declare-function fontsloth-glyph-outline-bounds-xmin "fontsloth-geometry")
(declare-function fontsloth-glyph-outline-bounds-ymin "fontsloth-geometry")
(declare-function fontsloth-glyph-outline-bounds-width "fontsloth-geometry")
(declare-function fontsloth-glyph-outline-bounds-height "fontsloth-geometry")

(declare-function fontsloth-geometry-create "fontsloth-geometry")
(declare-function fontsloth-geometry-finalize "fontsloth-geometry")

(declare-function fontsloth-otf-outline-glyph "fontsloth-otf")
(declare-function fontsloth-otf-glyph-hor-advance "fontsloth-otf")

(defun fontsloth-glyph-outline-bounds-scale (bounds scale)
  "Non-destructively scale an outline bounds by a scale factor.
returns a new object
BOUNDS the bounds to scale
SCALE the scale factor"
  (fontsloth-glyph-outline-bounds-create
   :xmin (* scale (fontsloth-glyph-outline-bounds-xmin bounds))
   :ymin (* scale (fontsloth-glyph-outline-bounds-ymin bounds))
   :width (* scale (fontsloth-glyph-outline-bounds-width bounds))
   :height (* scale (fontsloth-glyph-outline-bounds-height bounds))))

(defun fontsloth-glyph-create (glyph-id scale units-per-em)
  "Construct a glyph given its id and desired units-per-em.
GLYPH-ID the id
SCALE the geometry scale
UNITS-PER-EM the units per em"
  (let ((geom (fontsloth-geometry-create scale units-per-em)))
    (fontsloth-otf-outline-glyph glyph-id geom)
    (fontsloth-geometry-finalize
     geom (fontsloth-otf-glyph-hor-advance glyph-id) 0.0)))

(provide 'fontsloth-glyph)
;;; fontsloth-glyph.el ends here
