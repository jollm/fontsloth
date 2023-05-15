;;; fontsloth-coords.el --- Fns for fontsloth-coords type -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.17.0
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

;; Part of fontsloth

;; fontsloth-coords.el (this file): Fns for fontsloth-coords type

;;; Code:

(require 'cl-lib)

(cl-defstruct
    (fontsloth-coords
     (:constructor fontsloth-coords-create)
     (:copier nil)
     (:type vector))
  x0 y0 x1 y1)

(defun fontsloth-coords-reverse (coords)
  "Reverse a `fontsloth-coords' COORDS."
  (let ((x0 (fontsloth-coords-x1 coords))
        (y0 (fontsloth-coords-y1 coords))
        (x1 (fontsloth-coords-x0 coords))
        (y1 (fontsloth-coords-y0 coords)))
    (aset coords 0 x0)
    (aset coords 1 y0)
    (aset coords 2 x1)
    (aset coords 3 y1))
  coords)

(provide 'fontsloth-coords)
;;; fontsloth-coords.el ends here
