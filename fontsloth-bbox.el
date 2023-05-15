;;; fontsloth-bbox.el --- Fns for fontsloth-bbox type -*- lexical-binding: t -*-

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

;; fontsloth-bbox.el (this file): Fns for fontsloth-bbox type

;;; Code:

(require 'cl-lib)

(cl-defstruct
    (fontsloth-bbox
     (:constructor fontsloth-bbox-create)
     (:copier nil))
  (xmin cl-most-positive-float :type 'number)
  (ymin cl-most-positive-float :type 'number)
  (xmax cl-most-negative-float :type 'number)
  (ymax cl-most-negative-float :type 'number))

(defun fontsloth-bbox-extend-by (bbox x y)
  "Extend the bbox by to include x,y.
BBOX the bbox to extend
X x coord
Y y coord"
  (setf (fontsloth-bbox-xmin bbox) (min x (fontsloth-bbox-xmin bbox)))
  (setf (fontsloth-bbox-ymin bbox) (min y (fontsloth-bbox-ymin bbox)))
  (setf (fontsloth-bbox-xmax bbox) (max x (fontsloth-bbox-xmax bbox)))
  (setf (fontsloth-bbox-ymax bbox) (max y (fontsloth-bbox-ymax bbox)))
  bbox)

(provide 'fontsloth-bbox)
;;; fontsloth-bbox.el ends here
