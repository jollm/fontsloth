;;; fontsloth-point.el --- Fns for fontsloth-point type -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
;; Homepage: https://github.com/jollm/fontsloth
;; Package-Requires: ((emacs "26.1"))
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

;; fontsloth-point.el (this file): Fns for fontsloth-point type

;;; Code:

(require 'cl-lib)

(cl-defstruct
    (fontsloth-point
     (:constructor fontsloth-point-create)
     (:copier nil))
  (x 0.0 :type 'number)
  (y 0.0 :type 'number))

(defun fontsloth-point-between (p1 p2 ratio)
  "Given two points construct a point at ratio * distance between them.
P1 first point
P2 second point
RATIO ratio of distance for the new point, between 0.0 and 1.0"
  (fontsloth-point-create
   :x (+ (fontsloth-point-x p1)
         (* ratio (- (fontsloth-point-x p2) (fontsloth-point-x p1))))
   :y (+ (fontsloth-point-y p1)
         (* ratio (- (fontsloth-point-y p2) (fontsloth-point-y p1))))))

(defun fontsloth-point-scale (p scale)
  (setf (fontsloth-point-x p) (* scale (fontsloth-point-x p))
        (fontsloth-point-y p) (* scale (fontsloth-point-y p))))

(defun fontsloth-point-distance-squared (p1 p2)
  (let ((dx (- (fontsloth-point-x p1) (fontsloth-point-x p2)))
        (dy (- (fontsloth-point-y p1) (fontsloth-point-y p2))))
    (+ (* dx dx) (* dy dy))))

(provide 'fontsloth-point)
;;; fontsloth-point.el ends here
