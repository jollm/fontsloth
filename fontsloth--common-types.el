;;; fontsloth--common-types.el --- Useful types -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.14.1
;; Package-Requires: ((cl-lib "1.0") (names "20151201.0") (emacs "26.1"))
;; Keywords: true-type, font, ttf, otf

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

;; fontsloth--common-types (this file): Broadly useful types

;;; Code:

(require 'cl-lib)

(cl-float-limits)

(cl-defstruct
    (fontsloth-point
     (:constructor fontsloth-point-create)
     (:copier nil))
  (x 0.0 :type 'number)
  (y 0.0 :type 'number))

(cl-defstruct
    (fontsloth-coords
     (:constructor fontsloth-coords-create)
     (:copier nil)
     (:type list))
  x0 y0 x1 y1)

(cl-defstruct
    (fontsloth-nudge
     (:constructor fontsloth-nudge-create)
     (:copier nil)
     (:type list))
  start-x-nudge start-y-nudge end-x-nudge end-y-nudge)

(cl-defstruct
    (fontsloth-adj
     (:constructor fontsloth-adj-create)
     (:copier nil)
     (:type list))
  x-first-adj y-first-adj)

(cl-defstruct
    (fontsloth-params
     (:constructor fontsloth-params-create)
     (:copier nil)
     (:type list))
  tdx tdy dx dy)

(cl-defstruct
    (fontsloth-line
     (:constructor fontsloth-make-line)
     (:copier nil))
  (coords nil :type 'fontsloth-coords)
  (nudge nil :type 'fontsloth-nudge)
  (adjustment nil :type 'fontsloth-adj)
  (params nil :type 'fontsloth-params))

(cl-defstruct
    (fontsloth-bbox
     (:constructor fontsloth-bbox-create)
     (:copier nil))
  (xmin cl-most-positive-float :type 'number)
  (ymin cl-most-positive-float :type 'number)
  (xmax cl-most-negative-float :type 'number)
  (ymax cl-most-negative-float :type 'number))

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

(defun fontsloth--line-create (start end)
  "Construct a new line from `start' point and `end' point.
START the start point of type `fontsloth-point'
END the end point of type `fontsloth-point'"
  (let* ((floor-nudge 0)
         (ceil-nudge 0.0000000000001))
    (cl-multiple-value-bind (x-start-nudge
                             x-first-adj
                             y-start-nudge
                             y-first-adj
                             x-end-nudge y-end-nudge)
        (append (if (>= (fontsloth-point-x end) (fontsloth-point-x start))
                    `(,floor-nudge 1.0)
                  `(,ceil-nudge 0.0))
                (if (>= (fontsloth-point-y end) (fontsloth-point-y start))
                    `(,floor-nudge 1.0)
                  `(,ceil-nudge 0.0))
                (if (> (fontsloth-point-x end) (fontsloth-point-x start))
                    `(,ceil-nudge)
                  `(,floor-nudge))
                (if (> (fontsloth-point-y end) (fontsloth-point-y start))
                    `(,ceil-nudge)
                  `(,floor-nudge)))
      (let* ((dx (- (fontsloth-point-x end) (fontsloth-point-x start)))
             (dy (- (fontsloth-point-y end) (fontsloth-point-y start)))
             (tdx (if (eql 0.0 dx) cl-most-positive-float (/ 1.0 dx)))
             (tdy (/ 1.0 dy)))
        (fontsloth-make-line
         :coords (fontsloth-coords-create
                  :x0 (fontsloth-point-x start) :y0 (fontsloth-point-y start)
                  :x1 (fontsloth-point-x end) :y1 (fontsloth-point-y end))
         :nudge (fontsloth-nudge-create :start-x-nudge x-start-nudge
                                        :start-y-nudge y-start-nudge
                                        :end-x-nudge x-end-nudge
                                        :end-y-nudge y-end-nudge)
         :adjustment (fontsloth-adj-create :x-first-adj x-first-adj
                                           :y-first-adj y-first-adj)
         :params (fontsloth-params-create :tdx tdx :tdy tdy :dx dx :dy dy))))))

(defun fontsloth--reverse-coords (coords)
  (fontsloth-coords-create :x0 (fontsloth-coords-x1 coords)
                           :y0 (fontsloth-coords-y1 coords)
                           :x1 (fontsloth-coords-x0 coords)
                           :y1 (fontsloth-coords-y0 coords)))

(defun fontsloth--reposition-line (line bounds reverse)
  (let* ((coords (if reverse (fontsloth--reverse-coords
                              (fontsloth-line-coords line))
                   (fontsloth-line-coords line)))
         (x0 (- (fontsloth-coords-x0 coords) (fontsloth-bbox-xmin bounds)))
         (y0 (abs (- (fontsloth-coords-y0 coords)
                     (fontsloth-bbox-ymax bounds))))
         (x1 (- (fontsloth-coords-x1 coords) (fontsloth-bbox-xmin bounds)))
         (y1 (abs (- (fontsloth-coords-y1 coords)
                     (fontsloth-bbox-ymax bounds)))))
    (fontsloth--line-create (fontsloth-point-create :x x0 :y y0)
                            (fontsloth-point-create :x x1 :y y1))))

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

(provide 'fontsloth--common-types)
;;; fontsloth--common-types.el ends here
