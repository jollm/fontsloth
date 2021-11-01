;;; fontsloth-line.el --- Fns for fontsloth-line type -*- lexical-binding: t -*-

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

;; fontsloth-line.el (this file): Fns for fontsloth-line type

;;; Code:

(require 'cl-lib)

(require 'fontsloth-coords)

(declare-function fontsloth-point-create "fontsloth--common-types")
(declare-function fontsloth-point-x "fontsloth--common-types")
(declare-function fontsloth-point-y "fontsloth--common-types")

(declare-function fontsloth-make-line "fontsloth--common-types")
(declare-function fontsloth-line-coords "fontsloth--common-types")

(declare-function fontsloth-nudge-create "fontsloth--common-types")
(declare-function fontsloth-adj-create "fontsloth--common-types")
(declare-function fontsloth-params-create "fontsloth--common-types")

(declare-function fontsloth-bbox-xmin "fontsloth--common-types")
(declare-function fontsloth-bbox-ymax "fontsloth--common-types")

(declare-function fontsloth-coords-create "fontsloth--common-types")
(declare-function fontsloth-coords-x0 "fontsloth--common-types")
(declare-function fontsloth-coords-y0 "fontsloth--common-types")
(declare-function fontsloth-coords-x1 "fontsloth--common-types")
(declare-function fontsloth-coords-y1 "fontsloth--common-types")

(defun fontsloth-line-create (start end)
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

(defun fontsloth-line-reposition (line bounds reverse)
  "Reposition LINE given BOUNDS and REVERSE."
  (let* ((coords (if reverse (fontsloth-coords-reverse
                              (fontsloth-line-coords line))
                   (fontsloth-line-coords line)))
         (x0 (- (fontsloth-coords-x0 coords) (fontsloth-bbox-xmin bounds)))
         (y0 (abs (- (fontsloth-coords-y0 coords)
                     (fontsloth-bbox-ymax bounds))))
         (x1 (- (fontsloth-coords-x1 coords) (fontsloth-bbox-xmin bounds)))
         (y1 (abs (- (fontsloth-coords-y1 coords)
                     (fontsloth-bbox-ymax bounds)))))
    (fontsloth-line-create (fontsloth-point-create :x x0 :y y0)
                           (fontsloth-point-create :x x1 :y y1))))

(provide 'fontsloth-line)
;;; fontsloth-line.el ends here
