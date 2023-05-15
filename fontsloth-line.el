;;; fontsloth-line.el --- Fns for fontsloth-line type -*- lexical-binding: t -*-

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

;; fontsloth-line.el (this file): Fns for fontsloth-line type

;;; Code:

(require 'cl-lib)

(require 'fontsloth-bbox)
(require 'fontsloth-coords)
(require 'fontsloth-point)

(cl-defstruct
    (fontsloth-nudge
     (:constructor fontsloth-nudge-create)
     (:copier nil)
     (:type vector))
  start-x-nudge start-y-nudge end-x-nudge end-y-nudge)

(cl-defstruct
    (fontsloth-adj
     (:constructor fontsloth-adj-create)
     (:copier nil)
     (:type vector))
  x-first-adj y-first-adj)

(cl-defstruct
    (fontsloth-params
     (:constructor fontsloth-params-create)
     (:copier nil)
     (:type vector))
  tdx tdy dx dy)

(cl-defstruct
    (fontsloth-line
     (:constructor fontsloth-make-line)
     (:copier nil))
  (coords nil :type 'fontsloth-coords)
  (nudge nil :type 'fontsloth-nudge)
  (adjustment nil :type 'fontsloth-adj)
  (params nil :type 'fontsloth-params))

(defun fontsloth-line--create (start end)
  (let* ((floor-nudge 0) (ceil-nudge 0.0000000000001))
    (let ((x-start-nudge) (x-first-adj) (y-start-nudge) (y-first-adj)
          (x-end-nudge) (y-end-nudge))
      (if (>= (fontsloth-point-x end) (fontsloth-point-x start))
          (setq x-start-nudge floor-nudge
                x-first-adj 1.0)
        (setq x-start-nudge ceil-nudge
              x-first-adj 0.0))
      (if (>= (fontsloth-point-y end) (fontsloth-point-y start))
          (setq y-start-nudge floor-nudge
                y-first-adj 1.0)
        (setq y-start-nudge ceil-nudge
              y-first-adj 0.0))
      (if (> (fontsloth-point-x end) (fontsloth-point-x start))
          (setq x-end-nudge ceil-nudge)
        (setq x-end-nudge floor-nudge))
      (if (> (fontsloth-point-y end) (fontsloth-point-y start))
          (setq y-end-nudge ceil-nudge)
        (setq y-end-nudge floor-nudge))
      (let* ((dx (- (fontsloth-point-x end) (fontsloth-point-x start)))
             (dy (- (fontsloth-point-y end) (fontsloth-point-y start)))
             (tdx (if (eql 0.0 dx) cl-most-positive-float (/ 1.0 dx)))
             (tdy (/ 1.0 dy)))
        `(,(fontsloth-coords-create
            :x0 (fontsloth-point-x start) :y0 (fontsloth-point-y start)
            :x1 (fontsloth-point-x end) :y1 (fontsloth-point-y end))
          ,(fontsloth-nudge-create :start-x-nudge x-start-nudge
                                   :start-y-nudge y-start-nudge
                                   :end-x-nudge x-end-nudge
                                   :end-y-nudge y-end-nudge)
          ,(fontsloth-adj-create :x-first-adj x-first-adj
                                 :y-first-adj y-first-adj)
          ,(fontsloth-params-create :tdx tdx :tdy tdy :dx dx :dy dy))))))

(defsubst fontsloth-line-create (start end)
  "Construct a new line from `start' point and `end' point.
START the start point of type `fontsloth-point'
END the end point of type `fontsloth-point'"
  (cl-multiple-value-bind (c n a p)
      (fontsloth-line--create start end)
    (fontsloth-make-line :coords c :nudge n :adjustment a :params p)))

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
    (cl-multiple-value-bind (c n a p)
        (fontsloth-line--create (fontsloth-point-create :x x0 :y y0)
                                (fontsloth-point-create :x x1 :y y1))
      (setf (fontsloth-line-coords line) c
            (fontsloth-line-nudge line) n
            (fontsloth-line-adjustment line) a
            (fontsloth-line-params line) p)
      line)))

(provide 'fontsloth-line)
;;; fontsloth-line.el ends here
