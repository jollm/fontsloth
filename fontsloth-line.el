;;; fontsloth-line.el --- Fns for fontsloth-line type -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.21.0
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
  "A line with all raster/finalize fields inline."
  (x0 0.0 :type 'number)
  (y0 0.0 :type 'number)
  (x1 0.0 :type 'number)
  (y1 0.0 :type 'number)
  (start-x-nudge 0.0 :type 'number)
  (start-y-nudge 0.0 :type 'number)
  (end-x-nudge 0.0 :type 'number)
  (end-y-nudge 0.0 :type 'number)
  (x-first-adj 0.0 :type 'number)
  (y-first-adj 0.0 :type 'number)
  (tdx 0.0 :type 'number)
  (tdy 0.0 :type 'number)
  (dx 0.0 :type 'number)
  (dy 0.0 :type 'number))

;; Compatibility views for callers that still want the old sub-structs.
;; These allocate; the raster and geometry hot paths do not use them.
(defun fontsloth-line-coords (line)
  "Return LINE's coordinates as a `fontsloth-coords'."
  (fontsloth-coords-create :x0 (fontsloth-line-x0 line)
                           :y0 (fontsloth-line-y0 line)
                           :x1 (fontsloth-line-x1 line)
                           :y1 (fontsloth-line-y1 line)))

(defun fontsloth-line-nudge (line)
  "Return LINE's nudges as a `fontsloth-nudge'."
  (fontsloth-nudge-create
   :start-x-nudge (fontsloth-line-start-x-nudge line)
   :start-y-nudge (fontsloth-line-start-y-nudge line)
   :end-x-nudge (fontsloth-line-end-x-nudge line)
   :end-y-nudge (fontsloth-line-end-y-nudge line)))

(defun fontsloth-line-adjustment (line)
  "Return LINE's first-point adjustment as a `fontsloth-adj'."
  (fontsloth-adj-create :x-first-adj (fontsloth-line-x-first-adj line)
                        :y-first-adj (fontsloth-line-y-first-adj line)))

(defun fontsloth-line-params (line)
  "Return LINE's inverse parameters as a `fontsloth-params'."
  (fontsloth-params-create :tdx (fontsloth-line-tdx line)
                           :tdy (fontsloth-line-tdy line)
                           :dx (fontsloth-line-dx line)
                           :dy (fontsloth-line-dy line)))

(defun fontsloth-line--create-adj (x0 y0 x1 y1 &optional line)
  "Create a `fontsloth-line' with START, X0,Y0, and X1,Y1.
With optional LINE, adjust LINE to match start and end points."
  (let* ((floor-nudge 0) (ceil-nudge 0.0000000000001))
    (let ((x-start-nudge) (x-first-adj) (y-start-nudge) (y-first-adj)
          (x-end-nudge) (y-end-nudge))
      (if (>= x1 x0)
          (setq x-start-nudge floor-nudge
                x-first-adj 1.0)
        (setq x-start-nudge ceil-nudge
              x-first-adj 0.0))
      (if (>= y1 y0)
          (setq y-start-nudge floor-nudge
                y-first-adj 1.0)
        (setq y-start-nudge ceil-nudge
              y-first-adj 0.0))
      (if (> x1 x0)
          (setq x-end-nudge ceil-nudge)
        (setq x-end-nudge floor-nudge))
      (if (> y1 y0)
          (setq y-end-nudge ceil-nudge)
        (setq y-end-nudge floor-nudge))
      (let* ((dx (- x1 x0))
             (dy (- y1 y0))
             (tdx (if (eql 0.0 dx) cl-most-positive-float (/ 1.0 dx)))
             (tdy (/ 1.0 dy)))
        (if line
            (setf (fontsloth-line-x0 line) x0
                  (fontsloth-line-y0 line) y0
                  (fontsloth-line-x1 line) x1
                  (fontsloth-line-y1 line) y1
                  (fontsloth-line-start-x-nudge line) x-start-nudge
                  (fontsloth-line-start-y-nudge line) y-start-nudge
                  (fontsloth-line-end-x-nudge line) x-end-nudge
                  (fontsloth-line-end-y-nudge line) y-end-nudge
                  (fontsloth-line-x-first-adj line) x-first-adj
                  (fontsloth-line-y-first-adj line) y-first-adj
                  (fontsloth-line-tdx line) tdx
                  (fontsloth-line-tdy line) tdy
                  (fontsloth-line-dx line) dx
                  (fontsloth-line-dy line) dy)
          (fontsloth-make-line
           :x0 x0 :y0 y0 :x1 x1 :y1 y1
           :start-x-nudge x-start-nudge
           :start-y-nudge y-start-nudge
           :end-x-nudge x-end-nudge
           :end-y-nudge y-end-nudge
           :x-first-adj x-first-adj
           :y-first-adj y-first-adj
           :tdx tdx :tdy tdy :dx dx :dy dy))))))

(defsubst fontsloth-line-create (start end)
  "Construct a new line from `start' point and `end' point.
START the start point of type `fontsloth-point'
END the end point of type `fontsloth-point'"
  (fontsloth-line--create-adj (fontsloth-point-x start)
                              (fontsloth-point-y start)
                              (fontsloth-point-x end)
                              (fontsloth-point-y end)))

(defun fontsloth-line-reposition (line bounds reverse)
  "Reposition LINE given BOUNDS and REVERSE."
  (let ((x0 (fontsloth-line-x0 line))
        (y0 (fontsloth-line-y0 line))
        (x1 (fontsloth-line-x1 line))
        (y1 (fontsloth-line-y1 line)))
    (when reverse
      (cl-psetf x0 x1 x1 x0
                y0 y1 y1 y0))
    (setq x0 (- x0 (fontsloth-bbox-xmin bounds))
          y0 (abs (- y0 (fontsloth-bbox-ymax bounds)))
          x1 (- x1 (fontsloth-bbox-xmin bounds))
          y1 (abs (- y1 (fontsloth-bbox-ymax bounds))))
    (fontsloth-line--create-adj x0 y0 x1 y1 line)
    line))

(provide 'fontsloth-line)
;;; fontsloth-line.el ends here
