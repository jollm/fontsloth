;;; fontsloth-geometry.el --- Implements otf outliner generics -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.12.0
;; Package-Requires: ((cl-lib "1.0") (emacs "26.1"))
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

;; fontsloth-geometry.el:
;; This provides one implementation of otf outliner generics in fontsloth-otf.
;; It is modeled after fontdue's implementation of the ttf-parser outliner
;; methods.

;;; Code:

(require 'cl-lib)
(require 'fontsloth--common-types)
(require 'fontsloth-otf)

(cl-defstruct
    (fontsloth-quad-curve
     (:constructor fontsloth-quad-curve-create)
     (:copier nil))
  "Describes a bezier curve with one control point."
  (a nil :type 'fontsloth-point :documentation "point a")
  (b nil :type 'fontsloth-point :documentation "control point")
  (c nil :type 'fontsloth-point :documentation "point b"))

(cl-defstruct
    (fontsloth-geometry
     (:constructor fontsloth-make-geometry)
     (:copier nil))
  "Describes the geometry of a glyph outline.
To be handed to `fontsloth-otf-outline-glyph'"
  (v-lines nil :type 'vector
           :documentation "lines which vary in y but not in x")
  (m-lines nil :type 'vector :documentation "lines which are not v-lines")
  (effective-bounds (fontsloth-bbox-create) :type 'fontsloth-bbox
                    :documentation "the calculated glyph bounds")
  (start-point (fontsloth-point-create) :type 'fontsloth-point
               :documentation "the first point")
  (previous-point (fontsloth-point-create) :type 'fontsloth-point
                  :documentation "the previous point")
  (area 0.0 :type 'number :documentation "the calculated area")
  (reverse-points nil :type 'boolean
                  :documentation "t if reversing points during finalization")
  (max-area nil :type 'number :documentation "a bounds for the glyph area"))

(cl-defstruct
    (fontsloth-segment
     (:constructor fontsloth-segment-create)
     (:copier nil))
  "Describes a quad curve segment"
  (a nil :type 'fontsloth-point :documentation "the start point")
  (at nil :type 'number
      :documentation "the start point's position in the curve")
  (c nil :type 'fontsloth-point :documentation "the end point")
  (ct nil :type 'number
      :documentation "the end point's position in the curve"))

(cl-defstruct
    (fontsloth-glyph-outline-bounds
     (:constructor fontsloth-glyph-outline-bounds-create)
     (:copier nil))
  "Describes a glyph's calculated outline bounds."
  (xmin 0.0 :type 'number :documentation "least glyph x")
  (ymin 0.0 :type 'number :documentation "least glyph y")
  (width 0.0 :type 'number :documentation "calculated width")
  (height 0.0 :type 'number :documentation "calculated height"))

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

(cl-defstruct
    (fontsloth-glyph
     (:constructor nil)
     (:copier nil))
  "Describes a glyph in its bounded geometry."
  (v-lines nil :type 'vector
           :documentation "lines which vary in y but not in x")
  (m-lines nil :type 'vector  :documentation "lines which are not v-lines")
  (advance-width 0.0 :type 'number :documentation "advance width as specified")
  (advance-height 0.0 :type 'number
                  :documentation "advance height as specified")
  (bounds (fontsloth-glyph-outline-bounds-create)
          :type 'fontsloth-glyph-outline-bounds
          :documentation "calculated outline bounds"))

(defun fontsloth--quad-curve-point (qc time)
  "Determine the point along a curve at a given time.
QC the curve
TIME the time"
  (let* ((tm (- 1.0 time))
         (a (* tm tm))
         (b (* 2.0 tm time))
         (c (* time time))
         (x (+ (* a (fontsloth-point-x (fontsloth-quad-curve-a qc)))
               (* b (fontsloth-point-x (fontsloth-quad-curve-b qc)))
               (* c (fontsloth-point-x (fontsloth-quad-curve-c qc)))))
         (y (+ (* a (fontsloth-point-y (fontsloth-quad-curve-a qc)))
               (* b (fontsloth-point-y (fontsloth-quad-curve-b qc)))
               (* c (fontsloth-point-y (fontsloth-quad-curve-c qc))))))
    (fontsloth-point-create :x x :y y)))

(defun fontsloth-geometry-create (scale units-per-em)
  "Construct a new geometry given `scale' and `units-per-em'.
SCALE a number indicating the font scale for drawing
UNITS-PER-EM a number indicating units per em"
  (let* ((error-threshold-px 3.0)
         (max-area (* 2.0 error-threshold-px (/ units-per-em scale))))
    (fontsloth-make-geometry :max-area max-area)))

(defun fontsloth--geometry-push (geom start end)
  "Push a new line into the outline geometry.
GEOM a `fontsloth-geometry'
START the line start point
END the line end point"
  (when (not (eql (fontsloth-point-y start) (fontsloth-point-y end)))
    (setf (fontsloth-geometry-area geom)
          (+ (fontsloth-geometry-area geom)
             (* (- (fontsloth-point-y end) (fontsloth-point-y start))
                (+ (fontsloth-point-x end) (fontsloth-point-x start)))))
    (if (eql (fontsloth-point-x start) (fontsloth-point-x end))
        (setf (fontsloth-geometry-v-lines geom)
              (vconcat (fontsloth-geometry-v-lines geom)
                       `(,(fontsloth--line-create start end))))
      (setf (fontsloth-geometry-m-lines geom)
            (vconcat (fontsloth-geometry-m-lines geom)
                     `(,(fontsloth--line-create start end)))))
    (fontsloth-bbox-extend-by (fontsloth-geometry-effective-bounds geom)
                              (fontsloth-point-x start)
                              (fontsloth-point-y start))
    (fontsloth-bbox-extend-by (fontsloth-geometry-effective-bounds geom)
                              (fontsloth-point-x end)
                              (fontsloth-point-y end))))

(defun fontsloth-geometry-finalize (geom advance-width advance-height)
  "Finalize the outline geometry into a new `fontsloth-glyph'.
Lines are repositioned according to calculated effective bounds, which are then
given as the glyph's outline bounds
GEOM the geometry to finalize
ADVANCE-WIDTH glyph's advance width
ADVANCE-HEIGHT glyph's advance height"
  (let ((ebounds (if (or (fontsloth-geometry-v-lines geom)
                         (fontsloth-geometry-m-lines geom))
                     (fontsloth-geometry-effective-bounds geom)
                   (fontsloth-bbox-create
                    :xmin 0.0 :ymin 0.0 :xmax 0.0 :ymax 0.0))))
    (cl-flet ((reposition-lines (lines)
                (cl-loop for ln across lines
                         collect
                         (fontsloth--reposition-line
                          ln ebounds
                          (fontsloth-geometry-reverse-points geom)))))
      (setf (fontsloth-geometry-reverse-points geom)
            (< 0 (fontsloth-geometry-area geom)))
      (setf (fontsloth-geometry-v-lines geom)
            (apply #'vector
              (reposition-lines (fontsloth-geometry-v-lines geom)))
            (fontsloth-geometry-m-lines geom)
            (apply #'vector
                   (reposition-lines (fontsloth-geometry-m-lines geom))))
      (record 'fontsloth-glyph
              (fontsloth-geometry-v-lines geom)
              (fontsloth-geometry-m-lines geom)
              advance-width
              advance-height
              (fontsloth-glyph-outline-bounds-create
               :xmin (fontsloth-bbox-xmin ebounds)
               :ymin (fontsloth-bbox-ymin ebounds)
               :width (- (fontsloth-bbox-xmax ebounds)
                         (fontsloth-bbox-xmin ebounds))
               :height (- (fontsloth-bbox-ymax ebounds)
                          (fontsloth-bbox-ymin ebounds)))))))

(defun fontsloth-glyph-create (glyph-id scale units-per-em)
  "Construct a glyph given its id and desired units-per-em.
GLYPH-ID the id
SCALE the geometry scale
UNITS-PER-EM the units per em"
  (let ((geom (fontsloth-geometry-create scale units-per-em)))
    (fontsloth-otf-outline-glyph glyph-id geom)
    (fontsloth-geometry-finalize
     geom (fontsloth-otf-glyph-hor-advance glyph-id) 0.0)))


(cl-defmethod fontsloth-otf-move-to ((outliner fontsloth-geometry) x y)
  "Implement move-to on fontsloth-geometry.
OUTLINER geometry struct
X x coord
Y y coord"
  (let ((next-point (fontsloth-point-create :x x :y y)))
    (setf (fontsloth-geometry-start-point outliner) next-point
          (fontsloth-geometry-previous-point outliner) next-point)))

(cl-defmethod fontsloth-otf-line-to ((outliner fontsloth-geometry) x y)
  "Implement line-to on fontsloth-geometry.
OUTLINER geometry struct
X x coord
Y y coord"
  (let ((next-point (fontsloth-point-create :x x :y y)))
    (fontsloth--geometry-push
     outliner (fontsloth-geometry-previous-point outliner) next-point)
    (setf (fontsloth-geometry-previous-point outliner) next-point)))

(cl-defmethod fontsloth-otf-quad-to ((outliner fontsloth-geometry) x0 y0 x1 y1)
  "Implement quad-to on fontsloth-geometry.
OUTLINER geometry struct
X0 x coord of the control point
Y0 y coord of the control point
X1 x coord of the next curve point
Y1 y coord of the next curve point"
  (let* ((control-point (fontsloth-point-create :x x0 :y y0))
         (next-point (fontsloth-point-create :x x1 :y y1))
         (curve (fontsloth-quad-curve-create
                 :a (fontsloth-geometry-previous-point outliner)
                 :b control-point
                 :c next-point))
         (stack `(,(fontsloth-segment-create
                    :a (fontsloth-geometry-previous-point outliner) :at 0.0
                    :c next-point :ct 1.0))))
    (cl-loop for seg = (pop stack) then (pop stack)
             while seg do
             (let* ((bt (* 0.5 (+ (fontsloth-segment-at seg)
                                  (fontsloth-segment-ct seg))))
                    (b (fontsloth--quad-curve-point curve bt))
                    (area (- (* (- (fontsloth-point-x b)
                                   (fontsloth-point-x
                                    (fontsloth-segment-a seg)))
                                (- (fontsloth-point-y (fontsloth-segment-c seg))
                                   (fontsloth-point-y
                                    (fontsloth-segment-a seg))))
                             (* (- (fontsloth-point-x (fontsloth-segment-c seg))
                                   (fontsloth-point-x
                                    (fontsloth-segment-a seg)))
                                (- (fontsloth-point-y b)
                                   (fontsloth-point-y
                                    (fontsloth-segment-a seg)))))))
               (if (> (abs area) (fontsloth-geometry-max-area outliner))
                   (progn (push (fontsloth-segment-create
                                 :a (fontsloth-segment-a seg)
                                 :at (fontsloth-segment-at seg)
                                 :c b
                                 :ct bt) stack)
                          (push (fontsloth-segment-create
                                 :a b
                                 :at bt
                                 :c (fontsloth-segment-c seg)
                                 :ct (fontsloth-segment-ct seg)) stack))
                 (fontsloth--geometry-push
                  outliner
                  (fontsloth-segment-a seg)
                  (fontsloth-segment-c seg)))))
    (setf (fontsloth-geometry-previous-point outliner) next-point)))

(cl-defmethod fontsloth-otf-close-contour ((outliner fontsloth-geometry))
  "Implement close-contour on fontsloth-geometry.
OUTLINER the geometry"
  (when (not (equal (fontsloth-geometry-start-point outliner)
                    (fontsloth-geometry-previous-point outliner)))
    (fontsloth--geometry-push outliner
                              (fontsloth-geometry-previous-point outliner)
                              (fontsloth-geometry-start-point outliner)))
  (setf (fontsloth-geometry-previous-point outliner)
        (fontsloth-geometry-start-point outliner)))

(provide 'fontsloth-geometry)
;;; fontsloth-geometry.el ends here
