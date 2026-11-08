;;; fontsloth-geometry.el --- Implements otf outliner generics -*- lexical-binding: t -*-

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
    (fontsloth-cube-curve
     (:constructor fontsloth-cube-curve-create)
     (:copier nil))
  "Describes a bezier curve with two control points."
  (a nil :type 'fontsloth-point :documentation "point 1")
  (b nil :type 'fontsloth-point :documentation "control point")
  (c nil :type 'fontsloth-point :documentation "control point")
  (d nil :type 'fontsloth-point :documentation "point 2"))

(cl-defstruct
    (fontsloth-geometry
     (:constructor fontsloth-make-geometry)
     (:copier nil))
  "Describes the geometry of a glyph outline.
To be handed to `fontsloth-otf-outline-glyph'"
  (v-lines nil :type 'sequence
           :documentation "lines which vary in y but not in x")
  (m-lines nil :type 'sequence :documentation "lines which are not v-lines")
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

(defvar fontsloth-seg-stack--size 128
  "Initial size for `fontsloth-seg-stack-pool'.")

(cl-defstruct
    (fontsloth-seg-stack
     (:constructor fontsloth-seg-stack-create)
     (:copier nil))
  "Holds state for a stack of `fontsloth-segment'."
  (idx -1 :type 'integer :documentation "current push index")
  (pop-ptr -1 :type 'integer :documentation "current pop pointer")
  (pops (make-vector fontsloth-seg-stack--size 0)
        :type vector :documentation "previous pop pointer per stack slot")
  (pool nil :type 'vector :documentation "pool of objects"))

(defsubst fontsloth-seg-stack--reset (stack)
  "Reset STACK."
  (setf (fontsloth-seg-stack-idx stack) -1
        (fontsloth-seg-stack-pop-ptr stack) -1))

(defun fontsloth-seg-stack--grow (stack)
  "Grow STACK."
  (let* ((old-length (length (fontsloth-seg-stack-pool stack)))
         (new-length (+ old-length old-length old-length))
         (new-pool (make-vector new-length nil))
         (new-pops (make-vector new-length 0)))
      (fontsloth:info*
       fontsloth-log "Fontsloth-geometry: resizing segment stack from %s to %s"
       old-length new-length)
      (cl-loop for i from 0 below new-length
               if (< i old-length) do
               (aset new-pool i (aref (fontsloth-seg-stack-pool stack) i))
               (aset new-pops i (aref (fontsloth-seg-stack-pops stack) i))
               else do
               (aset new-pool i (fontsloth-segment-create))
               finally (setf (fontsloth-seg-stack-pool stack) new-pool
                             (fontsloth-seg-stack-pops stack) new-pops))))

(defun fontsloth-seg-stack--push (a at c ct stack)
  "Push `fontsloth-segment' components A, AT, C, CT onto STACK."
  (unless (fontsloth-seg-stack-pool stack)
    (cl-loop for i from 0 below fontsloth-seg-stack--size
             with res = (make-vector fontsloth-seg-stack--size nil)
             do (aset res i (fontsloth-segment-create))
             finally do (setf (fontsloth-seg-stack-pool stack) res)))
  (cl-incf (fontsloth-seg-stack-idx stack))
  (when (>= (fontsloth-seg-stack-idx stack)
            (length (fontsloth-seg-stack-pool stack)))
    (fontsloth-seg-stack--grow stack))
  (aset (fontsloth-seg-stack-pops stack)
        (fontsloth-seg-stack-idx stack)
        (fontsloth-seg-stack-pop-ptr stack))
  (setf (fontsloth-seg-stack-pop-ptr stack) (fontsloth-seg-stack-idx stack))
  (let ((seg (aref (fontsloth-seg-stack-pool stack)
                   (fontsloth-seg-stack-idx stack))))
    (aset seg 1 a)
    (aset seg 2 at)
    (aset seg 3 c)
    (aset seg 4 ct)))

(defun fontsloth-seg-stack--pop (stack)
  "Pop a `fontsloth-segment' from STACK or nil if STACK is empty."
  (when (<= 0 (fontsloth-seg-stack-pop-ptr stack))
    (prog1
        (aref (fontsloth-seg-stack-pool stack)
              (fontsloth-seg-stack-pop-ptr stack))
      (setf (fontsloth-seg-stack-pop-ptr stack)
            (aref (fontsloth-seg-stack-pops stack)
                  (fontsloth-seg-stack-pop-ptr stack))))))

(defvar fontsloth-geom--seg-stack (fontsloth-seg-stack-create)
  "A stack with a fixed pool of `fontsloth-segment' objects.
Objects are re-used during curve-to and quad-to.")

(cl-defstruct
    (fontsloth-glyph-outline-bounds
     (:constructor fontsloth-glyph-outline-bounds-create)
     (:copier nil))
  "Describes a glyph's calculated outline bounds."
  (xmin 0.0 :type 'number :documentation "least glyph x")
  (ymin 0.0 :type 'number :documentation "least glyph y")
  (width 0.0 :type 'number :documentation "calculated width")
  (height 0.0 :type 'number :documentation "calculated height"))

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

(defmacro fontsloth-geometry--with-quad-curve-xy (qc time x y &rest body)
  "Bind X and Y to the quad curve QC at TIME, then evaluate BODY."
  (declare (indent 4) (debug (form form symbolp symbolp body)))
  (let ((tm (make-symbol "tm")) (a (make-symbol "a"))
        (b (make-symbol "b")) (c (make-symbol "c")))
    `(let* ((,tm (- 1.0 ,time))
            (,a (* ,tm ,tm))
            (,b (* 2.0 ,tm ,time))
            (,c (* ,time ,time))
            (,x (+ (* ,a (fontsloth-point-x (fontsloth-quad-curve-a ,qc)))
                   (* ,b (fontsloth-point-x (fontsloth-quad-curve-b ,qc)))
                   (* ,c (fontsloth-point-x (fontsloth-quad-curve-c ,qc)))))
            (,y (+ (* ,a (fontsloth-point-y (fontsloth-quad-curve-a ,qc)))
                   (* ,b (fontsloth-point-y (fontsloth-quad-curve-b ,qc)))
                   (* ,c (fontsloth-point-y (fontsloth-quad-curve-c ,qc))))))
       ,@body)))

(defun fontsloth-geometry--quad-curve-point (qc time)
  "Determine the point along a curve at a given time.
QC the curve
TIME the time"
  (fontsloth-geometry--with-quad-curve-xy qc time x y
    (fontsloth-point-create :x x :y y)))

(defmacro fontsloth-geometry--with-cube-curve-xy (cc time x y &rest body)
  "Bind X and Y to the cube curve CC at TIME, then evaluate BODY."
  (declare (indent 4) (debug (form form symbolp symbolp body)))
  (let ((tm (make-symbol "tm")) (a (make-symbol "a"))
        (b (make-symbol "b")) (c (make-symbol "c"))
        (d (make-symbol "d")))
    `(let* ((,tm (- 1.0 ,time))
            (,a (* ,tm ,tm ,tm))
            (,b (* 3.0 ,tm ,tm ,time))
            (,c (* 3.0 ,tm ,time ,time))
            (,d (* ,time ,time ,time))
            (,x (+ (* ,a (fontsloth-point-x (fontsloth-cube-curve-a ,cc)))
                   (* ,b (fontsloth-point-x (fontsloth-cube-curve-b ,cc)))
                   (* ,c (fontsloth-point-x (fontsloth-cube-curve-c ,cc)))
                   (* ,d (fontsloth-point-x (fontsloth-cube-curve-d ,cc)))))
            (,y (+ (* ,a (fontsloth-point-y (fontsloth-cube-curve-a ,cc)))
                   (* ,b (fontsloth-point-y (fontsloth-cube-curve-b ,cc)))
                   (* ,c (fontsloth-point-y (fontsloth-cube-curve-c ,cc)))
                   (* ,d (fontsloth-point-y (fontsloth-cube-curve-d ,cc))))))
       ,@body)))

(defun fontsloth-geometry--cube-curve-point (cc time)
  "Determine the point along a curve at a given time.
CC the curve
TIME the time"
  (fontsloth-geometry--with-cube-curve-xy cc time x y
    (fontsloth-point-create :x x :y y)))

(defun fontsloth-geometry-create (scale units-per-em)
  "Construct a new geometry given `scale' and `units-per-em'.
SCALE a number indicating the font scale for drawing
UNITS-PER-EM a number indicating units per em"
  (let* ((error-threshold-px 3.0)
         (max-area (* 2.0 error-threshold-px (/ units-per-em scale))))
    (fontsloth-make-geometry :max-area max-area)))

(defun fontsloth-geometry-push (geom start end)
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
        (push (fontsloth-line-create start end)
              (fontsloth-geometry-v-lines geom))
      (push (fontsloth-line-create start end)
            (fontsloth-geometry-m-lines geom)))
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
                (cl-loop for ln in lines do
                         (fontsloth-line-reposition
                          ln ebounds
                          (fontsloth-geometry-reverse-points geom)))))
      (setf (fontsloth-geometry-reverse-points geom)
            (< 0 (fontsloth-geometry-area geom)))
      (setf (fontsloth-geometry-v-lines geom)
            (nreverse (fontsloth-geometry-v-lines geom))
            (fontsloth-geometry-m-lines geom)
            (nreverse (fontsloth-geometry-m-lines geom)))
      (reposition-lines (fontsloth-geometry-v-lines geom))
      (reposition-lines (fontsloth-geometry-m-lines geom))
      (record 'fontsloth-glyph
              (apply #'vector (fontsloth-geometry-v-lines geom))
              (apply #'vector (fontsloth-geometry-m-lines geom))
              advance-width
              advance-height
              (fontsloth-glyph-outline-bounds-create
               :xmin (fontsloth-bbox-xmin ebounds)
               :ymin (fontsloth-bbox-ymin ebounds)
               :width (- (fontsloth-bbox-xmax ebounds)
                         (fontsloth-bbox-xmin ebounds))
               :height (- (fontsloth-bbox-ymax ebounds)
                          (fontsloth-bbox-ymin ebounds)))))))

(require 'fontsloth-glyph)

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
    (fontsloth-geometry-push
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
         (stack fontsloth-geom--seg-stack))
    (cl-loop for seg = (fontsloth-seg-stack--pop stack)
             initially do
             (fontsloth-seg-stack--reset stack)
             (fontsloth-seg-stack--push
              (fontsloth-geometry-previous-point outliner) 0.0 next-point 1.0
              stack)
             while seg do
             (let* ((bt (* 0.5 (+ (fontsloth-segment-at seg)
                                  (fontsloth-segment-ct seg))))
                    (a-x (fontsloth-point-x (fontsloth-segment-a seg)))
                    (a-y (fontsloth-point-y (fontsloth-segment-a seg)))
                    (c-x (fontsloth-point-x (fontsloth-segment-c seg)))
                    (c-y (fontsloth-point-y (fontsloth-segment-c seg))))
               (fontsloth-geometry--with-quad-curve-xy curve bt bx by
                 (let ((area (- (* (- bx a-x) (- c-y a-y))
                                (* (- c-x a-x) (- by a-y)))))
                   (if (> (abs area) (fontsloth-geometry-max-area outliner))
                       (let ((b (fontsloth-point-create :x bx :y by)))
                         (fontsloth-seg-stack--push
                          (fontsloth-segment-a seg)
                          (fontsloth-segment-at seg)
                          b bt stack)
                         (fontsloth-seg-stack--push
                          b bt
                          (fontsloth-segment-c seg)
                          (fontsloth-segment-ct seg)
                          stack))
                     (fontsloth-geometry-push
                      outliner
                      (fontsloth-segment-a seg)
                      (fontsloth-segment-c seg)))))))
    (setf (fontsloth-geometry-previous-point outliner) next-point)))

(cl-defmethod fontsloth-otf-curve-to
  ((outliner fontsloth-geometry) x0 y0 x1 y1 x2 y2)
  "Implement curve-to on fontsloth-geometry.
OUTLINER geometry struct
X0 x coord of the first control point
Y0 y coord of the first control point
X1 x coord of the second control point
Y1 y coord of the second control point
X2 x coord of the next curve point
Y2 y coord of the next curve point"
  (let* ((first-control (fontsloth-point-create :x x0 :y y0))
         (second-control (fontsloth-point-create :x x1 :y y1))
         (next-point (fontsloth-point-create :x x2 :y y2))
         (curve (fontsloth-cube-curve-create
                 :a (fontsloth-geometry-previous-point outliner)
                 :b first-control
                 :c second-control
                 :d next-point))
         (stack fontsloth-geom--seg-stack))
    (cl-loop for seg = (fontsloth-seg-stack--pop stack)
             initially do
             (fontsloth-seg-stack--reset stack)
             (fontsloth-seg-stack--push
              (fontsloth-geometry-previous-point outliner) 0.0 next-point 1.0
              stack)
             while seg do
             (let* ((bt (* 0.5 (+ (fontsloth-segment-at seg)
                                  (fontsloth-segment-ct seg))))
                    (a-x (fontsloth-point-x (fontsloth-segment-a seg)))
                    (a-y (fontsloth-point-y (fontsloth-segment-a seg)))
                    (c-x (fontsloth-point-x (fontsloth-segment-c seg)))
                    (c-y (fontsloth-point-y (fontsloth-segment-c seg))))
               (fontsloth-geometry--with-cube-curve-xy curve bt bx by
                 (let ((area (- (* (- bx a-x) (- c-y a-y))
                                (* (- c-x a-x) (- by a-y)))))
                   (if (> (abs area) (fontsloth-geometry-max-area outliner))
                       (let ((b (fontsloth-point-create :x bx :y by)))
                         (fontsloth-seg-stack--push
                          (fontsloth-segment-a seg)
                          (fontsloth-segment-at seg)
                          b bt stack)
                         (fontsloth-seg-stack--push
                          b bt
                          (fontsloth-segment-c seg)
                          (fontsloth-segment-ct seg)
                          stack))
                     (fontsloth-geometry-push
                      outliner
                      (fontsloth-segment-a seg)
                      (fontsloth-segment-c seg)))))))
    (setf (fontsloth-geometry-previous-point outliner) next-point)))

(cl-defmethod fontsloth-otf-close-contour ((outliner fontsloth-geometry))
  "Implement close-contour on fontsloth-geometry.
OUTLINER the geometry"
  (when (not (equal (fontsloth-geometry-start-point outliner)
                    (fontsloth-geometry-previous-point outliner)))
    (fontsloth-geometry-push outliner
                             (fontsloth-geometry-previous-point outliner)
                             (fontsloth-geometry-start-point outliner)))
  (setf (fontsloth-geometry-previous-point outliner)
        (fontsloth-geometry-start-point outliner)))

(provide 'fontsloth-geometry)
;;; fontsloth-geometry.el ends here
