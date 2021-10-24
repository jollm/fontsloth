;;; fontsloth-otf--outline-glyf.el --- Outlining glyphs using glyf -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.13.1
;; Package-Requires: ((cl-lib "1.0") (emacs "26.1"))
;; Keywords: true-type, font, glyph, glyf, ttf, otf

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

;; fontsloth-otf--outline-glyf (this file): Glyph outline implementation that
;; relies on the OTF/TTF loca/glyf tables

;;; Code:

(require 'cl-lib)

(require 'fontsloth--common-types)

(cl-defstruct
    (fontsloth-otf--glyf-transform
     (:constructor fontsloth-otf--glyf-transform-create)
     (:copier nil))
  (a 1.0 :type 'number)
  (b 0.0 :type 'number)
  (c 0.0 :type 'number)
  (d 1.0 :type 'number)
  (e 0.0 :type 'number)
  (f 0.0 :type 'number))

(defsubst fontsloth-otf--glyf-transform-default-p (ts)
  "Return t if TS is the default transform."
  (pcase ts
    ((cl-struct fontsloth-otf--glyf-transform
                (a '1.0) (b '0.0) (c '0.0) (d '1.0) (e '0.0) (f '0.0)) t)
    (_ nil)))

(defsubst fontsloth-otf--glyf-transform-combine (ts1 ts2)
  "Combine transforms TS1 and TS2."
  (fontsloth-otf--glyf-transform-create
   :a (+ (* (fontsloth-otf--glyf-transform-a ts1)
            (fontsloth-otf--glyf-transform-a ts2))
         (* (fontsloth-otf--glyf-transform-c ts1)
            (fontsloth-otf--glyf-transform-b ts2)))
   :b (+ (* (fontsloth-otf--glyf-transform-b ts1)
            (fontsloth-otf--glyf-transform-a ts2))
         (* (fontsloth-otf--glyf-transform-d ts1)
            (fontsloth-otf--glyf-transform-b ts2)))
   :c (+ (* (fontsloth-otf--glyf-transform-a ts1)
            (fontsloth-otf--glyf-transform-c ts2))
         (* (fontsloth-otf--glyf-transform-c ts1)
            (fontsloth-otf--glyf-transform-d ts2)))
   :d (+ (* (fontsloth-otf--glyf-transform-b ts1)
            (fontsloth-otf--glyf-transform-c ts2))
         (* (fontsloth-otf--glyf-transform-d ts1)
            (fontsloth-otf--glyf-transform-d ts2)))
   :e (+ (* (fontsloth-otf--glyf-transform-a ts1)
            (fontsloth-otf--glyf-transform-e ts2))
         (* (fontsloth-otf--glyf-transform-c ts1)
            (fontsloth-otf--glyf-transform-f ts2))
         (fontsloth-otf--glyf-transform-e ts1))
   :f (+ (* (fontsloth-otf--glyf-transform-b ts1)
            (fontsloth-otf--glyf-transform-e ts2))
         (* (fontsloth-otf--glyf-transform-d ts1)
            (fontsloth-otf--glyf-transform-f ts2))
         (fontsloth-otf--glyf-transform-f ts1))))

(defsubst fontsloth-otf--glyf-transform-apply-to (ts x y)
  "Apply affine transform TS to X and Y."
  (pcase-let (((cl-struct fontsloth-otf--glyf-transform a b c d e f) ts))
    `(,(+ (* a x) (* c y) e) ,(+ (* b x) (* d y) f))))

(cl-defstruct
  (fontsloth-otf--glyf-builder
     (:constructor fontsloth-otf--glyf-builder-create)
     (:copier nil))
  (outliner nil)
  (transform (fontsloth-otf--glyf-transform-create)
             :type 'fontsloth-otf--glyf-transform)
  (bbox (fontsloth-bbox-create) :type 'fontsloth-bbox)
  (first-on-curve nil :type 'fontsloth-point)
  (first-off-curve nil :type 'fontsloth-point)
  (last-off-curve nil :type 'fontsloth-point))

(defun fontsloth-otf--glyf-move-to (builder x y)
  "Move to the start of a contour at x,y.
BUILDER a `fontsloth-otf--glyf-builder'
X x coord of contour start
Y y coord of contour start"
  (unless (fontsloth-otf--glyf-transform-default-p
           (fontsloth-otf--glyf-builder-transform builder))
    (pcase-let* ((ts (fontsloth-otf--glyf-builder-transform builder))
                 (`(,tx ,ty) (fontsloth-otf--glyf-transform-apply-to ts x y)))
      (setq x tx y ty)))
  (fontsloth-bbox-extend-by (fontsloth-otf--glyf-builder-bbox builder) x y)
  (fontsloth-otf-move-to (fontsloth-otf--glyf-builder-outliner builder) x y))

(defun fontsloth-otf--glyf-line-to (builder x y)
  "Add a contour segment ending at x,y.
BUILDER a `fontsloth-otf--glyf-builder'
X x coord of line end
Y y coord of line end"
  (unless (fontsloth-otf--glyf-transform-default-p
           (fontsloth-otf--glyf-builder-transform builder))
    (pcase-let* ((ts (fontsloth-otf--glyf-builder-transform builder))
                 (`(,tx ,ty) (fontsloth-otf--glyf-transform-apply-to ts x y)))
      (setq x tx y ty)))
  (fontsloth-bbox-extend-by (fontsloth-otf--glyf-builder-bbox builder) x y)
  (fontsloth-otf-line-to (fontsloth-otf--glyf-builder-outliner builder) x y))

(defun fontsloth-otf--glyf-quad-to (builder x1 y1 x y)
  "Add the parabolic curve with control point x1, y1 ending at x,y.
BUILDER a `fontsloth-otf--glyf-builder'
X1 x coord of control point
Y1 y coord of control point
X x coord of curve end
Y y coord of curve end"
  (unless (fontsloth-otf--glyf-transform-default-p
           (fontsloth-otf--glyf-builder-transform builder))
    (pcase-let* ((ts (fontsloth-otf--glyf-builder-transform builder))
                 (`(,tx1 ,ty1) (fontsloth-otf--glyf-transform-apply-to
                                ts x1 y1))
                 (`(,tx ,ty) (fontsloth-otf--glyf-transform-apply-to ts x y)))
      (setq x1 tx1 y1 ty1 x tx y ty)))
  (fontsloth-bbox-extend-by (fontsloth-otf--glyf-builder-bbox builder) x1 y1)
  (fontsloth-bbox-extend-by (fontsloth-otf--glyf-builder-bbox builder) x y)
  (fontsloth-otf-quad-to
   (fontsloth-otf--glyf-builder-outliner builder) x1 y1 x y))

(defun fontsloth-otf--glyf-push-point (builder x y on-curve? last-point?)
  "Push a new point into the glyph outline.
BUILDER a `fontsloth-otf--glyf-builder'
X x coord of the new point
Y y coord of the new point
ON-CURVE? t if the point is on the curve
LAST-POINT? t if the point is the last of a contour"
  (let ((p (fontsloth-point-create :x x :y y)))
    (if (not (fontsloth-otf--glyf-builder-first-on-curve builder))
        (if on-curve?
            (progn (setf (fontsloth-otf--glyf-builder-first-on-curve builder) p)
                   (fontsloth-otf--glyf-move-to builder x y))
          (if-let ((off-curve
                    (fontsloth-otf--glyf-builder-first-off-curve builder)))
              (let ((mid (fontsloth-point-between off-curve p 0.5)))
                (setf (fontsloth-otf--glyf-builder-first-on-curve builder) mid
                      (fontsloth-otf--glyf-builder-last-off-curve builder) p)
                (fontsloth-otf--glyf-move-to builder
                                             (fontsloth-point-x mid)
                                             (fontsloth-point-y mid)))
            (setf (fontsloth-otf--glyf-builder-first-off-curve builder) p)))
      (pcase `(,(fontsloth-otf--glyf-builder-last-off-curve builder)
               . ,on-curve?)
        (`(,(and offcurve (pred identity)) . t)
         (setf (fontsloth-otf--glyf-builder-last-off-curve builder) nil)
         (fontsloth-otf--glyf-quad-to
          builder
          (fontsloth-point-x offcurve) (fontsloth-point-y offcurve)
          (fontsloth-point-x p) (fontsloth-point-y p)))
        (`(,(and offcurve (pred identity)) . nil)
         (setf (fontsloth-otf--glyf-builder-last-off-curve builder) p)
         (let ((mid (fontsloth-point-between offcurve p 0.5)))
           (fontsloth-otf--glyf-quad-to
            builder (fontsloth-point-x offcurve) (fontsloth-point-y offcurve)
            (fontsloth-point-x mid) (fontsloth-point-y mid))))
        (`(nil . t) (fontsloth-otf--glyf-line-to
                     builder (fontsloth-point-x p) (fontsloth-point-y p)))
        (_ (setf (fontsloth-otf--glyf-builder-last-off-curve builder) p)))))
  (when last-point? (fontsloth-otf--glyf-finish-contour builder)))

(defun fontsloth-otf--glyf-finish-contour (builder)
  "Finish a contour.
BUILDER a `fontsloth-otf--glyf-builder'"
  (when-let ((offcurve1 (fontsloth-otf--glyf-builder-first-off-curve builder))
             (offcurve2 (fontsloth-otf--glyf-builder-last-off-curve builder)))
    (setf (fontsloth-otf--glyf-builder-last-off-curve builder) nil)
    (let ((mid (fontsloth-point-between offcurve2 offcurve1 0.5)))
      (fontsloth-otf--glyf-quad-to
       builder
       (fontsloth-point-x offcurve2) (fontsloth-point-y offcurve2)
       (fontsloth-point-x mid) (fontsloth-point-y mid))))
  (if-let ((p (fontsloth-otf--glyf-builder-first-on-curve builder))
           (offcurve1 (fontsloth-otf--glyf-builder-first-off-curve builder)))
      (fontsloth-otf--glyf-quad-to
       builder
       (fontsloth-point-x offcurve1) (fontsloth-point-y offcurve1)
       (fontsloth-point-x p) (fontsloth-point-y p))
    (if-let ((p (fontsloth-otf--glyf-builder-first-on-curve builder))
             (offcurve2 (fontsloth-otf--glyf-builder-last-off-curve builder)))
        (fontsloth-otf--glyf-quad-to
         builder
         (fontsloth-point-x offcurve2) (fontsloth-point-y offcurve2)
         (fontsloth-point-x p) (fontsloth-point-y p))
      (when-let ((p (fontsloth-otf--glyf-builder-first-on-curve builder)))
        (fontsloth-otf--glyf-line-to
         builder (fontsloth-point-x p) (fontsloth-point-y p)))))
  (setf (fontsloth-otf--glyf-builder-first-on-curve builder) nil
        (fontsloth-otf--glyf-builder-first-off-curve builder) nil
        (fontsloth-otf--glyf-builder-last-off-curve builder) nil)
  (fontsloth-otf-close-contour (fontsloth-otf--glyf-builder-outliner builder)))

(defun fontsloth-otf--glyf-outline (glyphs glyph-id outliner)
  (when-let* ((builder (fontsloth-otf--glyf-builder-create :outliner outliner))
              (glyph (elt glyphs glyph-id))
              (num-contours (alist-get 'number-of-contours glyph))
              (glyph-data (alist-get 'data glyph)))
    (if (< 0 num-contours)
        (when-let ((num-points (alist-get 'num-points glyph-data))
                   (end-pts (alist-get 'end-pts glyph-data))
                   (flags (alist-get 'flags glyph-data))
                   (x-coords (alist-get 'x-coords glyph-data))
                   (y-coords (alist-get 'y-coords glyph-data)))
          (dotimes (i num-points)
            (let ((flags (car (elt flags i))))
              (fontsloth-otf--glyf-push-point builder
                                              (elt x-coords i) (elt y-coords i)
                                              (alist-get 'on-curve-point flags)
                                              ;; TODO do less work here
                                              (seq-some (lambda (p) (= i p))
                                                        end-pts))))
          (fontsloth-otf--glyf-builder-bbox builder)))))


(provide 'fontsloth-otf--outline-glyf)
;;; fontsloth-otf--outline-glyf.el ends here
