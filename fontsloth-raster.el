;;; fontsloth-raster.el --- Elisp otf/ttf font renderer -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.12.1
;; Package-Requires: ((cl-lib "1.0") (logito "0.1") (emacs "26.1"))
;; Keywords: true-type, font, rasterization, ttf, otf

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

;; fontsloth-raster (this file): for now a rip off of fontdue raster

;;; Code:

(require 'cl-lib)

(require 'fontsloth--common-types)
(require 'fontsloth-log)

(cl-defstruct
    (fontsloth-raster
     (:copier nil))
  "A struct to hold the raster output."
  (width nil :type 'fixed :documentation "expected width")
  (height nil :type 'fixed :documentation "expected height")
  (canvas nil :type 'vector :documentation "output goes here"))

(defun fontsloth-raster-create (width height)
  "Create a new raster with the given width and height.
WIDTH the expected width
HEIGHT the expected height"
  (make-fontsloth-raster :width width :height height
                         :canvas (make-vector (+ 3 (* width height)) 0)))

(defun fontsloth--raster-fract (f)
  "Return the fractional part of float `f'.
F float to fract"
  (- f (truncate f)))

(defun fontsloth--raster-add (raster index height mid-x)
  "Add a raster output at the given index and coordinates.
RASTER the raster to update
INDEX the raster index
HEIGHT the computed height of the output
MID-X the computed mid x coord of the output"
  (let ((c (fontsloth-raster-canvas raster))
        (m (* height mid-x)))
    (aset c index (+ (- height m) (aref c index)))
    (aset c (1+ index) (+ (aref c (1+ index)) m))))

(defun fontsloth--raster-v-line (raster line coords)
  "Rasterize a v-line.
RASTER the raster to update
LINE the v-line
COORDS the scaled coordinates"
  (let* ((x0 (car coords)) (y0 (cadr coords)) (y1 (cadddr coords))
         (nudge (fontsloth-line-nudge line))
         (temp `(,(truncate (- (car coords) (car nudge)))
                 ,(truncate (- (cadr coords) (cadr nudge)))
                 ,(truncate (- (caddr coords) (caddr nudge)))
                 ,(truncate (- (cadddr coords) (cadddr nudge)))))
         (stx (car temp)) (sty (cadr temp)) (ex (caddr temp)) (ey (cadddr temp))
         (adj (fontsloth-line-adjustment line))
         (target-y (+ (cadr adj) (cadr temp)))
         (sy (copysign 1.0 (- y1 y0)))
         (y-prev y0)
         (index (truncate (+ stx (* sty (fontsloth-raster-width raster) 1.0))))
         (index-y-inc (truncate
                       (copysign (* 1.0 (fontsloth-raster-width raster)) sy)))
         (dist (truncate (abs (- sty ey))))
         (mid-x (fontsloth--raster-fract x0)))
    (fontsloth:debug* fontsloth-log
                      (concat
                       "start v-line: "
                       "dist %s stx %s sty %s ex %s ey %s
"
                       "index %s index-y-inc %s")
                      dist stx sty ex ey
                      index index-y-inc)
    (dotimes (_ dist)
      (fontsloth--raster-add raster index (- y-prev target-y) mid-x)
      (setf index (+ index index-y-inc))
      (setf y-prev target-y)
      (setf target-y (+ target-y sy))
      (fontsloth:debug* fontsloth-log "index %s index-y-inc %s"
                        index index-y-inc))
    (fontsloth--raster-add
     raster (truncate (+ ex (* ey (fontsloth-raster-width raster))))
     (- y-prev y1) mid-x))
  (fontsloth:debug* fontsloth-log "end v-line"))

(defun fontsloth--raster-m-line (raster line coords params)
  "Rasterize an m-line.
RASTER the raster to update
LINE the m-line
COORDS the scaled coordinates
PARAMS the scaled parameters"
  (let* ((x0 (car coords)) (y0 (cadr coords))
         (x1 (caddr coords)) (y1 (cadddr coords))
         (nudge (fontsloth-line-nudge line))
         (temp `(,(truncate (- (car coords) (car nudge)))
                 ,(truncate (- (cadr coords) (cadr nudge)))
                 ,(truncate (- (caddr coords) (caddr nudge)))
                 ,(truncate (- (cadddr coords) (cadddr nudge)))))
         (stx (car temp)) (sty (cadr temp)) (ex (caddr temp)) (ey (cadddr temp))
         (tdx (car params)) (tdy (cadr params))
         (dx (caddr params)) (dy (cadddr params))
         (adj (fontsloth-line-adjustment line))
         (target-x (+ (car adj) (car temp)))
         (target-y (+ (cadr adj) (cadr temp)))
         (sx (copysign 1.0 tdx))
         (sy (copysign 1.0 tdy))
         (tmx (* tdx (- target-x x0))) (tmy (* tdy (- target-y y0)))
         (tdx (abs tdx)) (tdy (abs tdy))
         (x-prev x0) (y-prev y0)
         (index (truncate (+ stx (* sty (fontsloth-raster-width raster)))))
         (index-x-inc (truncate sx))
         (index-y-inc (truncate
                       (copysign (* 1.0 (fontsloth-raster-width raster)) sy)))
         (dist (+ (abs (- stx ex)) (abs (- sty ey)))))
    (fontsloth:debug*
     fontsloth-log
     (concat "start m-line: "
             "x0 %.6f y0 %.6f x1 %.6f y1 %.6f
"
             "dist %s stx %s sty %s ex %s ey %s
"
             "tdx %.6f tdy %.6f dx %.6f dy %.6f tmx %.6f tmy %.6f
"
             "index %s index-x-inc %s index-y-inc %s")
     x0 y0 x1 y1
     dist stx sty ex ey
     tdx tdy dx dy tmx tmy
     index index-x-inc index-y-inc)
    (dotimes (_ dist)
      (let ((prev-index index)
            (y-next) (x-next))
        (if (< tmx tmy)
            (setf y-next (+ y0 (* dy tmx))
                  x-next target-x
                  tmx (+ tmx tdx)
                  target-x (+ target-x sx)
                  index (+ index index-x-inc))
          (setf y-next target-y
                x-next (+ x0 (* dx tmy))
                tmy (+ tmy tdy)
                target-y (+ target-y sy)
                index (+ index index-y-inc)))
        (fontsloth--raster-add raster prev-index (- y-prev y-next)
                               (fontsloth--raster-fract (/ (+ x-prev x-next)
                                                           2.0)))
        (setf x-prev x-next y-prev y-next)
        (fontsloth:debug* fontsloth-log "index %s index-x-inc %s index-y-inc %s"
                          index index-x-inc index-y-inc)))
    (fontsloth--raster-add raster
                           (truncate (+ ex
                                        (* (fontsloth-raster-width raster) ey)))
                           (- y-prev y1)
                           (fontsloth--raster-fract (/ (+ x-prev x1) 2.0)))
    (fontsloth:debug* fontsloth-log "end m-line")))

(defun fontsloth-raster-draw (raster glyph scale-x scale-y offset-x offset-y)
  "Enter into the raster with a given outlined glyph and scaling/offset.
RASTER the raster in which to draw
SCALE-X x scale
SCALE-Y y scale
OFFSET-X x offset
OFFSET-Y y offset"
  (let ((params `(,(/ 1.0 scale-x) ,(/ 1.0 scale-y) ,scale-x ,scale-y)))
    (cl-flet ((scale-off (coords)
                `(,(+ offset-x (* scale-x (car coords)))
                  ,(+ offset-y (* scale-y (cadr coords)))
                  ,(+ offset-x (* scale-x (caddr coords)))
                  ,(+ offset-y (* scale-y (cadddr coords)))))
              (parameterize (lparams)
                `(,(* (car params) (car lparams))
                  ,(* (cadr params) (cadr lparams))
                  ,(* (caddr params) (caddr lparams))
                  ,(* (cadddr params) (cadddr lparams)))))
      (cl-loop for ln across (fontsloth-glyph-v-lines glyph) do
               (fontsloth--raster-v-line raster ln
                 (scale-off (fontsloth-line-coords ln))))
      (cl-loop for ln across (fontsloth-glyph-m-lines glyph) do
               (fontsloth--raster-m-line raster ln
                 (scale-off (fontsloth-line-coords ln))
                 (parameterize (fontsloth-line-params ln)))))))

(defun fontsloth-raster-get-pixel (raster length)
  "Get the pixelated output of the raster.
RASTER the raster
LENGTH the expected length"
  (cl-flet ((clamp (in min max)
              (cond ((> min in) min)
                    ((< max in) max)
                    (t in))))
    (let ((canvas (fontsloth-raster-canvas raster))
           (output (make-vector length 0))
          (height 0.0))
      (dotimes (i length)
        (setf height (+ height (elt canvas i)))
        (aset output i (truncate (clamp (* (abs height) 255.9) 0.0 255.0))))
      output)))

(cl-defun fontsloth-raster-npbm (pixel width height &optional (format 'pgm))
  "Insert the pixelated raster output into a NetPBM format.
PIXEL vector of pixel bytes
WIDTH the glyph outline width
HEIGHT the glyph outline height
FORMAT optional 'ppm, 'pgm, or 'pbm, default 'pgm"
  (let ((formats '((ppm . "P6
")
                   (pgm . "P5
")
                   (pbm . "P4
"))))
    (cl-flet ((vec->str (v s)
                (dotimes (i (length v))
                  (aset s i (aref v i)))
                s))
      (let* ((pixel-str (make-string (length pixel) 0 nil))
              (npbm (concat (alist-get format formats)
                            (number-to-string width)
                            " "
                            (number-to-string height) "
255
")))
        (concat npbm (vec->str pixel pixel-str))))))

(provide 'fontsloth-raster)
;;; fontsloth-raster.el ends here
