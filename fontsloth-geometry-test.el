;;; fontsloth-geometry-test.el --- Fontsloth geometry tests -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.20.0
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

;; fontsloth-geometry-test.el:
;; Tests for fontsloth geometry outliner.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-geometry)

(ert-deftest fontsloth-seg-stack-push-pop ()
  "Check the fixed object pool stack against a linked list stack."
  (let ((fontsloth-seg-stack--size 10)
        (point-a (fontsloth-point-create :x 1.0 :y 2.0))
        (point-c (fontsloth-point-create :x -1.0 :y 4.0)))
    (cl-loop repeat 10
             with stack = (fontsloth-seg-stack-create)
             with check-stack
             for seg = (fontsloth-seg-stack--pop stack)
             for check = (pop check-stack)
             do
             (should (equal check seg))
             (push (fontsloth-segment-create :a point-a :at 0.0
                                             :c point-c :ct 1.0)
                   check-stack)
             (fontsloth-seg-stack--push
              point-a 0.0 point-c 1.0 stack)
             (push (fontsloth-segment-create :a point-c :at 1.0
                                             :c point-a :ct 0.0)
                   check-stack)
             (fontsloth-seg-stack--push
              point-c 1.0 point-a 0.0 stack)
             when seg do
             (push (fontsloth-segment-create
                    :a (fontsloth-segment-c check) :at 1.0
                    :c (fontsloth-segment-a check) :ct 0.0)
                   check-stack)
             (fontsloth-seg-stack--push
              (fontsloth-segment-c seg) 1.0
              (fontsloth-segment-a seg) 0.0 stack)
             finally do
             (should (eq 30 (length (fontsloth-seg-stack-pool stack))))
             (should (eq 20 (length check-stack)))
             (should (equal (cl-loop for seg = (pop check-stack)
                                     while seg collect seg)
                            (cl-loop for seg = (fontsloth-seg-stack--pop stack)
                                     while seg collect seg))))))

(ert-deftest fontsloth-geometry-quad-curve-point ()
  "Check quadratic Bezier evaluation at the endpoints and midpoint."
  (let ((curve (fontsloth-quad-curve-create
                :a (fontsloth-point-create :x 0.0 :y 0.0)
                :b (fontsloth-point-create :x 0.0 :y 10.0)
                :c (fontsloth-point-create :x 10.0 :y 0.0))))
    (should (equal (fontsloth-geometry--quad-curve-point curve 0.0)
                   (fontsloth-point-create :x 0.0 :y 0.0)))
    (should (equal (fontsloth-geometry--quad-curve-point curve 1.0)
                   (fontsloth-point-create :x 10.0 :y 0.0)))
    (should (equal (fontsloth-geometry--quad-curve-point curve 0.5)
                   (fontsloth-point-create :x 2.5 :y 5.0)))))

(ert-deftest fontsloth-geometry-cube-curve-point ()
  "Check cubic Bezier evaluation at the endpoints and midpoint."
  (let ((curve (fontsloth-cube-curve-create
                :a (fontsloth-point-create :x 0.0 :y 0.0)
                :b (fontsloth-point-create :x 0.0 :y 10.0)
                :c (fontsloth-point-create :x 10.0 :y 10.0)
                :d (fontsloth-point-create :x 10.0 :y 0.0))))
    (should (equal (fontsloth-geometry--cube-curve-point curve 0.0)
                   (fontsloth-point-create :x 0.0 :y 0.0)))
    (should (equal (fontsloth-geometry--cube-curve-point curve 1.0)
                   (fontsloth-point-create :x 10.0 :y 0.0)))
    (should (equal (fontsloth-geometry--cube-curve-point curve 0.5)
                   (fontsloth-point-create :x 5.0 :y 7.5)))))

(ert-deftest fontsloth-geometry-create-max-area ()
  "Check that max-area scales with units-per-em and scale."
  (should (= (fontsloth-geometry-max-area
              (fontsloth-geometry-create 1.0 1000))
             6000.0))
  (should (= (fontsloth-geometry-max-area
              (fontsloth-geometry-create 2.0 1000))
             3000.0)))

(ert-deftest fontsloth-geometry-push ()
  "Check line categorization, area, and bounds accumulation."
  (let ((geom (fontsloth-geometry-create 1.0 1000)))
    ;; Horizontal lines are not needed for the scanline raster.
    (fontsloth-geometry-push geom
                             (fontsloth-point-create :x 0.0 :y 0.0)
                             (fontsloth-point-create :x 10.0 :y 0.0))
    (should (null (fontsloth-geometry-v-lines geom)))
    (should (null (fontsloth-geometry-m-lines geom)))
    (should (= (fontsloth-geometry-area geom) 0.0))
    (fontsloth-geometry-push geom
                             (fontsloth-point-create :x 0.0 :y 0.0)
                             (fontsloth-point-create :x 0.0 :y 10.0))
    (fontsloth-geometry-push geom
                             (fontsloth-point-create :x 0.0 :y 0.0)
                             (fontsloth-point-create :x 10.0 :y 10.0))
    (should (= (length (fontsloth-geometry-v-lines geom)) 1))
    (should (= (length (fontsloth-geometry-m-lines geom)) 1))
    (should (= (fontsloth-geometry-area geom) 100.0))
    (let ((bounds (fontsloth-geometry-effective-bounds geom)))
      (should (= (fontsloth-bbox-xmin bounds) 0.0))
      (should (= (fontsloth-bbox-ymin bounds) 0.0))
      (should (= (fontsloth-bbox-xmax bounds) 10.0))
      (should (= (fontsloth-bbox-ymax bounds) 10.0)))))

(ert-deftest fontsloth-geometry-finalize-empty ()
  "Check finalizing a geometry with no lines."
  (let ((glyph (fontsloth-geometry-finalize
                (fontsloth-geometry-create 1.0 1000) 500.0 600.0)))
    (should (= (length (fontsloth-glyph-v-lines glyph)) 0))
    (should (= (length (fontsloth-glyph-m-lines glyph)) 0))
    (should (= (fontsloth-glyph-advance-width glyph) 500.0))
    (should (= (fontsloth-glyph-advance-height glyph) 600.0))
    (let ((bounds (fontsloth-glyph-bounds glyph)))
      (should (= (fontsloth-glyph-outline-bounds-xmin bounds) 0.0))
      (should (= (fontsloth-glyph-outline-bounds-ymin bounds) 0.0))
      (should (= (fontsloth-glyph-outline-bounds-width bounds) 0.0))
      (should (= (fontsloth-glyph-outline-bounds-height bounds) 0.0)))))

(ert-deftest fontsloth-geometry-finalize-square ()
  "Check finalizing a square contour, including close-contour."
  (let ((geom (fontsloth-geometry-create 1.0 1000)))
    (fontsloth-otf-move-to geom 0.0 0.0)
    (fontsloth-otf-line-to geom 10.0 0.0)
    (fontsloth-otf-line-to geom 10.0 10.0)
    (fontsloth-otf-line-to geom 0.0 10.0)
    (fontsloth-otf-close-contour geom)
    (should (= (length (fontsloth-geometry-v-lines geom)) 2))
    ;; The horizontal top edge is skipped, so there are no m-lines.
    (should (= (length (fontsloth-geometry-m-lines geom)) 0))
    (should (= (fontsloth-geometry-area geom) 200.0))
    (let ((glyph (fontsloth-geometry-finalize geom 500.0 0.0)))
      (should (fontsloth-geometry-reverse-points geom))
      (should (= (length (fontsloth-glyph-v-lines glyph)) 2))
      (should (= (length (fontsloth-glyph-m-lines glyph)) 0))
      (should (= (fontsloth-glyph-advance-width glyph) 500.0))
      (let ((bounds (fontsloth-glyph-bounds glyph)))
        (should (= (fontsloth-glyph-outline-bounds-xmin bounds) 0.0))
        (should (= (fontsloth-glyph-outline-bounds-ymin bounds) 0.0))
        (should (= (fontsloth-glyph-outline-bounds-width bounds) 10.0))
        (should (= (fontsloth-glyph-outline-bounds-height bounds) 10.0))))))

(provide 'fontsloth-geometry-test)
;;; fontsloth-geometry-test.el ends here
