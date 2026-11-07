;;; fontsloth-primitives-test.el --- Tests for basic geometry types -*- lexical-binding: t -*-

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

;; fontsloth-primitives-test.el:
;; Tests for bbox, point, coords, and line primitives.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-bbox)
(require 'fontsloth-coords)
(require 'fontsloth-line)
(require 'fontsloth-point)

(ert-deftest fontsloth-bbox-extend-by ()
  "Check that extending a bbox accumulates the extents."
  (let ((bbox (fontsloth-bbox-create)))
    (should (= (fontsloth-bbox-xmin bbox) cl-most-positive-float))
    (should (= (fontsloth-bbox-ymin bbox) cl-most-positive-float))
    (should (= (fontsloth-bbox-xmax bbox) cl-most-negative-float))
    (should (= (fontsloth-bbox-ymax bbox) cl-most-negative-float))
    (should (eq bbox (fontsloth-bbox-extend-by bbox 2.0 3.0)))
    (fontsloth-bbox-extend-by bbox -1.0 5.0)
    (fontsloth-bbox-extend-by bbox 0.0 4.0)
    (should (= (fontsloth-bbox-xmin bbox) -1.0))
    (should (= (fontsloth-bbox-ymin bbox) 3.0))
    (should (= (fontsloth-bbox-xmax bbox) 2.0))
    (should (= (fontsloth-bbox-ymax bbox) 5.0))))

(ert-deftest fontsloth-point-between ()
  "Check linear interpolation between two points."
  (let ((p1 (fontsloth-point-create :x 0.0 :y 0.0))
        (p2 (fontsloth-point-create :x 10.0 :y 20.0)))
    (pcase-let ((`(,x . ,y)
                 (let ((p (fontsloth-point-between p1 p2 0.25)))
                   (cons (fontsloth-point-x p) (fontsloth-point-y p)))))
      (should (= x 2.5))
      (should (= y 5.0)))
    (should (equal (fontsloth-point-between p1 p2 0.0) p1))
    (should (equal (fontsloth-point-between p1 p2 1.0) p2))))

(ert-deftest fontsloth-point-scale ()
  "Check that scaling a point mutates it."
  (let ((p (fontsloth-point-create :x 2.0 :y -3.0)))
    (fontsloth-point-scale p 2.5)
    (should (= (fontsloth-point-x p) 5.0))
    (should (= (fontsloth-point-y p) -7.5))))

(ert-deftest fontsloth-point-distance-squared ()
  "Check squared distance computation."
  (should (= (fontsloth-point-distance-squared
              (fontsloth-point-create :x 0.0 :y 0.0)
              (fontsloth-point-create :x 3.0 :y 4.0))
             25.0))
  (should (= (fontsloth-point-distance-squared
              (fontsloth-point-create :x 3.0 :y 4.0)
              (fontsloth-point-create :x 3.0 :y 4.0))
             0.0)))

(ert-deftest fontsloth-coords-reverse ()
  "Check that reversing coords swaps the endpoints in place."
  (let ((coords (fontsloth-coords-create :x0 1.0 :y0 2.0 :x1 3.0 :y1 4.0)))
    (should (eq coords (fontsloth-coords-reverse coords)))
    (should (= (fontsloth-coords-x0 coords) 3.0))
    (should (= (fontsloth-coords-y0 coords) 4.0))
    (should (= (fontsloth-coords-x1 coords) 1.0))
    (should (= (fontsloth-coords-y1 coords) 2.0))))

(ert-deftest fontsloth-line-create-diagonal ()
  "Check line construction for a line going up and to the right."
  (let ((line (fontsloth-line-create
               (fontsloth-point-create :x 1.0 :y 2.0)
               (fontsloth-point-create :x 5.0 :y 6.0))))
    (should (= (fontsloth-coords-x0 (fontsloth-line-coords line)) 1.0))
    (should (= (fontsloth-coords-y0 (fontsloth-line-coords line)) 2.0))
    (should (= (fontsloth-coords-x1 (fontsloth-line-coords line)) 5.0))
    (should (= (fontsloth-coords-y1 (fontsloth-line-coords line)) 6.0))
    (should (= (fontsloth-adj-x-first-adj (fontsloth-line-adjustment line))
               1.0))
    (should (= (fontsloth-adj-y-first-adj (fontsloth-line-adjustment line))
               1.0))
    (should (= (fontsloth-nudge-start-x-nudge (fontsloth-line-nudge line))
               0.0))
    (should (= (fontsloth-nudge-end-x-nudge (fontsloth-line-nudge line))
               1e-13))
    (should (= (fontsloth-nudge-end-y-nudge (fontsloth-line-nudge line))
               1e-13))
    (should (= (fontsloth-params-dx (fontsloth-line-params line)) 4.0))
    (should (= (fontsloth-params-dy (fontsloth-line-params line)) 4.0))
    (should (= (fontsloth-params-tdx (fontsloth-line-params line)) 0.25))
    (should (= (fontsloth-params-tdy (fontsloth-line-params line)) 0.25))))

(ert-deftest fontsloth-line-create-down-left ()
  "Check line construction for a line going down and to the left."
  (let ((line (fontsloth-line-create
               (fontsloth-point-create :x 5.0 :y 6.0)
               (fontsloth-point-create :x 1.0 :y 2.0))))
    (should (= (fontsloth-adj-x-first-adj (fontsloth-line-adjustment line))
               0.0))
    (should (= (fontsloth-adj-y-first-adj (fontsloth-line-adjustment line))
               0.0))
    (should (= (fontsloth-nudge-start-x-nudge (fontsloth-line-nudge line))
               1e-13))
    (should (= (fontsloth-nudge-end-x-nudge (fontsloth-line-nudge line))
               0.0))
    (should (= (fontsloth-params-tdx (fontsloth-line-params line)) -0.25))
    (should (= (fontsloth-params-tdy (fontsloth-line-params line)) -0.25))))

(ert-deftest fontsloth-line-create-vertical ()
  "Check line construction for a vertical line."
  (let ((line (fontsloth-line-create
               (fontsloth-point-create :x 2.0 :y 0.0)
               (fontsloth-point-create :x 2.0 :y 10.0))))
    (should (= (fontsloth-params-tdx (fontsloth-line-params line))
               cl-most-positive-float))
    (should (= (fontsloth-params-tdy (fontsloth-line-params line)) 0.1))
    (should (= (fontsloth-adj-x-first-adj (fontsloth-line-adjustment line))
               1.0))
    (should (= (fontsloth-adj-y-first-adj (fontsloth-line-adjustment line))
               1.0))))

(ert-deftest fontsloth-line-reposition ()
  "Check repositioning against bounds, with and without reversal."
  (let* ((bounds (fontsloth-bbox-create :xmin 2.0 :ymin 0.0
                                        :xmax 12.0 :ymax 8.0))
         (line (fontsloth-line-create
                (fontsloth-point-create :x 0.0 :y 0.0)
                (fontsloth-point-create :x 10.0 :y 10.0))))
    (fontsloth-line-reposition line bounds nil)
    (should (= (fontsloth-coords-x0 (fontsloth-line-coords line)) -2.0))
    (should (= (fontsloth-coords-y0 (fontsloth-line-coords line)) 8.0))
    (should (= (fontsloth-coords-x1 (fontsloth-line-coords line)) 8.0))
    (should (= (fontsloth-coords-y1 (fontsloth-line-coords line)) 2.0)))
  (let* ((bounds (fontsloth-bbox-create :xmin 0.0 :ymin 0.0
                                        :xmax 10.0 :ymax 10.0))
         (line (fontsloth-line-create
                (fontsloth-point-create :x 0.0 :y 0.0)
                (fontsloth-point-create :x 10.0 :y 10.0))))
    (fontsloth-line-reposition line bounds t)
    (should (= (fontsloth-coords-x0 (fontsloth-line-coords line)) 10.0))
    (should (= (fontsloth-coords-y0 (fontsloth-line-coords line)) 0.0))
    (should (= (fontsloth-coords-x1 (fontsloth-line-coords line)) 0.0))
    (should (= (fontsloth-coords-y1 (fontsloth-line-coords line)) 10.0))))

(provide 'fontsloth-primitives-test)
;;; fontsloth-primitives-test.el ends here
