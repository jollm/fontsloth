;;; fontsloth-raster-test.el --- Tests for the rasterizer -*- lexical-binding: t -*-

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

;; fontsloth-raster-test.el:
;; Tests for raster construction and output conversion.
;;

;;; Code:

(require 'ert)
(require 'seq)
(require 'fontsloth-raster)

(ert-deftest fontsloth-raster-create ()
  "Check raster dimensions and canvas allocation."
  (let ((raster (fontsloth-raster-create 2 3)))
    (should (= (fontsloth-raster-width raster) 2))
    (should (= (fontsloth-raster-height raster) 3))
    (should (= (length (fontsloth-raster-canvas raster)) 9))
    (should (seq-every-p #'zerop (fontsloth-raster-canvas raster)))))

(ert-deftest fontsloth-raster-fract ()
  "Check fractional part extraction."
  (should (= (fontsloth-raster-fract 1.25) 0.25))
  (should (= (fontsloth-raster-fract 2.0) 0.0))
  ;; truncate rounds toward zero, not toward negative infinity.
  (should (= (fontsloth-raster-fract -1.25) -0.25)))

(ert-deftest fontsloth-raster-add ()
  "Check that adding coverage splits between two canvas cells."
  (let ((raster (fontsloth-raster-create 2 3)))
    (fontsloth-raster-add raster 1 2.0 0.25)
    (should (= (aref (fontsloth-raster-canvas raster) 1) 1.5))
    (should (= (aref (fontsloth-raster-canvas raster) 2) 0.5))))

(ert-deftest fontsloth-raster-get-pixel ()
  "Check coverage accumulation, scaling, and clamping."
  (let ((raster (make-fontsloth-raster
                 :width 3 :height 1 :canvas (vector 0.0 0.5 -1.0))))
    (should (equal (fontsloth-raster-get-pixel raster 3) [0 127 127])))
  (should (equal (fontsloth-raster-get-pixel
                  (make-fontsloth-raster
                   :width 1 :height 1 :canvas (vector 10.0))
                  1)
                 [255]))
  (should (equal (fontsloth-raster-get-pixel
                  (make-fontsloth-raster
                   :width 1 :height 1 :canvas (vector -0.001))
                  1)
                 [0])))

(ert-deftest fontsloth-raster-npbm ()
  "Check NetPBM headers and byte output."
  (let ((pixels [0 255]))
    (dolist (case '(("P5\n" . pgm) ("P6\n" . ppm) ("P4\n" . pbm)))
      (let ((out (fontsloth-raster-npbm pixels 2 1 (cdr case))))
        (should (equal (substring out 0 3) (car case)))
        (should (equal (substring out 3 11) "2 1\n255\n"))
        (should (= (aref out 11) 0))
        (should (= (aref out 12) 255))))))

(provide 'fontsloth-raster-test)
;;; fontsloth-raster-test.el ends here
