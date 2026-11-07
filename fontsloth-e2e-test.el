;;; fontsloth-e2e-test.el --- End-to-end font regression tests -*- lexical-binding: t -*-

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

;; fontsloth-e2e-test.el:
;; Load every fixture, rasterize every mapped glyph, and check that
;; malformed input fails instead of hanging or returning nonsense.
;;

;;; Code:

(require 'ert)
(require 'seq)
(require 'fontsloth)
(require 'fontsloth-test-util)

(defvar fontsloth-e2e-test--fixtures
  '("fontsloth-test.ttf"
    "fontsloth-test.otf"
    "fontsloth-test.woff"
    "IBMPlexSerif-Regular-subset.otf"))

(defun fontsloth-e2e-test--check-font (path)
  "Load PATH and sanity-check metrics and rasters for every mapped glyph."
  (let ((font (fontsloth-load-font path :cache 'bypass))
        (code-points nil))
    (setq code-points (fontsloth-font-code-points font))
    (should code-points)
    (should (equal code-points (sort (copy-sequence code-points) #'<)))
    (dolist (code-point code-points)
      (let ((glyph-id (fontsloth-font-glyph-id font code-point)))
        (should glyph-id)
        (pcase-let* (((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                      (fontsloth-font-rasterize font glyph-id 12.0))
                     ((cl-struct fontsloth-metrics width height
                                 advance-width advance-height)
                      metrics))
          (should (integerp width))
          (should (integerp height))
          (should (>= width 0))
          (should (>= height 0))
          (should (numberp advance-width))
          (should (numberp advance-height))
          (should (= (length pixmap) (* width height)))
          (should (seq-every-p (lambda (pixel)
                                 (and (integerp pixel)
                                      (<= 0 pixel 255)))
                               pixmap)))))))

(ert-deftest fontsloth-e2e-test-all-fixtures ()
  "Outline, measure, and rasterize every glyph in every fixture."
  (dolist (name fontsloth-e2e-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (file-exists-p path))
      (fontsloth-e2e-test--check-font path))))

(ert-deftest fontsloth-e2e-test-code-point-ranges ()
  "Check code points and ranges for the synthetic TTF fixture."
  (let ((font (fontsloth-load-font
               (fontsloth-test--fixture-font "fontsloth-test.ttf")
               :cache 'bypass)))
    (should (equal (fontsloth-font-code-points font)
                   '(32 65 66 180 193 128512)))
    (should (equal (fontsloth-font-code-point-ranges font)
                   '((32 33) (65 67) (180 181) (193 194) (128512 128513))))))

(ert-deftest fontsloth-e2e-test-scale-and-line-metrics ()
  "Check scale factor, computed px, and scaled line metrics."
  (let ((font (fontsloth-load-font
               (fontsloth-test--fixture-font "fontsloth-test.ttf")
               :cache 'bypass)))
    (should (= (fontsloth-font-scale-factor font 500.0) 0.5))
    (should (= (fontsloth-font-compute-px font 1000.0) 1000.0))
    (let ((metrics (fontsloth-scale-horizontal-line-metrics font 500.0)))
      (should (= (fontsloth-line-metrics-ascent metrics) 400.0))
      (should (= (fontsloth-line-metrics-descent metrics) -100.0))
      (should (= (fontsloth-line-metrics-line-gap metrics) 0.0))
      (should (= (fontsloth-line-metrics-new-line-size metrics) 500.0)))))

(ert-deftest fontsloth-e2e-test-empty-font ()
  "Check that an empty font file fails cleanly."
  (let ((path (make-temp-file "fontsloth-e2e-empty-" nil ".ttf")))
    (unwind-protect
        (progn
          (write-region "" nil path nil 'silent)
          (should-error (fontsloth-load-font path :cache 'bypass)))
      (delete-file path))))

(ert-deftest fontsloth-e2e-test-truncated-font ()
  "Check that a truncated font file fails cleanly."
  (let ((path (make-temp-file "fontsloth-e2e-truncated-" nil ".ttf"))
        (source (fontsloth-test--fixture-font "fontsloth-test.ttf")))
    (unwind-protect
        (progn
          (with-temp-file path
            (set-buffer-multibyte nil)
            (insert-file-contents-literally source nil 0 20))
          (should-error (fontsloth-load-font path :cache 'bypass)))
      (delete-file path))))

(provide 'fontsloth-e2e-test)
;;; fontsloth-e2e-test.el ends here
