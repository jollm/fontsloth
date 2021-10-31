;;; fontsloth-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.2
;; Package-Requires: ((fontsloth "0.15.2") (emacs "26.1"))
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

;; fontsloth-test.el:
;; Tests for fontsloth.el
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)

(defvar fontsloth-test--font "/usr/share/fonts/TTF/fontawesome.ttf")
(defvar fontsloth-test--expected-pixmap
  [0 0 0 0 29 81 105 101 69 14 0 0 0 0 0 1
     91 204 255 255 255 255 255 250 175 53 0 0
     22 192 255 248 169 105 77 82 119 195 255 254
     140 3 132 255 172 42 109 181 214 208 164 80
     61 214 254 61 3 75 75 234 255 254 226 231 255
     255 203 44 67 0 0 0 156 243 123 39 65 56 43
     165 253 85 0 0 0 0 2 28 145 250 255 255 235
     90 33 0 0 0 0 0 0 0 164 211 123 139 234 95 0
     0 0 0 0 0 0 0 1 25 164 134 4 0 0 0 0 0 0 0 0
     0 0 3 159 99 0 0 0 0 0 0])

    ;;; don't change the current state of the cache

(defvar fontsloth-test--post-invalidate? nil)

;; e.g. emacs -batch -l ert -l <test-file> --eval
;; "(ert-run-tests-batch-and-exit fontsloth-test--order)"
(defvar fontsloth-test--order '(member fontsloth-test-font-load-rasterize
                                       fontsloth-test-font-pcache-rasterize))

(defun fontsloth-test--pre-fixture (body)
  (unwind-protect
      (progn (setq fontsloth-test--post-invalidate?
                   (not (pcache-has fontsloth-pcache fontsloth-test--font)))
             (funcall body))))

(defun fontsloth-test--post-fixture (body)
  (unwind-protect (funcall body)
    (when fontsloth-test--post-invalidate?
      (pcache-invalidate fontsloth-pcache fontsloth-test--font))))

(ert-deftest fontsloth-test-font-load-rasterize ()
  "Test loading a font and then rasterizing a glyph."
  (fontsloth-test--pre-fixture
   (lambda ()
     (skip-unless (f-exists-p fontsloth-test--font))
     (pcase-let* ((font (fontsloth-load-font fontsloth-test--font
                                             :cache 'reload))
                  ((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                   (fontsloth-font-rasterize font 477 12.0)))
       (should (eq (fontsloth-metrics-width metrics) 14))
       (should (eq (fontsloth-metrics-height metrics) 10))
       (should (equal pixmap fontsloth-test--expected-pixmap))))))

(ert-deftest fontsloth-test-font-pcache-rasterize ()
  "Test rasterizing a glyph from a cached font."
  (fontsloth-test--post-fixture
   (lambda ()
     (skip-unless (f-exists-p fontsloth-test--font))
     (pcase-let* ((font (fontsloth-load-font fontsloth-test--font))
                  ((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                   (fontsloth-font-rasterize font 477 12.0)))
       (should (eq (fontsloth-metrics-width metrics) 14))
       (should (eq (fontsloth-metrics-height metrics) 10))
       (should (equal pixmap fontsloth-test--expected-pixmap))))))

(provide 'fontsloth-test)
;;; fontsloth-test.el ends here
