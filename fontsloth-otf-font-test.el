;;; fontsloth-otf-font-test.el --- Tests for explicit font loading -*- lexical-binding: t -*-

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

;; fontsloth-otf-font-test.el:
;; Tests for explicit `fontsloth-otf-font' loading and accessors.
;;

;;; Code:

(require 'ert)
(require 'fontsloth)
(require 'fontsloth-geometry)
(require 'fontsloth-test-util)

(defvar fontsloth-otf-font-test--path
  (fontsloth-test--fixture-font "fontsloth-test.ttf"))

(ert-deftest fontsloth-otf-font-test-load-data ()
  "Check that loading returns an explicit object and accessors use it."
  (skip-unless (file-exists-p fontsloth-otf-font-test--path))
  (let ((before fontsloth-otf--current-tables))
    (let ((font (fontsloth-otf-load-font-data fontsloth-otf-font-test--path)))
      (should (fontsloth-otf-font-p font))
      (should (hash-table-p (fontsloth-otf-font-tables font)))
      (should-not (fontsloth-otf-font-bytes font))
      (should (stringp (fontsloth-otf-font-hash font)))
      (should (equal (fontsloth-otf-font-hash font)
                     (fontsloth-font-hash
                      (fontsloth-load-font fontsloth-otf-font-test--path
                                           :cache 'bypass))))
      (should (= (fontsloth-otf-num-glyphs font) 7))
      (should (= (fontsloth-otf-units-per-em font) 1000))
      (should (= (fontsloth-otf-ascender font) 800))
      (should (= (fontsloth-otf-descender font) -200))
      (should (= (fontsloth-otf-glyph-hor-advance 2 font) 600))
      (should (= (fontsloth-otf-glyph-id-for-code-point ?A font) 2))
      (should (= (gethash ?A (fontsloth-otf-char-to-glyph-map font)) 2))
      ;; Loading explicitly must not leak into the dynamic context.
      (should (eq fontsloth-otf--current-tables before)))))

(ert-deftest fontsloth-otf-font-test-outline-explicit ()
  "Check outlining with an explicit font argument."
  (skip-unless (file-exists-p fontsloth-otf-font-test--path))
  (let ((font (fontsloth-otf-load-font-data fontsloth-otf-font-test--path))
        (geom (fontsloth-geometry-create 1.0 1000)))
    (fontsloth-otf-outline-glyph 2 geom font)
    (should (> (+ (length (fontsloth-geometry-v-lines geom))
                  (length (fontsloth-geometry-m-lines geom)))
               0))))

(ert-deftest fontsloth-otf-font-test-with-font-nil ()
  "Check that `fontsloth-otf-with-font' preserves a nil context."
  (let ((fontsloth-otf--current-tables 'sentinel)
        (fontsloth-otf--current-font-bytes 'sentinel))
    (fontsloth-otf-with-font nil
      (should (eq fontsloth-otf--current-tables 'sentinel))
      (should (eq fontsloth-otf--current-font-bytes 'sentinel)))))

(ert-deftest fontsloth-otf-font-test-compat-load-font ()
  "Check that the legacy loader still returns tables and sets the context."
  (skip-unless (file-exists-p fontsloth-otf-font-test--path))
  (let ((tables (fontsloth-otf-load-font fontsloth-otf-font-test--path)))
    (should (hash-table-p tables))
    (should (eq tables fontsloth-otf--current-tables))
    (should (fontsloth-otf-font-p fontsloth-otf--current-font))
    (should (= (fontsloth-otf-num-glyphs) 7))))

(ert-deftest fontsloth-otf-font-test-high-level-no-leak ()
  "Check that the high-level loader does not leave the dynamic context set."
  (skip-unless (file-exists-p fontsloth-otf-font-test--path))
  (let ((before fontsloth-otf--current-tables))
    (fontsloth-load-font fontsloth-otf-font-test--path :cache 'bypass)
    (should (eq fontsloth-otf--current-tables before))))

(provide 'fontsloth-otf-font-test)
;;; fontsloth-otf-font-test.el ends here
