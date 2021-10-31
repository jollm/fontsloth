;;; fontsloth-layout-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
;; Homepage: https://github.com/jollm/fontsloth
;; Package-Requires: ((fontsloth "0.15.3") (emacs "26.1"))
;; Keywords: data, font, ttf, otf

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

;; fontsloth-layout-test.el:
;; Tests for fontsloth-layout.el
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth-layout)

(defvar fontsloth-layout-test--font "/usr/share/fonts/TTF/Roboto-Regular.ttf")

(cl-defun fontsloth-layout-test--verify-glyph-position
    (pos &key ekey eparent ex ey ewidth eheight echar-data euser-data)
  "Check POS against expected values.
EKEY expected key
EPARENT expected parent
EX EY expected x,y
EWIDTH EHEIGHT expected width and height
ECHAR-DATA expected char data
EUSER-DATA expected user data"
  (pcase-let (((cl-struct fontsloth-layout-glyph-position
                          key parent x y width height char-data user-data)
               pos))
    (should (equal key ekey))
    (should (eq parent eparent))
    (should (eql x ex))
    (should (eql y ey))
    (should (eq width ewidth))
    (should (eq height eheight))
    (should (equal char-data echar-data))
    (should (equal user-data euser-data))))

(ert-deftest fontsloth-layout-test-simple ()
  "Simple hello world test for layout."
  (skip-unless (f-exists-p fontsloth-layout-test--font))
  (let ((font (fontsloth-load-font fontsloth-layout-test--font :cache 'bypass))
        (layout (fontsloth-layout-create)))
    (fontsloth-layout-reset layout (fontsloth-layout-settings-create))
    (fontsloth-layout-append layout `(,font) (fontsloth-layout-text-style-create
                                              :text "Hello world!"
                                              :px 35.0 :font-index 0
                                              :horizontal-kern? nil))
    (let ((output (fontsloth-layout-finalize layout)))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 0)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 45 :px 35.0)
       :eparent ?H :ex 2.0 :ey -37.0 :ewidth 21 :eheight 25
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 1)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 74 :px 35.0)
       :eparent ?e :ex 26.0 :ey -38.0 :ewidth 17 :eheight 20
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 2)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 81 :px 35.0)
       :eparent ?l :ex 46.0 :ey -37.0 :ewidth 4 :eheight 27
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 3)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 81 :px 35.0)
       :eparent ?l :ex 54.0 :ey -37.0 :ewidth 4 :eheight 27
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 4)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 84 :px 35.0)
       :eparent ?o :ex 61.0 :ey -38.0 :ewidth 18 :eheight 20
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 5)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 5 :px 35.0)
       :eparent '? ' :ex 80.0 :ey -37.0 :ewidth 0 :eheight 0
       :echar-data (fontsloth-layout-linebreak-char-data-create :bits 1))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 6)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 92 :px 35.0)
       :eparent ?w :ex 89.0 :ey -37.0 :ewidth 26 :eheight 19
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 7)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 84 :px 35.0)
       :eparent ?o :ex 116.0 :ey -38.0 :ewidth 18 :eheight 20
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 8)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 87 :px 35.0)
       :eparent ?r :ex 137.0 :ey -37.0 :ewidth 10 :eheight 19
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 9)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 81 :px 35.0)
       :eparent ?l :ex 149.0 :ey -37.0 :ewidth 4 :eheight 27
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 10)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 73 :px 35.0)
       :eparent ?d :ex 156.0 :ey -38.0 :ewidth 17 :eheight 28
       :echar-data (fontsloth-layout-linebreak-char-data-create))
      (fontsloth-layout-test--verify-glyph-position
       (elt output 11)
       :ekey (fontsloth-layout-glyph-raster-config-create :glyph-id 6 :px 35.0)
       :eparent ?! :ex 177.0 :ey -38.0 :ewidth 5 :eheight 26
       :echar-data (fontsloth-layout-linebreak-char-data-create)))))

(provide 'fontsloth-layout-test)
;;; fontsloth-layout-test.el ends here
