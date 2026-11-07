;;; fontsloth-layout-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

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

;; fontsloth-layout-test.el:
;; Tests for fontsloth-layout.el
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth-layout)
(require 'fontsloth-test-util)

(defvar fontsloth-layout-test--font
  (fontsloth-test--fixture-font "IBMPlexSerif-Regular-subset.otf"))

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
      (should (= (length output) 12))
      (cl-loop
       for pos in output
       for (parent x y width height glyph-id bits) in
       '((?H 1.0 -36.0 25 25 34 0)
         (?e 29.0 -37.0 17 20 5 0)
         (?l 48.0 -36.0 9 27 12 0)
         (?l 59.0 -36.0 9 27 12 0)
         (?o 70.0 -37.0 18 20 15 0)
         (?\s 89.0 -36.0 0 0 158 1)
         (?w 97.0 -36.0 27 19 23 0)
         (?o 125.0 -37.0 18 20 15 0)
         (?r 145.0 -36.0 15 19 18 0)
         (?l 161.0 -36.0 9 27 12 0)
         (?d 172.0 -37.0 20 28 4 0)
         (?! 194.0 -37.0 6 26 80 0))
       do
       (fontsloth-layout-test--verify-glyph-position
        pos
        :ekey (fontsloth-layout-glyph-raster-config-create
               :glyph-id glyph-id :px 35.0 :font-hash (fontsloth-font-hash font))
        :eparent parent :ex x :ey y :ewidth width :eheight height
        :echar-data (fontsloth-layout-linebreak-char-data-create :bits bits))))))

(provide 'fontsloth-layout-test)
;;; fontsloth-layout-test.el ends here
