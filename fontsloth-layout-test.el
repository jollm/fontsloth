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

(ert-deftest fontsloth-layout-test-reset-settings ()
  "Check settings propagation and default line state."
  (let ((layout (fontsloth-layout-create))
        (settings (fontsloth-layout-settings-create
                   :x 5.0 :y 7.0 :max-width 100 :max-height 200
                   :horizontal-align 'center :vertical-align 'middle)))
    (fontsloth-layout-reset layout settings)
    (should (= (fontsloth-layout-x layout) 5.0))
    (should (= (fontsloth-layout-y layout) 7.0))
    (should (= (fontsloth-layout-max-width layout) 100))
    (should (= (fontsloth-layout-max-height layout) 200))
    (should (= (fontsloth-layout-horizontal-align layout) 0.5))
    (should (= (fontsloth-layout-vertical-align layout) 0.5))
    (should (= (fontsloth-layout-linebreak-data-bits
                (fontsloth-layout-wrap-mask layout))
               3))
    (should (= (fontsloth-layout-lines layout) 1))
    (should (= (fontsloth-layout-current-height layout) 0.0))))

(ert-deftest fontsloth-layout-test-clear ()
  "Check that clearing resets glyphs, output, and line state."
  (skip-unless (f-exists-p fontsloth-layout-test--font))
  (let ((font (fontsloth-load-font fontsloth-layout-test--font :cache 'bypass))
        (layout (fontsloth-layout-create)))
    (fontsloth-layout-reset layout (fontsloth-layout-settings-create))
    (fontsloth-layout-append layout `(,font)
                             (fontsloth-layout-text-style-create
                              :text "Hello" :px 35.0 :font-index 0))
    (should (> (fontsloth-layout-current-height layout) 0.0))
    (should (= (fontsloth-layout-lines layout) 1))
    (fontsloth-layout-clear layout)
    (should (null (fontsloth-layout-glyphs layout)))
    (should (null (fontsloth-layout-output layout)))
    (should (= (fontsloth-layout-lines layout) 1))
    (should (= (fontsloth-layout-current-height layout) 0.0))))

(ert-deftest fontsloth-layout-test-empty-text ()
  "Check that appending empty text is a no-op."
  (skip-unless (f-exists-p fontsloth-layout-test--font))
  (let ((font (fontsloth-load-font fontsloth-layout-test--font :cache 'bypass))
        (layout (fontsloth-layout-create)))
    (fontsloth-layout-reset layout (fontsloth-layout-settings-create))
    (fontsloth-layout-append layout `(,font)
                             (fontsloth-layout-text-style-create
                              :text "" :px 35.0 :font-index 0))
    (should (= (fontsloth-layout-lines layout) 1))
    (should-not (fontsloth-layout-finalize layout))))

(ert-deftest fontsloth-layout-test-missing-glyph ()
  "Check that code points without a glyph are skipped."
  (skip-unless (f-exists-p fontsloth-layout-test--font))
  (let ((font (fontsloth-load-font fontsloth-layout-test--font :cache 'bypass))
        (layout (fontsloth-layout-create)))
    (fontsloth-layout-reset layout (fontsloth-layout-settings-create))
    (should-not (fontsloth-font-glyph-id font ?→))
    (fontsloth-layout-append layout `(,font)
                             (fontsloth-layout-text-style-create
                              :text "→" :px 35.0 :font-index 0))
    (should (= (fontsloth-layout-lines layout) 1))
    (should-not (fontsloth-layout-finalize layout))))

(ert-deftest fontsloth-layout-test-kerning ()
  "Check that horizontal kerning shifts the second glyph."
  (skip-unless (f-exists-p fontsloth-layout-test--font))
  (let* ((font (fontsloth-load-font fontsloth-layout-test--font :cache 'bypass))
         (kern (fontsloth-font-horizontal-kern-by-code-point
                font ?& ?δ 1000.0))
         (positions
          (cl-loop
           for kern? in '(nil t)
           collect
           (let ((layout (fontsloth-layout-create)))
             (fontsloth-layout-reset layout (fontsloth-layout-settings-create))
             (fontsloth-layout-append
              layout `(,font)
              (fontsloth-layout-text-style-create
               :text "&δ" :px 1000.0 :font-index 0
               :horizontal-kern? kern?))
             (let ((output (fontsloth-layout-finalize layout)))
               (list (fontsloth-layout-glyph-position-x (elt output 0))
                     (fontsloth-layout-glyph-position-x (elt output 1))))))))
    (should (= kern -5.0))
    (should (= (- (elt (elt positions 1) 1)
                  (elt (elt positions 0) 1))
               kern))))

(provide 'fontsloth-layout-test)

;;; fontsloth-layout-test.el ends here
