;;; fontsloth-otf-glyf-test.el --- Tests for glyf composites and transforms -*- lexical-binding: t -*-

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

;; fontsloth-otf-glyf-test.el:
;; Tests for composite glyf transforms, component arguments, and
;; recursive composite outlining.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-geometry)
(require 'fontsloth-otf)
(require 'fontsloth-otf-glyf)
(require 'fontsloth-test-util)

(defun fontsloth-otf-glyf-test--u16 (value)
  "Return VALUE as a big-endian uint16 list."
  (list (logand (ash value -8) #xff) (logand value #xff)))

(defun fontsloth-otf-glyf-test--f2dot14 (value)
  "Return VALUE (a fraction) encoded as F2Dot14 bytes."
  (fontsloth-otf-glyf-test--u16
   (logand (round (* value 16384.0)) #xffff)))

(ert-deftest fontsloth-otf-glyf-test-transform-calculations ()
  "Check transform defaults, application, and composition."
  (let ((identity (fontsloth-otf-glyf-transform-create)))
    (should (fontsloth-otf-glyf-transform-default-p identity))
    (should (equal (fontsloth-otf-glyf-transform-apply-to identity 3 4)
                   '(3.0 4.0))))
  (let ((translate (fontsloth-otf-glyf-transform-create :e 10.0 :f 20.0)))
    (should-not (fontsloth-otf-glyf-transform-default-p translate))
    (should (equal (fontsloth-otf-glyf-transform-apply-to translate 3 4)
                   '(13.0 24.0))))
  (let ((scale (fontsloth-otf-glyf-transform-create :a 2.0 :d 3.0)))
    (should (equal (fontsloth-otf-glyf-transform-apply-to scale 3 4)
                   '(6.0 12.0))))
  (let ((rotate (fontsloth-otf-glyf-transform-create :a 0.0 :b 1.0
                                                     :c -1.0 :d 0.0)))
    (should (equal (fontsloth-otf-glyf-transform-apply-to rotate 3 4)
                   '(-4.0 3.0))))
  ;; Compose "translate by (10 20)" with "scale by 2".
  (let* ((translate (fontsloth-otf-glyf-transform-create :e 10.0 :f 20.0))
         (scale (fontsloth-otf-glyf-transform-create :a 2.0 :d 2.0))
         (combined (fontsloth-otf-glyf-transform-combine translate scale)))
    (should (equal (fontsloth-otf-glyf-transform-apply-to combined 1 1)
                   '(12.0 22.0))))
  (let ((identity (fontsloth-otf-glyf-transform-create))
        (scale (fontsloth-otf-glyf-transform-create :a 2.0 :d 2.0)))
    (should (equal (fontsloth-otf-glyf-transform-combine identity scale)
                   scale))))

(ert-deftest fontsloth-otf-glyf-test-component-arguments ()
  "Check component argument spec selection."
  (let ((words `((args-are-words . t) (args-are-xy . t))))
    (should (= (bindat-unpack
                (fontsloth-otf--make-component-argument-spec words)
                [255 254])
               -2)))
  (let ((u16 `((args-are-words . t) (args-are-xy . nil))))
    (should (= (bindat-unpack
                (fontsloth-otf--make-component-argument-spec u16)
                [0 2])
               2)))
  (let ((s8 `((args-are-words . nil) (args-are-xy . t))))
    (should (= (bindat-unpack
                (fontsloth-otf--make-component-argument-spec s8)
                [254])
               -2)))
  (let ((u8 `((args-are-words . nil) (args-are-xy . nil))))
    (should (= (bindat-unpack
                (fontsloth-otf--make-component-argument-spec u8)
                [254])
               254))))

(ert-deftest fontsloth-otf-glyf-test-transform-options ()
  "Check transform option spec selection and F2Dot14 decoding."
  (let ((identity (bindat-unpack
                   (fontsloth-otf--make-transform-option-spec nil)
                   [])))
    (should (= (alist-get 'a identity) 1.0))
    (should (= (alist-get 'b identity) 0.0))
    (should (= (alist-get 'c identity) 0.0))
    (should (= (alist-get 'd identity) 1.0)))
  (let* ((flags '((we-have-a-scale . t)))
         (bytes (fontsloth-otf-glyf-test--f2dot14 0.5))
         (transform (bindat-unpack
                     (fontsloth-otf--make-transform-option-spec flags)
                     (apply #'unibyte-string bytes))))
    (should (= (alist-get 'a transform) 0.5))
    (should (= (alist-get 'b transform) 0.0))
    (should (= (alist-get 'c transform) 0.0))
    (should (= (alist-get 'd transform) 0.5)))
  (let* ((flags '((we-have-an-xy-scale . t)))
         (bytes (append (fontsloth-otf-glyf-test--f2dot14 1.0)
                        (fontsloth-otf-glyf-test--f2dot14 0.5)))
         (transform (bindat-unpack
                     (fontsloth-otf--make-transform-option-spec flags)
                     (apply #'unibyte-string (append bytes nil)))))
    (should (= (alist-get 'a transform) 1.0))
    (should (= (alist-get 'd transform) 0.5)))
  (let* ((flags '((we-have-a-two-by-two . t)))
         (bytes (append (fontsloth-otf-glyf-test--f2dot14 1.0)
                        (fontsloth-otf-glyf-test--f2dot14 0.0)
                        (fontsloth-otf-glyf-test--f2dot14 0.0)
                        (fontsloth-otf-glyf-test--f2dot14 0.5)))
         (transform (bindat-unpack
                     (fontsloth-otf--make-transform-option-spec flags)
                     (apply #'unibyte-string (append bytes nil)))))
    (should (= (alist-get 'a transform) 1.0))
    (should (= (alist-get 'b transform) 0.0))
    (should (= (alist-get 'c transform) 0.0))
    (should (= (alist-get 'd transform) 0.5))))

(ert-deftest fontsloth-otf-glyf-test-composite-outline ()
  "Check that a composite fixture outlines both components."
  (let* ((path (fontsloth-test--fixture-font "fontsloth-test.ttf"))
         (font (fontsloth-otf-load-font-data path))
         (glyph-id (fontsloth-otf-glyph-id-for-code-point ?Á font))
         (geom (fontsloth-geometry-create 1.0 1000)))
    (skip-unless (and font glyph-id))
    (fontsloth-otf-outline-glyph glyph-id geom font)
    (let ((bounds (fontsloth-geometry-effective-bounds geom)))
      ;; A is 700 units high; the acute accent reaches 900.
      (should (= (fontsloth-bbox-ymax bounds) 900.0))
      (should (= (fontsloth-bbox-xmin bounds) 50.0))
      (should (= (fontsloth-bbox-xmax bounds) 600.0)))))

(provide 'fontsloth-otf-glyf-test)
;;; fontsloth-otf-glyf-test.el ends here
