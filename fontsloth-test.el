;;; fontsloth-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.21.0
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

;; fontsloth-test.el:
;; Tests for fontsloth.el
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-test-util)

(defvar fontsloth-test--font
  (fontsloth-test--fixture-font "fontsloth-test.ttf"))

(defvar fontsloth-test--glyph-code-point ?A)

(defvar fontsloth-test--px 12.0)

(defvar fontsloth-test--expected-pixmap
  [0 0 5 102 87 0 0 0 0 0 65 255 252 17 0 0 0 0 138 255 255 87 0 0 0 0 212 255
   255 160 0 0 0 29 255 255 255 232 1 0 0 102 255 255 255 255 51 0 0 175 255 255
   255 255 124 0 5 242 255 255 255 255 197 0 65 255 255 255 255 255 252 17])

(defvar fontsloth-test--post-invalidate? nil
  "For use in fixtures to ensure the current state of cache is left unchanged.")

;; e.g. emacs -batch -l ert -l <test-file> --eval
;; "(ert-run-tests-batch-and-exit fontsloth-test--order)"
(defvar fontsloth-test--order '(member fontsloth-test-font-load-rasterize
                                       fontsloth-test-font-pcache-rasterize
                                       fontsloth-test-font-formats
                                       fontsloth-test-font-composite-rasterize
                                       fontsloth-test-font-code-point-diff))

(defun fontsloth-test--assert-raster (font)
  "Check FONT's raster output for the fixture glyph against the golden pixmap."
  (pcase-let* ((glyph-id (fontsloth-font-glyph-id
                          font fontsloth-test--glyph-code-point))
               ((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                (fontsloth-font-rasterize font glyph-id fontsloth-test--px)))
    (should (eq (fontsloth-metrics-width metrics) 8))
    (should (eq (fontsloth-metrics-height metrics) 9))
    (should (equal pixmap fontsloth-test--expected-pixmap))))

(defun fontsloth-test--pre-fixture (body)
  "A fixture to run before BODY."
  (unwind-protect
      (progn (mkdir "./pcache")
             (setq pcache-directory "./pcache"
                   fontsloth-test--post-invalidate?
                   (not (fontsloth-cache-get fontsloth-test--font)))
             (funcall body))))

(defun fontsloth-test--post-fixture (body)
  "A fixture to run after BODY."
  (unwind-protect (funcall body)
    (when fontsloth-test--post-invalidate?
      (fontsloth-cache-invalidate fontsloth-test--font)
      (delete-directory "./pcache" t))))

(ert-deftest fontsloth-test-font-load-rasterize ()
  "Test loading a font and then rasterizing a glyph."
  (fontsloth-test--pre-fixture
   (lambda ()
     (skip-unless (f-exists-p fontsloth-test--font))
     (fontsloth-test--assert-raster
      (fontsloth-load-font fontsloth-test--font :cache 'reload)))))

(ert-deftest fontsloth-test-font-pcache-rasterize ()
  "Test rasterizing a glyph from a cached font."
  (fontsloth-test--post-fixture
   (lambda ()
     (skip-unless (f-exists-p fontsloth-test--font))
     (fontsloth-test--assert-raster
      (fontsloth-load-font fontsloth-test--font)))))

(ert-deftest fontsloth-test-font-formats ()
  "Check that TTF, CFF1 OTF, and WOFF fixtures rasterize identically."
  (dolist (name '("fontsloth-test.ttf"
                  "fontsloth-test.otf"
                  "fontsloth-test.woff"))
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (f-exists-p path))
      (fontsloth-test--assert-raster
       (fontsloth-load-font path :cache 'bypass)))))

(ert-deftest fontsloth-test-font-composite-rasterize ()
  "Check that a composite glyf can be outlined and rasterized."
  (skip-unless (f-exists-p fontsloth-test--font))
  (let* ((font (fontsloth-load-font fontsloth-test--font :cache 'bypass))
         (glyph-id (fontsloth-font-glyph-id font ?Á)))
    (should glyph-id)
    (pcase-let* (((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                  (fontsloth-font-rasterize font glyph-id fontsloth-test--px)))
      ;; The acute accent makes the composite taller than plain A.
      (should (= (fontsloth-metrics-width metrics) 8))
      (should (= (fontsloth-metrics-height metrics) 11))
      (should (= (length pixmap) 88)))))

(ert-deftest fontsloth-test-font-code-point-diff ()
  "Test `fontsloth-font-code-point-diff'."
  ;; nothing in b
  (should (null (fontsloth-font-code-point-diff '((12 35)) nil)))

  ;; nothing in a
  (should (equal (fontsloth-font-code-point-diff nil '((12 35)))
                 '((12 35))))

  ;; start of first b past the end of first a
  (should (equal (fontsloth-font-code-point-diff '((1 13) (35 78)) '((13 35)))
                 '((13 35))))

  ;; end of first b less than start of first a
  (should (equal (fontsloth-font-code-point-diff '((49 91)) '((12 35) (36 39)))
                 '((12 35) (36 39))))

  ;; end of b less than or equal to end of a and start of b less than that of a
  (should (equal (fontsloth-font-code-point-diff '((12 35)) '((1 33) (34 35)))
                 '((1 12))))

  ;; start of b less than start of a
  (should (equal (fontsloth-font-code-point-diff '((12 35)) '((1 36)))
                 '((1 12) (35 36))))

  ;; start of b greater than or equal to that of a and end of b greater than a
  (should (equal (fontsloth-font-code-point-diff '((1 13) (14 29)) '((12 35)))
                 '((13 14) (29 35))))

  ;; b is inside a
  (should (equal (fontsloth-font-code-point-diff
                  '((35 78) (79 82)) '((36 77) (78 344)))
                 '((78 79) (82 344)))))

(ert-deftest fontsloth-test-lazy-outline ()
  "Check lazy outlining matches eager outlining for a glyph and raster."
  (skip-unless (f-exists-p fontsloth-test--font))
  (let* ((lazy-settings (fontsloth-font-settings-create :lazy-outline? t))
         (eager-settings (fontsloth-font-settings-create :lazy-outline? nil))
         (lazy (fontsloth-load-font fontsloth-test--font
                                    :font-settings lazy-settings
                                    :cache 'bypass))
         (eager (fontsloth-load-font fontsloth-test--font
                                     :font-settings eager-settings
                                     :cache 'bypass))
         (glyph-id (fontsloth-font-glyph-id lazy fontsloth-test--glyph-code-point))
         (glyphs (fontsloth-font-glyphs lazy)))
    (should (fontsloth-font-settings-lazy-outline? lazy-settings))
    (should (fontsloth-font-source lazy))
    ;; The mapped glyph starts unoutlined, then is filled on first access.
    (should-not (aref glyphs glyph-id))
    (should (fontsloth-font-glyph lazy glyph-id))
    (should (aref glyphs glyph-id))
    ;; Eager mode fills mapped glyphs at load time.
    (should (aref (fontsloth-font-glyphs eager) glyph-id))
    (should (equal (fontsloth-test--outline-signatures lazy)
                   (fontsloth-test--outline-signatures eager)))
    (should (equal (fontsloth-test--raster-signature lazy glyph-id 24.0)
                   (fontsloth-test--raster-signature eager glyph-id 24.0)))
    ;; Forcing all glyphs fills the vector without changing outlines.
    (should (fontsloth-font-all-glyphs lazy))
    (should (equal (fontsloth-test--outline-signatures lazy)
                   (fontsloth-test--outline-signatures eager)))))

(ert-deftest fontsloth-test-lazy-kern-map ()
  "Check that the GPOS kern map is built on first use."
  (let ((path (fontsloth-test--fixture-font
               "IBMPlexSerif-Regular-subset.otf")))
    (skip-unless (f-exists-p path))
    (let ((font (fontsloth-load-font path :cache 'bypass)))
      (should-not (fontsloth-font-horizontal-kern font))
      (should-not (fontsloth-font-horizontal-kern-loaded? font))
      (should (= (fontsloth-font-horizontal-kern-by-code-point
                  font ?& ?δ 1000.0)
                 -5.0))
      (should (fontsloth-font-horizontal-kern-loaded? font))
      (should (hash-table-p (fontsloth-font-horizontal-kern font))))))

(ert-deftest fontsloth-test-release-parser-data ()
  "A full glyph pass releases transient parser data and can reparse."
  (let ((path (fontsloth-test--fixture-font
               "IBMPlexSerif-Regular-subset.otf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-load-font path :cache 'bypass))
           (glyph-id (fontsloth-font-glyph-id font ?&)))
      (should (gethash font fontsloth--lazy-fonts))
      (fontsloth-font-all-glyphs font)
      (should-not (gethash font fontsloth--lazy-fonts))
      ;; Existing glyphs remain available without reparsing.
      (should (aref (fontsloth-font-glyphs font) glyph-id))
      ;; A later kern request reparses from the source path.
      (should (= (fontsloth-font-horizontal-kern-by-code-point
                  font ?& ?δ 1000.0)
                 -5.0))
      (should (gethash font fontsloth--lazy-fonts)))))

(provide 'fontsloth-test)



;;; fontsloth-test.el ends here
