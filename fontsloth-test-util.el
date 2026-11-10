;;; fontsloth-test-util.el --- Utilities for fontsloth tests -*- lexical-binding: t -*-

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

;; fontsloth-test-util.el:
;; Utilities for fontsloth tests.
;;

;;; Code:

(require 'cl-lib)


(defvar fontsloth-test--font-directory
  (or (getenv "FONTSLOTH_TEST_FONT_DIR")
      (expand-file-name
       "test/fonts"
       (file-name-directory
        (or (locate-library "fontsloth-test-util")
            load-file-name
            buffer-file-name))))
  "Directory containing the font fixtures used by the test suite.
Override with the FONTSLOTH_TEST_FONT_DIR environment variable.")

(defun fontsloth-test--fixture-font (name)
  "Return the absolute path of fixture font NAME.
Tests should skip when the returned path does not exist."
  (expand-file-name name fontsloth-test--font-directory))

(defun fontsloth-test--fixture-font-p (name)
  "Return non-nil if fixture font NAME exists."
  (file-exists-p (fontsloth-test--fixture-font name)))

(defun fontsloth-test--zlib-available-p ()
  "Return non-nil when Emacs has zlib decompression support."
  (and (fboundp 'zlib-available-p)
       (zlib-available-p)))

(defun fontsloth-test--fixture-supported-p (name)
  "Return non-nil when fixture NAME can be loaded in this Emacs."
  (or (not (string-suffix-p ".woff" name))
      (fontsloth-test--zlib-available-p)))

(defun fontsloth-test--supported-fixtures (names)
  "Return the fixtures in NAMES supported by this Emacs."
  (cl-remove-if-not #'fontsloth-test--fixture-supported-p names))

(defun fontsloth-test--format-number (n)
  "Format N canonically for outline comparisons."
  (format "%.6f" (if (< (abs n) 1.0e-9) 0.0 n)))

(defun fontsloth-test--line-signature (line)
  "Return a canonical signature for LINE."
  (let ((coords (fontsloth-line-coords line)))
    (mapcar #'fontsloth-test--format-number
            (list (fontsloth-coords-x0 coords)
                  (fontsloth-coords-y0 coords)
                  (fontsloth-coords-x1 coords)
                  (fontsloth-coords-y1 coords)))))

(defun fontsloth-test--glyph-signature (glyph)
  "Return a canonical signature for GLYPH."
  (let ((bounds (fontsloth-glyph-bounds glyph)))
    (list :v (mapcar #'fontsloth-test--line-signature
                     (fontsloth-glyph-v-lines glyph))
          :m (mapcar #'fontsloth-test--line-signature
                     (fontsloth-glyph-m-lines glyph))
          :bounds (mapcar #'fontsloth-test--format-number
                          (list (fontsloth-glyph-outline-bounds-xmin bounds)
                                (fontsloth-glyph-outline-bounds-ymin bounds)
                                (fontsloth-glyph-outline-bounds-width bounds)
                                (fontsloth-glyph-outline-bounds-height bounds)))
          :advance (mapcar #'fontsloth-test--format-number
                           (list (fontsloth-glyph-advance-width glyph)
                                 (fontsloth-glyph-advance-height glyph))))))

(defun fontsloth-test--glyph-hash (glyph)
  "Return a stable SHA-1 of GLYPH's outline signature."
  (secure-hash 'sha1 (prin1-to-string (fontsloth-test--glyph-signature glyph))))

(defun fontsloth-test--glyph-ids (char-to-glyph)
  "Return the sorted unique glyph ids in CHAR-TO-GLYPH."
  (let ((seen (make-hash-table :test 'eq))
        (ids nil))
    (maphash (lambda (_code-point glyph-id)
               (unless (gethash glyph-id seen)
                 (puthash glyph-id t seen)
                 (push glyph-id ids)))
             char-to-glyph)
    (sort ids #'<)))

(defun fontsloth-test--outline-signatures (font)
  "Return an alist of glyph id to outline signature for FONT."
  (let ((glyphs (fontsloth-font-all-glyphs font)))
    (cl-loop for glyph-id in (fontsloth-test--glyph-ids
                              (fontsloth-font-char-to-glyph font))
             for glyph = (aref glyphs glyph-id)
             when glyph
             collect (cons glyph-id (fontsloth-test--glyph-signature glyph)))))

(defun fontsloth-test--outline-signatures-from-data (path)
  "Outline PATH from an explicit parser object and return signatures."
  (let* ((data (fontsloth-otf-load-font-data path))
         (settings (fontsloth-font-settings-create))
         (scale (fontsloth-font-settings-scale settings))
         (units-per-em (fontsloth-otf-units-per-em data))
         (ids (fontsloth-test--glyph-ids
               (fontsloth-otf-char-to-glyph-map data))))
    (cl-loop for glyph-id in ids
             collect (cons glyph-id
                           (fontsloth-test--glyph-signature
                            (fontsloth-glyph-create glyph-id scale
                                                    units-per-em data))))))

(defun fontsloth-test--raster-signature (font glyph-id px)
  "Return a canonical raster signature for GLYPH-ID at PX."
  (let* ((result (fontsloth-font-rasterize font glyph-id px))
         (metrics (fontsloth-metrics+pixmap-metrics result))
         (pixmap (fontsloth-metrics+pixmap-pixmap result)))
    (list (list (fontsloth-metrics-width metrics)
                (fontsloth-metrics-height metrics)
                (fontsloth-metrics-offset-x metrics)
                (fontsloth-metrics-offset-y metrics)
                (fontsloth-metrics-advance-width metrics)
                (fontsloth-metrics-advance-height metrics))
          (secure-hash 'sha1
                       (apply #'unibyte-string (append pixmap nil))))))

(provide 'fontsloth-test-util)
;;; fontsloth-test-util.el ends here
