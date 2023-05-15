;;; fontsloth.el --- Elisp otf/ttf font loader/renderer -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.17.0
;; Homepage: https://github.com/jollm/fontsloth
;; Package-Requires: ((f "0.20.0") (logito "0.1") (pcache "0.5") (stream "2.2.5") (emacs "28.1"))
;; Keywords: data, font, rasterization, ttf, otf

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

;; fontsloth: the slowest font renderer in the world written in pure elisp
;; inspired by fontdue, the fastest font renderer in the world, written in pure
;; rust

;; *Please see the website for a detailed README.*

;; To use this module, load and enable it as follows:
;;   (use-package fontsloth)
;;
;; If you also want layout functions (includes fontsloth):
;;   (use-package fontsloth-layout)
;;

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'map)
(require 'fontsloth--common-types)
(require 'fontsloth-cache)
(require 'fontsloth-geometry)
(require 'fontsloth-otf)
(require 'fontsloth-raster)

(defgroup fontsloth nil
  "Fontsloth is a font loader/outliner/renderer."
  :group 'display)

(cl-defstruct
    (fontsloth-metrics
     (:constructor fontsloth-metrics-create)
     (:copier nil))
  "Describe the metrics for a rendered glyph."
  (xmin 0 :type 'fixed)
  (ymin 0 :type 'fixed)
  (width 0 :type 'fixed)
  (height 0 :type 'fixed)
  (advance-width 0 :type 'fixed)
  (advance-height 0 :type 'fixed)
  (offset-x 0 :type 'fixed)
  (offset-y 0 :type 'fixed)
  (bounds (fontsloth-glyph-outline-bounds-create)
          :type 'fontsloth-glyph-outline-bounds))

(cl-defstruct
    (fontsloth-metrics+pixmap
     (:constructor fontsloth-metrics+pixmap-create)
     (:copier nil))
  "Container for raster output and associated metrics."
  (metrics (fontsloth-metrics-create) :type 'fontsloth-metrics)
  (pixmap [] :type 'vector))

(cl-defstruct (fontsloth-line-metrics
               (:constructor nil)
               (:copier nil))
  "Describe a font's line metrics."
  (ascent 0.0 :type 'number :read-only t)
  (descent 0.0 :type 'number :read-only t)
  (line-gap 0.0 :type 'number :read-only t)
  (new-line-size 0.0 :type 'number :read-only t))

(defsubst fontsloth-line-metrics-create (ascent descent line-gap)
  "Create a new `fontsloth-line-metrics'.
ASCENT the ascent
DESCENT the descent
LINE-GAP the line gap"
  (record 'fontsloth-line-metrics ascent descent line-gap
          (+ (- ascent descent) line-gap)))

(defun fontsloth-line-metrics-scale (line-metrics scale)
  "Create a new `fontsloth-line-metrics' scaled from LINE-METRICS by SCALE."
  (pcase-let (((cl-struct fontsloth-line-metrics
                          ascent descent line-gap new-line-size) line-metrics))
    (record 'fontsloth-line-metrics (* ascent scale) (* descent scale)
            (* line-gap scale) (* new-line-size scale))))

(cl-defstruct (fontsloth-font-settings
               (:constructor fontsloth-font-settings-create)
               (:copier nil))
  "Settings for a `fontsloth-font'."
  (collection-index 0 :type 'fixed)
  (scale 40.0 :type 'number))

(cl-defstruct (fontsloth-font
               (:constructor nil)
               (:copier nil))
  "Describe a font."
  (name nil :type 'string :read-only t)
  (units-per-em 1000 :type 'number :read-only t)
  (glyphs nil :type 'vector :read-only t)
  (char-to-glyph nil :type 'hash-table :read-only t)
  (horizontal-line-metrics nil :type 'fontsloth-line-metrics :read-only t)
  (horizontal-kern nil :type 'hash-table :read-only t)
  (vertical-line-metrics nil :type 'fontsloth-line-metrics :read-only t)
  (settings (fontsloth-font-settings-create) :type 'fontsloth-font-settings
            :read-only t))

(defun fontsloth--load-font (path font-settings)
  "Load a font given file path PATH and settings FONT-SETTINGS."
  (fontsloth-otf-load-font path)
  (let* ((name (f-base path))
         (char-to-glyph (fontsloth-otf-char-to-glyph-map))
         (units-per-em (fontsloth-otf-units-per-em))
         (glyph-count (fontsloth-otf-num-glyphs))
         (glyphs (let ((glyphs (make-vector glyph-count nil)))
                   (cl-loop for glyph-id being the hash-values of char-to-glyph
                            collect
                            (unless (aref glyphs glyph-id)
                              (aset glyphs glyph-id
                                    (fontsloth-glyph-create
                                     glyph-id
                                     (fontsloth-font-settings-scale
                                      font-settings)
                                     units-per-em))))
                   glyphs))
         (horizontal-line-metrics (fontsloth-line-metrics-create
                                   (fontsloth-otf-ascender)
                                   (fontsloth-otf-descender)
                                   (fontsloth-otf-line-gap)))
         (horizontal-kern (fontsloth-otf-find-hkern-mappings))
         (vertical-line-metrics nil) ; TODO: vertical line metrics
         )
    (record 'fontsloth-font name units-per-em glyphs char-to-glyph
            horizontal-line-metrics horizontal-kern
            vertical-line-metrics font-settings)))

(defun fontsloth--load-font-cached (path font-settings)
  "Retrieve a font from cache and if not load it with PATH and FONT-SETTINGS."
  (unless fontsloth-cache
    (fontsloth-cache-init))
  (if-let ((font (pcache-get fontsloth-cache path)))
      font
    (let ((font (fontsloth--load-font path font-settings)))
      (pcache-put fontsloth-cache path font)
      font)))

(cl-defun fontsloth-load-font
    (source &key (font-settings (fontsloth-font-settings-create))
            (cache t))
  "Load a font from SOURCE and return a `fontsloth-font' describing it.
Currently only file path sources are accepted.
Loading from font collections is not yet implemented.
SOURCE the font source
FONT-SETTINGS the `fontsloth-font-settings' to use
CACHE t to use cache, 'bypass to bypass cache, 'reload to force update
cache"
  (unless (and (stringp source) (f-file-p source))
    (error "Source %s with type %s currently unsupported"
           source (type-of source)))
  (cl-case cache
    ('bypass (fontsloth--load-font source font-settings))
    ('reload (pcache-invalidate fontsloth-cache source)
             (fontsloth--load-font-cached source font-settings))
    (t (fontsloth--load-font-cached source font-settings))))

(defsubst fontsloth-font-scale-factor (font px)
  "Return a scaling factor for FONT given PX pixel size."
  (/ (* 1.0 px) (fontsloth-font-units-per-em font)))

(defun fontsloth-font-compute-px (font desired-line-height)
  "Compute a pixel size for FONT given a DESIRED-LINE-HEIGHT line height."
  (let* ((line-metrics (fontsloth-font-horizontal-line-metrics font))
         (new-line-size (fontsloth-line-metrics-new-line-size line-metrics)))
    (* (/ (* 1.0 desired-line-height) new-line-size)
       (fontsloth-font-units-per-em font))))

(defun fontsloth-scale-horizontal-line-metrics (font px)
  "Return FONT's horizontal line metrics scaled to PX pixel size."
  (fontsloth-line-metrics-scale (fontsloth-font-horizontal-line-metrics font)
                                (fontsloth-font-scale-factor font px)))

(defsubst fontsloth-font-glyph-id (font code-point)
  "Return FONT's glyph mapping for CODE-POINT or nil.
FONT a `fontsloth-font'
CODE-POINT the character code point to map"
  (map-elt (fontsloth-font-char-to-glyph font) code-point))

(defun fontsloth-font-horizontal-kern-by-id (font left right px)
  "Return FONT's horizontal kern value for LEFT/RIGHT scaled to PX or 0."
  (let ((scale (fontsloth-font-scale-factor font px)))
    (* (or (when-let ((mappings (fontsloth-font-horizontal-kern font)))
             (gethash (logior (ash left 16) right) mappings))
           0)
       scale)))

(defun fontsloth-font-horizontal-kern-by-code-point
    (font left-code-point right-code-point px)
  "Return FONT's horizontal kern value for left and right scaled to PX or 0.
LEFT-CODE-POINT the char code point for the left hand side
RIGHT-CODE-POINT the char code point for the right hand side"
  (or (when-let ((left (fontsloth-font-glyph-id font left-code-point))
                 (right (fontsloth-font-glyph-id font right-code-point)))
        (fontsloth-font-horizontal-kern-by-id font left right px))
      0))

(defun fontsloth-font-metrics-raw (scale glyph offset)
  "Determine the expected metrics for a render of GLYPH at SCALE and OFFSET."
  (pcase-let* ((bounds (fontsloth-glyph-outline-bounds-scale
                        (fontsloth-glyph-bounds glyph) scale))
               ((cl-struct fontsloth-glyph-outline-bounds
                           xmin ymin width height)
                bounds)
               (offset-x (fontsloth-raster-fract (+ xmin offset)))
               (offset-y (fontsloth-raster-fract
                          (- 1.0 (fontsloth-raster-fract height)
                             (fontsloth-raster-fract ymin)))))
    (when (> 0 offset-x)
      (setf offset-x (1+ offset-x)))
    (when (> 0 offset-y)
      (setf offset-y (1+ offset-y)))
    (fontsloth-metrics-create
     :xmin (truncate (floor xmin))
     :ymin (truncate (floor ymin))
     :width (truncate (ceiling (+ width offset-x)))
     :height (truncate (ceiling (+ height offset-y)))
     :advance-width (* scale (fontsloth-glyph-advance-width glyph))
     :advance-height (* scale (fontsloth-glyph-advance-height glyph))
     :offset-x offset-x
     :offset-y offset-y
     :bounds bounds)))

(defun fontsloth-font-metrics (font glyph-id px)
  "Return FONT's `fontsloth-metrics' for GLYPH-ID at PX pixel size."
  (let ((glyph (aref (fontsloth-font-glyphs font) glyph-id))
        (scale (fontsloth-font-scale-factor font px)))
    (fontsloth-font-metrics-raw scale glyph 0.0)))

(cl-defun fontsloth-font-rasterize (font glyph-id px &optional (subpixel? nil))
  "Rasterize FONT's glyph at GLYPH-ID in PX pixel size.
Produces a greyscale pixmap suitable for PGM.  Returns
`fontsloth-metrics+pixmap'.
SUBPIXEL? optional t to produce a subpixel render suitable for PPM"
  (when-let ((glyph (elt (fontsloth-font-glyphs font) glyph-id)))
    (pcase-let* ((scale (fontsloth-font-scale-factor font px))
                 (metrics (fontsloth-font-metrics-raw scale glyph 0.0))
                 ((cl-struct fontsloth-metrics
                             width height offset-x offset-y) metrics)
                 (canvas (fontsloth-raster-create (* (if subpixel? 3 1) width)
                                                  height)))
      (fontsloth-raster-draw canvas glyph (* (if subpixel? 3.0 1.0) scale)
                             scale
                             offset-x offset-y)
      (fontsloth:verbose* fontsloth-log "bitmap size is %sx%s"
                          (fontsloth-raster-width canvas)
                          (fontsloth-raster-height canvas))
      (fontsloth-metrics+pixmap-create
       :pixmap (fontsloth-raster-get-pixel
                canvas
                (* (fontsloth-raster-width canvas)
                   (fontsloth-raster-height canvas)))
       :metrics metrics))))

(provide 'fontsloth)
;;; fontsloth.el ends here
