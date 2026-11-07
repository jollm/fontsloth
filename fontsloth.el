;;; fontsloth.el --- Elisp otf/ttf font loader/renderer -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.20.0
;; Homepage: https://github.com/jollm/fontsloth
;; Package-Requires: ((async "1.9.7") (f "0.20.0") (logito "0.1") (pcache "0.5") (emacs "28.1"))
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

(require 'async)
(require 'cl-lib)
(require 'f)
(require 'map)
(require 'pcase)
(require 'seq)
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
  (scale 40.0 :type 'number)
  (lazy-outline? t :type 'boolean
                 :documentation "Outline glyphs on first use."))

(defconst fontsloth--font-data-version 3
  "For cache invalidation if fontsloth-font changes.")

(cl-defstruct (fontsloth-font
               (:constructor nil)
               (:copier nil))
  "Describe a font."
  (-data-version fontsloth--font-data-version :type 'integer :read-only t)
  (name nil :type 'string :read-only t)
  (source nil :type 'string :read-only t
          :documentation "Path used to reload parser data for lazy outlines")
  (units-per-em 1000 :type 'number :read-only t)
  (glyphs nil :type 'vector
          :documentation "Glyph vector; nil entries are outlined on demand")
  (char-to-glyph nil :type 'hash-table :read-only t)
  (horizontal-line-metrics nil :type 'fontsloth-line-metrics :read-only t)
  (horizontal-kern nil :type 'hash-table)
  (horizontal-kern-loaded? nil :type 'boolean)
  (vertical-line-metrics nil :type 'fontsloth-line-metrics :read-only t)
  (settings (fontsloth-font-settings-create) :type 'fontsloth-font-settings
            :read-only t)
  (hash 0 :type 'fixed :read-only t))

(defvar fontsloth--lazy-fonts (make-hash-table :test 'eq :weakness 'key)
  "Weak map from `fontsloth-font' to its transient `fontsloth-otf-font'.
Keeping the parser data here rather than in the cached font avoids
serializing tables and bytes into pcache.")

(defun fontsloth--font-data (font)
  "Return FONT's parser data, reloading it from FONT's source if needed."
  (or (gethash font fontsloth--lazy-fonts)
      (let ((source (fontsloth-font-source font)))
        (unless source
          (error "Cannot outline font %s lazily: no source path"
                 (fontsloth-font-name font)))
        (let ((data (fontsloth-otf-load-font-data source)))
          (puthash font data fontsloth--lazy-fonts)
          data))))

(defun fontsloth-font-glyph (font glyph-id)
  "Return FONT's GLYPH-ID, outlining it on first use.
Returns nil when GLYPH-ID is out of range or nil."
  (when (and glyph-id
             (integerp glyph-id)
             (<= 0 glyph-id)
             (< glyph-id (length (fontsloth-font-glyphs font))))
    (let ((glyphs (fontsloth-font-glyphs font)))
      (if (not (fontsloth-font-settings-lazy-outline?
                (fontsloth-font-settings font)))
          (aref glyphs glyph-id)
        (or (aref glyphs glyph-id)
            (let ((glyph (fontsloth-glyph-create
                          glyph-id
                          (fontsloth-font-settings-scale
                           (fontsloth-font-settings font))
                          (fontsloth-font-units-per-em font)
                          (fontsloth--font-data font))))
              (aset glyphs glyph-id glyph)
              glyph))))))

(defun fontsloth-font-kern-map (font)
  "Return FONT's horizontal kern mapping, building it on first use."
  (unless (fontsloth-font-horizontal-kern-loaded? font)
    (setf (fontsloth-font-horizontal-kern font)
          (fontsloth-otf-find-hkern-mappings (fontsloth--font-data font))
          (fontsloth-font-horizontal-kern-loaded? font) t))
  (fontsloth-font-horizontal-kern font))

(defun fontsloth-font-all-glyphs (font)
  "Outline every mapped glyph in FONT and return its glyph vector.
Useful for tests and benchmarks that need the eager representation."
  (let ((seen (make-hash-table :test 'eq)))
    (maphash (lambda (_code-point glyph-id)
               (unless (gethash glyph-id seen)
                 (puthash glyph-id t seen)
                 (fontsloth-font-glyph font glyph-id)))
             (fontsloth-font-char-to-glyph font)))
  (fontsloth-font-glyphs font))

(defun fontsloth--load-font (path font-settings)
  "Load a font given file path PATH and settings FONT-SETTINGS."
  (let* ((data (fontsloth-otf-load-font-data path))
         (source (expand-file-name path)))
    (fontsloth-otf-with-font data
      (let* ((name (f-base path))
             (char-to-glyph (fontsloth-otf-char-to-glyph-map data))
             (units-per-em (fontsloth-otf-units-per-em data))
             (glyph-count (fontsloth-otf-num-glyphs data))
             (glyphs (make-vector glyph-count nil))
             (horizontal-line-metrics
              (fontsloth-line-metrics-create
               (fontsloth-otf-ascender data)
               (fontsloth-otf-descender data)
               (fontsloth-otf-line-gap data)))
             (horizontal-kern nil)
             (vertical-line-metrics nil)) ; TODO: vertical line metrics
        (let ((font (record 'fontsloth-font
                            fontsloth--font-data-version name source
                            units-per-em glyphs char-to-glyph
                            horizontal-line-metrics horizontal-kern
                            nil ; horizontal-kern-loaded?
                            vertical-line-metrics font-settings
                            (fontsloth-otf-font-hash data))))
          (if (fontsloth-font-settings-lazy-outline? font-settings)
              (puthash font data fontsloth--lazy-fonts)
            (let ((seen (make-hash-table :test 'eq))
                  (scale (fontsloth-font-settings-scale font-settings)))
              (maphash
               (lambda (_code-point glyph-id)
                 (unless (gethash glyph-id seen)
                   (puthash glyph-id t seen)
                   (aset glyphs glyph-id
                         (fontsloth-glyph-create glyph-id scale
                                                 units-per-em data))))
               char-to-glyph)))
          font)))))

(defun fontsloth--load-font-cached (path font-settings)
  "Retrieve a font from cache and if not load it with PATH and FONT-SETTINGS."
  (cl-flet ((load-font ()
              (let ((font (fontsloth--load-font path font-settings)))
                (fontsloth-cache-put path font)
                font)))
    (if-let ((cached-font (fontsloth-cache-get path)))
        (if (and (eq (aref cached-font 1) fontsloth--font-data-version)
                 (eq (fontsloth-font-settings-lazy-outline?
                      (fontsloth-font-settings cached-font))
                     (fontsloth-font-settings-lazy-outline? font-settings)))
            cached-font
          (message
           "fontsloth: mismatched version or settings, invalidating cache for %s"
           path)
          (fontsloth-cache-invalidate path)
          (load-font))
      (load-font))))

(cl-defun fontsloth-load-font
    (source &key (font-settings (fontsloth-font-settings-create))
            (cache t))
  "Load a font from SOURCE and return a `fontsloth-font' describing it.
Currently only file path sources are accepted.
Loading from font collections is not yet implemented.
SOURCE the font source
FONT-SETTINGS the `fontsloth-font-settings' to use
CACHE t to use cache, \\='bypass to bypass cache, \\='reload to force update
cache"
  (unless (and (stringp source) (f-file-p source))
    (error "Source %s with type %s currently unsupported"
           source (type-of source)))
  (cl-case cache
    (bypass (fontsloth--load-font source font-settings))
    (reload (fontsloth-cache-invalidate source)
            (fontsloth--load-font-cached source font-settings))
    (t (fontsloth--load-font-cached source font-settings))))

(defsubst fontsloth--async-do-reload-fn (sources font-settings force-reload?)
  "Return a lambda to load and cache fonts in an asynchronous process.

SOURCES a sequence of font sources
FONT-SETTINGS the `fontsloth-font-settings' to use
FORCE-RELOAD? non-nil if sources should be re-loaded and cached
regardless of whether they are already cached"
  (lambda ()
    (require 'fontsloth)
    (require 'seq)
    (seq-map (lambda (s)
               (when s
                 (progn
                   (fontsloth-load-font
                    s
                    :font-settings font-settings
                    :cache (when force-reload? 'reload))
                   (pcache-save
                    (gethash (fontsloth-cache--pcache-path-name s)
                             *pcache-repositories*)
                    t)
                   t)))
             sources)))

(cl-defun fontsloth-async-load-and-cache-fonts
    (sources &key (font-settings (fontsloth-font-settings-create))
             finish-func
             force-reload?)
  "Given a sequence of font sources SOURCES, attempt to cache and save
each font in an asynchronous process. Returns the process or nil if no
process is started.

Optional keyword arguments:
FONT-SETTINGS the `fontsloth-font-settings' to use
FINISH-FUNC a function to be called when the process completes,
expecting a sequence containing t for each source loaded.
FORCE-RELOAD? non-nil if sources should be re-loaded and cached
regardless of whether they are already cached"
  (if-let ((sources (seq-remove #'null sources))
           (sources
            (if force-reload?
                sources
              (seq-remove #'fontsloth-cache-get sources))))
      (progn
        (message "fontsloth: loading %s font(s) asynchronously" (length sources))
        (fontsloth:info fontsloth-log
                        "fontsloth: attempting to load and cache:\n%s\n..."
                        (pp-to-string sources))
        (async-start
         (fontsloth--async-do-reload-fn sources font-settings force-reload?)
         (lambda (result)
           (when (seq-remove #'null result)
             (fontsloth-cache--load-repos t))
           (when finish-func
                 (apply finish-func result nil)))))
    (when finish-func
      (apply finish-func nil nil))))

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

(defsubst fontsloth-font-code-points (font)
  "Return ascendingly sorted code points covered by FONT."
  (cl-remove-if (lambda (c) (eq 0 (fontsloth-font-glyph-id font c)))
                (sort (map-keys (fontsloth-font-char-to-glyph font)))))

(defun fontsloth-font-code-point-ranges (font)
  "Return code points covered by FONT as an ascendingly sorted set of ranges.

The result is a sequence of pairs where each pair represents a code
point range starting from the first element up to but not including the
second element."
  (cl-loop for (a b) on (fontsloth-font-code-points font)
           for res = `((,a)) then res do
           (unless (eq (1+ a) b)
             (setcdr (car res) `(,(1+ a)))
             (when b
               (push `(,b) res)))
           finally return (nreverse res)))

(defun fontsloth-font-code-point-diff (a b)
  "Return code point ranges which are covered by font B and not by font A.

See `fontsloth-font-code-point-ranges' to get code point ranges for a font."
  (cl-labels ((recur (a b diff)
                (cond ((null a) (append (nreverse diff) b))
                      ((null b) (nreverse diff))
                      ((>= (caar b) (cadar a))
                       (recur (cdr a) b diff))
                      ((< (cadar b) (caar a))
                       (recur a (cdr b) (cons (car b) diff)))
                      ((and (<= (cadar b) (cadar a))
                            (< (caar b) (caar a)))
                       (recur (cons `(,(cadar b) ,(cadar a)) (cdr a))
                              (cdr b)
                              (cons `(,(caar b) ,(caar a)) diff)))
                      ((< (caar b) (caar a))
                       (recur (cdr a)
                              (cons `(,(cadar a) ,(cadar b)) (cdr b))
                              (cons `(,(caar b) ,(caar a)) diff)))
                      ((and (>= (caar b) (caar a))
                            (> (cadar b) (cadar a)))
                       (recur (cdr a)
                              (cons `(,(cadar a) ,(cadar b)) (cdr b))
                              diff))
                      (t (recur (cons `(,(1+ (cadar b)) ,(cadar a)) (cdr a))
                                (cdr b)
                                diff)))))
    (recur a b nil)))

(defun fontsloth-font-horizontal-kern-by-id (font left right px)
  "Return FONT's horizontal kern value for LEFT/RIGHT scaled to PX or 0."
  (let ((scale (fontsloth-font-scale-factor font px)))
    (* (or (when-let ((mappings (fontsloth-font-kern-map font)))
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
  (when-let ((glyph (fontsloth-font-glyph font glyph-id)))
    (fontsloth-font-metrics-raw (fontsloth-font-scale-factor font px)
                                glyph 0.0)))

(cl-defun fontsloth-font-rasterize (font glyph-id px &optional (subpixel? nil))
  "Rasterize FONT's glyph at GLYPH-ID in PX pixel size.
Produces a greyscale pixmap suitable for PGM.  Returns
`fontsloth-metrics+pixmap'.
SUBPIXEL? optional t to produce a subpixel render suitable for PPM"
  (when-let ((glyph (fontsloth-font-glyph font glyph-id)))
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
