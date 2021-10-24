;;; fontsloth-layout.el --- Elisp otf/ttf layout organizer -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.13.1
;; Package-Requires: ((cl-lib "0.5") (emacs "26.1"))
;; Keywords: true-type, font, bindat, ttf, otf, parsing

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

;; Part of fontsloth: the slowest font renderer in the world written in pure
;; elisp. inspired by fontdue

;; fontsloth-layout (this file): glyph layout helpers

;; To use this module by itself, load and enable it as follows:
;;   (use-package fontsloth-layout)
;;

;;; Code:

(require 'cl-lib)
(cl-float-limits)

(require 'fontsloth)
(require 'fontsloth-layout-linebreak)

;; FIXME: the fontdue author has found a better way to index glyphs
;; this is awkward and it would be good to do likewise
(cl-defstruct (fontsloth-layout-glyph-raster-config
               (:constructor fontsloth-layout-glyph-raster-config-create)
               (:copier nil))
  "This is a key for indexing a rastered glyph."
  (glyph-id 0 :type 'fixed)
  (px 12.0 :type 'number)
  (font-index 0 :type 'fixed))

(cl-defstruct (fontsloth-layout-glyph-position
               (:constructor fontsloth-layout-glyph-position-create)
               (:copier nil))
  "Describe a position in a text layout."
  (key nil :type 'fontsloth-layout-glyph-raster-config)
  (parent nil :type 'character)
  (x nil :type 'number)
  (y nil :type 'number)
  (width nil :type 'fixed)
  (height nil :type 'fixed)
  (char-data nil :type 'fontsloth-layout-char-data)
  (user-data nil))

(cl-defstruct (fontsloth-layout-text-style
               (:constructor fontsloth-layout-text-style-create)
               (:copier nil))
  "Describe text to layout."
  (text nil :type 'string)
  (px 12.0 :type 'number)
  (font-index 0 :type 'fixed)
  (user-data nil))

(cl-defstruct (fontsloth-layout-line-metrics
               (:constructor fontsloth-layout-line-metrics-create)
               (:copier nil))
  "Describe metrics for a layout line."
  (padding 0.0 :type 'number)
  (ascent 0.0 :type 'number)
  (x-start 0.0 :type 'number)
  (new-line-size 0.0 :type 'number)
  (end-index 0 :type 'fixed))

(cl-defstruct (fontsloth-layout-settings
               (:constructor fontsloth-layout-settings-create)
               (:copier nil))
  "Describe the layout settings."
  (x 0.0 :type 'number)
  (y 0.0 :type 'number)
  (max-width nil :type 'fixed)
  (max-height nil :type 'fixed)
  (horizontal-align 'left)
  (vertical-align 'top)
  (wrap-style 'word)
  (wrap-hard-breaks t :type 'boolean))

(cl-defstruct (fontsloth-layout
               (:constructor fontsloth-layout-create)
               (:copier nil))
  "Describe the layout."
  (flip nil :type 'boolean)
  (x 0.0 :type 'number)
  (y 0.0 :type 'number)
  (wrap-mask nil :type 'fontsloth-layout-linebreak-data)
  (max-width 0.0 :type 'number)
  (max-height 0.0 :type 'number)
  (vertical-align 0.0  :type 'number)
  (horizontal-align 0.0 :type 'number)
  (output nil :type 'vector)
  (glyphs nil :type 'vector)
  (line-metrics nil :type 'list)
  (linebreaker (fontsloth-layout-linebreaker-create)
               :type 'fontsloth-layout-linebreaker)
  (linebreak-prev fontsloth-layout-linebreak-none
                  :type 'fontsloth-layout-linebreak-data)
  (linebreak-pos 0.0 :type 'number)
  (linebreak-idx 0 :type 'fixed)
  (current-pos 0.0 :type 'number)
  (current-ascent 0.0 :type 'number)
  (current-new-line 0.0 :type 'number)
  (current-px 0.0 :type 'number)
  (start-pos 0.0 :type 'number)
  (height 0.0 :type 'number))

(defun fontsloth-layout-reset (layout settings)
  "Reset LAYOUT with SETTINGS."
  (setf (fontsloth-layout-x layout)
        (fontsloth-layout-settings-x settings)
        (fontsloth-layout-y layout)
        (fontsloth-layout-settings-y settings)
        (fontsloth-layout-wrap-mask layout)
        (fontsloth-layout-lb-data-from-mask
         (eq (fontsloth-layout-settings-wrap-style settings) 'word)
         (fontsloth-layout-settings-wrap-hard-breaks settings)
         (fontsloth-layout-settings-max-width settings))
        (fontsloth-layout-max-width layout)
        (or (fontsloth-layout-settings-max-width settings)
            cl-most-positive-float)
        (fontsloth-layout-max-height layout)
        (or (fontsloth-layout-settings-max-height settings)
            cl-most-positive-float)
        (fontsloth-layout-vertical-align layout)
        (if (fontsloth-layout-settings-max-height settings)
            (cl-case (fontsloth-layout-settings-vertical-align settings)
              ('top 0.0)
              ('middle 0.5)
              ('bottom 1.0))
          0.0)
        (fontsloth-layout-horizontal-align layout)
        (if (fontsloth-layout-settings-max-width settings)
            (cl-case (fontsloth-layout-settings-horizontal-align settings)
              ('left 0.0)
              ('center 0.5)
              ('right 1.0))
          0.0))
  (fontsloth-layout-clear layout))

(defun fontsloth-layout-clear (layout)
  "Clear LAYOUT."
  (setf (fontsloth-layout-glyphs layout) nil
        (fontsloth-layout-output layout) nil
        (fontsloth-layout-line-metrics layout)
        `(,(fontsloth-layout-line-metrics-create))
        (fontsloth-layout-linebreak-prev layout)
        fontsloth-layout-linebreak-none
        (fontsloth-layout-linebreak-pos layout) 0.0
        (fontsloth-layout-linebreak-idx layout) 0
        (fontsloth-layout-current-pos layout) 0.0
        (fontsloth-layout-current-ascent layout) 0.0
        (fontsloth-layout-current-new-line layout) 0.0
        (fontsloth-layout-current-px layout) 0.0
        (fontsloth-layout-start-pos layout) 0.0
        (fontsloth-layout-height layout) 0.0)
  (fontsloth-layout-linebreaker-reset
   (fontsloth-layout-linebreaker layout)))

(defsubst fontsloth-layout-current-height (layout)
  "Calculate the current height of LAYOUT."
  (if-let ((line (car (fontsloth-layout-line-metrics layout))))
      (+ (fontsloth-layout-height layout)
         (fontsloth-layout-line-metrics-new-line-size line))
    0.0))

(defsubst fontsloth-layout-lines (layout)
  "Return the number of lines in LAYOUT."
  (length (fontsloth-layout-line-metrics layout)))

(defun fontsloth-layout-append (layout fonts style)
  "Append text in STYLE to LAYOUT using FONTS."
  ;; TODO: rewrite this because, well, just look
  (pcase-let* (((cl-struct fontsloth-layout
                           flip max-width glyphs wrap-mask linebreaker) layout)
               ((cl-struct fontsloth-layout-text-style
                           text px font-index user-data)
                style)
               (font (elt fonts font-index))
               (metrics
                (fontsloth-scale-horizontal-line-metrics font px))
               ((cl-struct fontsloth-line-metrics ascent new-line-size) metrics)
               (current-ascent (ceiling ascent))
               (current-new-line (ceiling new-line-size)))
    (when-let ((line (car (fontsloth-layout-line-metrics layout))))
      (when (< (fontsloth-layout-line-metrics-ascent line)
               current-ascent)
        (setf (fontsloth-layout-line-metrics-ascent line) current-ascent))
      (when (< (fontsloth-layout-line-metrics-new-line-size line)
               current-new-line)
        (setf (fontsloth-layout-line-metrics-new-line-size line)
              current-new-line)))
    (cl-loop for character across text do
             (pcase-let*
                 ((linebreak
                   (fontsloth-layout-lb-data-mask
                    (fontsloth-layout-linebreaker-next linebreaker character)
                    wrap-mask))
                  (glyph-id (fontsloth-font-glyph-id font character))
                  (char-data
                   (fontsloth-layout-char-data-classify character glyph-id))
                  ((cl-struct fontsloth-metrics
                              width height advance-width bounds)
                   (if (fontsloth-layout-char-data-control-p
                        char-data)
                       (fontsloth-metrics-create)
                     (fontsloth-font-metrics font glyph-id px))))
               (when (<= (fontsloth-layout-linebreak-data-bits
                          (fontsloth-layout-linebreak-prev layout))
                         (fontsloth-layout-linebreak-data-bits linebreak))
                 (setf (fontsloth-layout-linebreak-prev layout) linebreak
                       (fontsloth-layout-linebreak-pos layout)
                       (fontsloth-layout-current-pos layout)
                       (fontsloth-layout-linebreak-idx layout) (length glyphs)))
               (let ((advance (ceiling advance-width)))
                 (when (or (fontsloth-layout-lb-data-hard-p linebreak)
                           (< max-width
                              (+ advance
                                 (- (fontsloth-layout-current-pos layout)
                                    (fontsloth-layout-start-pos layout)))))
                   (setf (fontsloth-layout-linebreak-prev layout)
                         fontsloth-layout-linebreak-none)
                   (when-let ((line
                               (car (fontsloth-layout-line-metrics layout))))
                     (setf (fontsloth-layout-line-metrics-end-index line)
                           (fontsloth-layout-linebreak-idx layout)
                           (fontsloth-layout-line-metrics-padding line)
                           (- max-width
                              (- (fontsloth-layout-linebreak-pos layout)
                                 (fontsloth-layout-start-pos layout)))
                           (fontsloth-layout-height layout)
                           (+ (fontsloth-layout-height layout)
                              (fontsloth-layout-line-metrics-new-line-size line))))
                   (push (fontsloth-layout-line-metrics-create
                          :padding 0.0
                          :ascent (fontsloth-layout-current-ascent layout)
                          :x-start (fontsloth-layout-linebreak-pos layout)
                          :new-line-size (fontsloth-layout-current-new-line
                                          layout)
                          :end-index 0)
                         (fontsloth-layout-line-metrics layout))
                   (setf (fontsloth-layout-start-pos layout)
                         (fontsloth-layout-linebreak-pos layout)))
                 (pcase-let* (((cl-struct fontsloth-glyph-outline-bounds
                                          xmin ymin (height bounds.height))
                               bounds)
                              (y (if flip
                                     (floor (- (* -1 bounds.height) ymin))
                                   (floor ymin))))
                   (push
                    (fontsloth-layout-glyph-position-create
                     :key (fontsloth-layout-glyph-raster-config-create
                           :glyph-id glyph-id
                           :px px
                           :font-index font-index)
                     :parent character
                     :x (floor (+ (fontsloth-layout-current-pos layout)
                                  xmin))
                     :y y
                     :width width
                     :height height
                     :char-data char-data
                     :user-data user-data)
                    (fontsloth-layout-glyphs layout))
                   (setf (fontsloth-layout-current-pos layout)
                         (+ advance (fontsloth-layout-current-pos layout)))))))
    (when-let ((line (car (fontsloth-layout-line-metrics layout))))
      (setf (fontsloth-layout-line-metrics-padding line)
            (- max-width (- (fontsloth-layout-current-pos layout)
                            (fontsloth-layout-start-pos layout)))
            (fontsloth-layout-line-metrics-end-index line)
            (length (fontsloth-layout-glyphs layout))))
    (setf (fontsloth-layout-line-metrics layout)
          (nreverse (fontsloth-layout-line-metrics layout))
          (fontsloth-layout-glyphs layout)
          (nreverse (fontsloth-layout-glyphs layout)))))

(defun fontsloth-layout-finalize (layout)
  "Finalize LAYOUT and return a list of `fontsloth-layout-glyph-position'."
  (if (= (length (fontsloth-layout-glyphs layout))
         (length (fontsloth-layout-output layout)))
      (fontsloth-layout-output layout)
    (pcase-let* (((cl-struct fontsloth-layout
                             flip x y max-height height line-metrics
                             glyphs horizontal-align vertical-align) layout)
                 (dir (if flip -1.0 1.0))
                 (y (- y (* dir (floor (- max-height height)) vertical-align)))
                 (idx 0))
      (pcase-dolist ((cl-struct fontsloth-layout-line-metrics
                                x-start padding ascent end-index
                                new-line-size) line-metrics)
        (setf y (- y (* dir ascent)))
        (let ((x (+ (- x x-start) (floor (* padding horizontal-align)))))
          (cl-loop for idx from idx below end-index do
                   (let ((glyph (elt glyphs idx)))
                     (setf (fontsloth-layout-glyph-position-x glyph)
                           (+ x (fontsloth-layout-glyph-position-x glyph))
                           (fontsloth-layout-glyph-position-y glyph)
                           (+ y (fontsloth-layout-glyph-position-y glyph)))
                     (push glyph
                           (fontsloth-layout-output layout))))
          (setf y (- y (* dir (- new-line-size ascent))))
          (setf (fontsloth-layout-current-pos layout)
                (+ x (fontsloth-layout-current-pos layout)))))
      (setf (fontsloth-layout-output layout)
            (nreverse (fontsloth-layout-output layout)))
      (fontsloth-layout-output layout))))

(provide 'fontsloth-layout)
;;; fontsloth-layout.el ends here
