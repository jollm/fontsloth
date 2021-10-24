;;; fontsloth-layout-linebreak.el --- Unicode linebreak state machine -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.14.0
;; Package-Requires: ((emacs "26.1"))
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

;; fontsloth-layout-linebreak (this file): linebreak state machine

;;; Code:

(require 'cl-lib)

(require 'fontsloth-layout-lb-tables)

(defconst fontsloth-layout--linebreak-none #b00000000)
(defconst fontsloth-layout--linebreak-soft #b00000001)
(defconst fontsloth-layout--linebreak-hard #b00000010)

(cl-defstruct
    (fontsloth-layout-linebreak-data
     (:constructor fontsloth-layout-linebreak-data-create)
     (:copier nil))
  "Container for linebreak data."
  (bits 0 :type 'fixed))

(defconst fontsloth-layout-linebreak-none
  (fontsloth-layout-linebreak-data-create
   :bits fontsloth-layout--linebreak-none))
(defconst fontsloth-layout-linebreak-soft
  (fontsloth-layout-linebreak-data-create
   :bits fontsloth-layout--linebreak-soft))
(defconst fontsloth-layout-linebreak-hard
  (fontsloth-layout-linebreak-data-create
   :bits fontsloth-layout--linebreak-hard))

(defun fontsloth-layout-lb-data-from-mask (wrap-soft? wrap-hard? has-width?)
  "Create a linebreak-data given mask set criteria.
WRAP-SOFT? t if soft wrapping
WRAP-HARD? t if hard wrapping
HAS-WIDTH? t if has width"
  (fontsloth-layout-linebreak-data-create
   :bits
   (let ((hard (if wrap-hard?
                   fontsloth-layout--linebreak-hard
                 0)))
     (if (and wrap-soft? has-width?)
         (logior hard fontsloth-layout--linebreak-soft)
       hard))))

(defsubst fontsloth-layout-lb-data-hard-p (lb-data)
  "Test whether LB-DATA indicates a hard break."
  (= (fontsloth-layout-linebreak-data-bits lb-data)
     fontsloth-layout--linebreak-hard))

(defsubst fontsloth-layout-lb-data-soft-p (lb-data)
  "Test whether LB-DATA indicates a soft break."
  (= (fontsloth-layout-linebreak-data-bits lb-data)
     fontsloth-layout--linebreak-soft))

(defsubst fontsloth-layout-lb-data-mask (lb-data1 lb-data2)
  "Mask LB-DATA1 with LB-DATA2."
  (fontsloth-layout-linebreak-data-create
   :bits (logand (fontsloth-layout-linebreak-data-bits lb-data1)
                 (fontsloth-layout-linebreak-data-bits lb-data2))))

(cl-defstruct
    (fontsloth-layout-linebreaker
     (:constructor fontsloth-layout-linebreaker-create)
     (:copier nil))
  "Container for linebreaker state."
  (state 0 :type 'fixed))

(defsubst fontsloth-layout-linebreaker-reset (lbreaker)
  "Reset the linebreaker LBREAKER."
  (setf (fontsloth-layout-linebreaker-state lbreaker) 0))

(defun fontsloth--u8->i8 (u8)
  "Interpret an unsigned byte U8 as a signed byte."
  (let* ((max (ash 1 7))
         (wrap (+ max max)))
    (if (>= u8 max) (- u8 wrap) u8)))

(defun fontsloth-layout-linebreaker-next (lbreaker code-point)
  "Step the linebreaker LBREAKER state machine forward with CODE-POINT."
  (let* ((lb (cond
              ((> #x800 code-point)
               (aref fontsloth-layout--linebreak-1-2 code-point))
              ((> #x10000 code-point)
               (let ((child (aref fontsloth-layout--linebreak-3-root
                                  (ash code-point -6))))
                 (aref fontsloth-layout--linebreak-3-child
                       (+ (* child #x40) (logand code-point #x3f)))))
              (t (let* ((mid (aref fontsloth-layout--linebreak-4-root
                                   (ash code-point -12)))
                        (leaf (aref fontsloth-layout--linebreak-4-mid
                                    (+ (* mid #x40)
                                       (logand (ash code-point -6) #x3f)))))
                   (aref fontsloth-layout--linebreak-4-leaves leaf)))))
         (i (+ lb (* (fontsloth-layout-linebreaker-state lbreaker)
                     fontsloth-layout--n-linebreak-categories)))
         (new (aref fontsloth-layout--linebreak-state-machine i)))
    (if (> 0 (fontsloth--u8->i8 new))
        (progn (setf (fontsloth-layout-linebreaker-state lbreaker)
                     (logand new #x3f))
               (if (<= #xc0 new)
                   fontsloth-layout-linebreak-hard
                 fontsloth-layout-linebreak-soft))
      (progn (setf (fontsloth-layout-linebreaker-state lbreaker) new)
             fontsloth-layout-linebreak-none))))

(cl-defstruct
    (fontsloth-layout-char-data
     (:constructor fontsloth-layout-char-data-create)
     (:copier nil))
  "Container for character metadata."
  (bits 0 :type 'fixed))

(defconst fontsloth-layout-char-data-whitespace #b00000001)
(defconst fontsloth-layout-char-data-control #b00000010)
(defconst fontsloth-layout-char-data-missing #b00000100)

(defun fontsloth-layout-char-data-classify (char index)
  "Create a char-data for CHAR at INDEX."
  (fontsloth-layout-char-data-create
   :bits
   (let* ((class (if (= 0 index) fontsloth-layout-char-data-missing 0))
          (whitespace
           (cl-case char
             ((?\t ?\n ?\x0c ?\r ?\ )
              (logior class fontsloth-layout-char-data-whitespace))
             (t class))))
     (or (cl-loop for c in (cons #x7f (cl-loop for c from 0 upto #x1f
                                               collect c))
                  thereis
                  (when (= c char)
                    (logior whitespace fontsloth-layout-char-data-control)))
         whitespace))))

(defsubst fontsloth-layout-char-data-whitespace-p (char-data)
  "Test whether CHAR-DATA indicates whitespace."
  (not (= 0 (logand fontsloth-layout-char-data-whitespace
                    (fontsloth-layout-char-data-bits char-data)))))

(defsubst fontsloth-layout-char-data-control-p (char-data)
  "Test whether CHAR-DATA indicates control character."
  (not (= 0 (logand fontsloth-layout-char-data-control
                    (fontsloth-layout-char-data-bits char-data)))))

(defsubst fontsloth-layout-char-data-missing-p (char-data)
  "Test whether CHAR-DATA indicates missing char."
  (not (= 0 (logand fontsloth-layout-char-data-missing
                    (fontsloth-layout-char-data-bits char-data)))))

(provide 'fontsloth-layout-linebreak)
;;; fontsloth-layout-linebreak.el ends here
