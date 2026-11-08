;;; fontsloth-otf-hvar.el --- HVAR parsing and advance deltas -*- lexical-binding: t -*-

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

;;; Commentary:

;; Parses HVAR and applies advance-width deltas from its ItemVariationStore.
;; LSB and RSB mappings are parsed but not applied yet.

;;; Code:

(require 'bindat)
(require 'cl-lib)

(require 'fontsloth-otf-var)

(defun fontsloth-otf-hvar--u16 (data offset)
  "Read a big-endian uint16 from DATA at OFFSET."
  (+ (ash (aref data offset) 8)
     (aref data (1+ offset))))

(defun fontsloth-otf-hvar--u32 (data offset)
  "Read a big-endian uint32 from DATA at OFFSET."
  (+ (ash (fontsloth-otf-hvar--u16 data offset) 16)
     (fontsloth-otf-hvar--u16 data (+ offset 2))))

(defun fontsloth-otf-hvar--read-uint (data offset size)
  "Read an unsigned integer of SIZE bytes from DATA at OFFSET."
  (cl-loop for i below size
           sum (ash (aref data (+ offset i)) (* 8 (- size 1 i)))))

(defun fontsloth-otf-hvar--parse-delta-set-index-map (data offset)
  "Parse a DeltaSetIndexMap from DATA at OFFSET.
Return a vector of (outer . inner) pairs."
  (when (and offset (< offset (length data)))
    (let ((format (aref data offset))
          (entry-format (aref data (1+ offset))))
      (let* ((count (pcase format
                      (0 (fontsloth-otf-hvar--u16 data (+ offset 2)))
                      (1 (fontsloth-otf-hvar--u32 data (+ offset 2)))
                      (_ (error "Unknown DeltaSetIndexMap format %s" format))))
             (map-start (+ offset (if (= format 0) 4 6)))
             (entry-size (+ 1 (ash (logand entry-format #x30) -4)))
             (inner-bits (1+ (logand entry-format #x0f)))
             (result (make-vector count nil)))
        (dotimes (i count)
          (let* ((entry (fontsloth-otf-hvar--read-uint
                         data (+ map-start (* i entry-size)) entry-size))
                 (outer (ash entry (- inner-bits)))
                 (inner (logand entry (1- (ash 1 inner-bits)))))
            (aset result i (cons outer inner))))
        result))))

(defun fontsloth-otf-hvar--parse (data)
  "Parse an HVAR table from DATA."
  (let* ((major (fontsloth-otf-hvar--u16 data 0))
         (minor (fontsloth-otf-hvar--u16 data 2))
         (store-offset (fontsloth-otf-hvar--u32 data 4))
         (advance-map-offset (fontsloth-otf-hvar--u32 data 8))
         (lsb-map-offset (if (>= minor 1) (fontsloth-otf-hvar--u32 data 12) 0))
         (rsb-map-offset (if (>= minor 1) (fontsloth-otf-hvar--u32 data 16) 0))
         (variation-store
          (when (not (= store-offset 0))
            (bindat-unpack fontsloth-otf-var--item-variation-store-spec
                           data store-offset))))
    `((major . ,major)
      (minor . ,minor)
      (variation-store . ,variation-store)
      (advance-map . ,(when (not (= advance-map-offset 0))
                        (fontsloth-otf-hvar--parse-delta-set-index-map
                         data advance-map-offset)))
      (lsb-map . ,(when (not (= lsb-map-offset 0))
                    (fontsloth-otf-hvar--parse-delta-set-index-map
                     data lsb-map-offset)))
      (rsb-map . ,(when (not (= rsb-map-offset 0))
                    (fontsloth-otf-hvar--parse-delta-set-index-map
                     data rsb-map-offset))))))

(defun fontsloth-otf-hvar-advance-delta (hvar glyph-id coords)
  "Return HVAR's advance-width delta for GLYPH-ID at COORDS."
  (let* ((store (alist-get 'variation-store hvar))
         (map (alist-get 'advance-map hvar))
         (entry (if map
                    (when (< glyph-id (length map)) (aref map glyph-id))
                  (cons 0 glyph-id))))
    (when (and store entry)
      (fontsloth-otf-var-item-delta store (car entry) (cdr entry) coords))))

(provide 'fontsloth-otf-hvar)
;;; fontsloth-otf-hvar.el ends here
