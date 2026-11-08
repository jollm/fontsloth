;;; fontsloth-otf-cff2.el --- CFF2 table parsing -*- lexical-binding: t -*-

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

;; Parses the CFF2 table: header, top dict, Global Subr INDEX, CharStrings,
;; FDArray, FDSelect, and the embedded ItemVariationStore.  Outlining CFF2
;; charstrings (including blend/vsindex) is a separate step.

;;; Code:

(require 'bindat)
(require 'cl-lib)

(require 'fontsloth-otf-cff)
(require 'fontsloth-otf-var)

(defun fontsloth-otf-cff2--u16 (data offset)
  "Read a big-endian uint16 from DATA at OFFSET."
  (+ (ash (aref data offset) 8)
     (aref data (1+ offset))))

(defun fontsloth-otf-cff2--u32 (data offset)
  "Read a big-endian uint32 from DATA at OFFSET."
  (+ (ash (fontsloth-otf-cff2--u16 data offset) 16)
     (fontsloth-otf-cff2--u16 data (+ offset 2))))

(defun fontsloth-otf-cff2--read-uint (data offset size)
  "Read an unsigned integer of SIZE bytes from DATA at OFFSET."
  (cl-loop for i below size
           sum (ash (aref data (+ offset i)) (* 8 (- size 1 i)))))

(defun fontsloth-otf-cff2--parse-index (data offset)
  "Parse a CFF2 INDEX at OFFSET.
Return a cons of the item count and a vector of byte strings, or nil."
  (when (and offset (< offset (length data)))
    (let ((count (fontsloth-otf-cff2--u32 data offset)))
      (if (= count 0)
          (cons 0 [])
        (let* ((offset-size (aref data (+ offset 4)))
               (offset-array-start (+ offset 5))
               (data-start (+ offset-array-start (* (1+ count)
                                                    offset-size)))
               (offsets (make-vector (1+ count) 0))
               (items (make-vector count nil)))
          (dotimes (i (1+ count))
            (aset offsets i
                  (fontsloth-otf-cff2--read-uint
                   data (+ offset-array-start (* i offset-size))
                   offset-size)))
          (dotimes (i count)
            (aset items i
                  (substring data
                             (+ data-start (1- (aref offsets i)))
                             (+ data-start (1- (aref offsets (1+ i)))))))
          (cons count items))))))

(defun fontsloth-otf-cff2--parse-font-dict (data)
  "Parse a CFF2 Font DICT from DATA."
  (let* ((max-operands 513)
         (operands-buffer (make-vector max-operands 0))
         (parser (fontsloth-otf-cff-dict-parser-create
                  :data data :operands operands-buffer)))
    (cl-loop for op = (fontsloth-otf-cff-dict-parser-next parser)
             while op collect
             (pcase op
               ((pred (eq fontsloth-otf-cff-top-dict-private-dict-size-and-offset))
                `(private-dict-range
                  . ,(fontsloth-otf-cff-dict-parse-range parser)))
               (_ nil)))))

(defun fontsloth-otf-cff2--parse-private-dict (data start end)
  "Parse the private dict in DATA between START and END."
  (when (and start end (<= end (length data)))
    (fontsloth-otf-cff1--parse-private-dict (substring data start end))))

(defun fontsloth-otf-cff2--parse-fd-array (data offset)
  "Parse the FDArray INDEX at OFFSET.
Return a vector of font-dict alists."
  (let* ((index (fontsloth-otf-cff2--parse-index data offset))
         (dicts (cdr index))
         (result (make-vector (length dicts) nil)))
    (dotimes (i (length dicts))
      (let* ((dict-data (aref dicts i))
             (font-dict (fontsloth-otf-cff2--parse-font-dict dict-data))
             (range (alist-get 'private-dict-range font-dict))
             (private-dict (when range
                             (fontsloth-otf-cff2--parse-private-dict
                              data (car range) (cadr range))))
             (local-subrs
              (when (and private-dict range)
                (when-let ((subrs-offset
                            (alist-get 'local-subrs-offset private-dict)))
                  (cdr (fontsloth-otf-cff2--parse-index
                        data (+ (car range) subrs-offset)))))))
        (aset result i
              `((dict . ,font-dict)
                (private-dict . ,private-dict)
                (local-subrs . ,local-subrs)))))
    result))

(defun fontsloth-otf-cff2--parse-fd-select (data offset num-glyphs)
  "Parse an FDSelect table at OFFSET for NUM-GLYPHS glyphs.
Return a vector mapping glyph id to FD index."
  (when (and offset (< offset (length data)))
    (let ((result (make-vector num-glyphs 0))
          (format (aref data offset)))
      (pcase format
        (0
         (let* ((count (fontsloth-otf-cff2--u16 data (1+ offset)))
                (fds (make-vector count 0)))
           (dotimes (i (min count num-glyphs))
             (aset fds i (aref data (+ offset 3 i)))
             (aset result i (aref fds i)))))
        (3
         (let* ((n-ranges (fontsloth-otf-cff2--u16 data (1+ offset)))
                (ranges-start (+ offset 3)))
           (dotimes (i n-ranges)
             (let* ((pos (+ ranges-start (* i 3)))
                    (first (fontsloth-otf-cff2--u16 data pos))
                    (fd (aref data (+ pos 2)))
                    (next (if (< (1+ i) n-ranges)
                              (fontsloth-otf-cff2--u16
                               data (+ pos 3))
                            (fontsloth-otf-cff2--u16
                             data (+ ranges-start (* n-ranges 3))))))
               (cl-loop for gid from first below (min next num-glyphs)
                        do (aset result gid fd))))))
        (4
         (let* ((n-ranges (fontsloth-otf-cff2--u32 data (1+ offset)))
                (ranges-start (+ offset 5)))
           (dotimes (i n-ranges)
             (let* ((pos (+ ranges-start (* i 6)))
                    (first (fontsloth-otf-cff2--u32 data pos))
                    (fd (fontsloth-otf-cff2--u16 data (+ pos 4)))
                    (next (if (< (1+ i) n-ranges)
                              (fontsloth-otf-cff2--u32
                               data (+ pos 6))
                            (fontsloth-otf-cff2--u32
                             data (+ ranges-start (* n-ranges 6))))))
               (cl-loop for gid from first below (min next num-glyphs)
                        do (aset result gid fd))))))
        (_ (error "Unknown CFF2 FDSelect format %s" format)))
      result)))

(defun fontsloth-otf-cff2--parse (data)
  "Parse a complete CFF2 table from DATA."
  (unless (= 2 (aref data 0))
    (error "Not a CFF2 table"))
  (let* ((header-size (aref data 2))
         (top-dict-length (fontsloth-otf-cff2--u16 data 3))
         (top-dict-data (substring data header-size
                                   (+ header-size top-dict-length)))
         (top-dict (fontsloth-otf-cff2--parse-top-dict top-dict-data))
         (global-subrs
          (cdr (fontsloth-otf-cff2--parse-index
                data (+ header-size top-dict-length))))
         (char-strings-offset (alist-get 'char-strings-offset top-dict))
         (char-strings-index (when char-strings-offset
                               (fontsloth-otf-cff2--parse-index
                                data char-strings-offset)))
         (num-glyphs (car char-strings-index))
         (char-strings (cdr char-strings-index))
         (fd-array (when-let ((offset (alist-get 'font-dict-index-offset
                                                  top-dict)))
                     (fontsloth-otf-cff2--parse-fd-array data offset)))
         (fd-select
          (or (when-let ((offset (alist-get 'fd-select-offset top-dict)))
                (fontsloth-otf-cff2--parse-fd-select data offset num-glyphs))
              (and num-glyphs (make-vector num-glyphs 0))))
         (variation-store
          (when-let ((offset (alist-get 'font-dict-variation-store-offset
                                        top-dict)))
            ;; CFF2 prefixes the ItemVariationStore with a uint16 length.
            (when (<= (+ offset 2) (length data))
              (bindat-unpack
               fontsloth-otf-var--item-variation-store-spec
               data (+ offset 2))))))
    `((major . ,(aref data 0))
      (minor . ,(aref data 1))
      (header-size . ,header-size)
      (top-dict . ,top-dict)
      (global-subrs . ,global-subrs)
      (char-strings . ,char-strings)
      (num-glyphs . ,num-glyphs)
      (font-dicts . ,fd-array)
      (fd-select . ,fd-select)
      (variation-store . ,variation-store))))

(defun fontsloth-otf-cff2-char-string (cff2 glyph-id)
  "Return GLYPH-ID's CFF2 charstring from CFF2."
  (aref (alist-get 'char-strings cff2) glyph-id))

(defun fontsloth-otf-cff2-num-glyphs (cff2)
  "Return the number of glyphs in CFF2."
  (alist-get 'num-glyphs cff2))

(defun fontsloth-otf-cff2-fd-select (cff2)
  "Return CFF2's glyph-to-FD mapping."
  (alist-get 'fd-select cff2))

(defun fontsloth-otf-cff2-font-dict (cff2 glyph-id)
  "Return the font dict alist for GLYPH-ID.
The result includes `private-dict' and `local-subrs'."
  (let ((fd (or (let ((select (fontsloth-otf-cff2-fd-select cff2)))
                  (when select (aref select glyph-id)))
                0)))
    (aref (alist-get 'font-dicts cff2) fd)))

(defun fontsloth-otf-cff2-outline (cff2 glyph-id outliner &optional coords)
  "Outline GLYPH-ID from CFF2 into OUTLINER.
COORDS is an optional normalized variation-coordinate vector."
  (fontsloth:verbose* fontsloth-log "Cff2: outlining glyph %s" glyph-id)
  (when-let* ((char-strings (alist-get 'char-strings cff2))
              (char-string (aref char-strings glyph-id)))
    (let* ((font-dict (fontsloth-otf-cff2-font-dict cff2 glyph-id))
           (local-subrs (alist-get 'local-subrs font-dict))
           (global-subrs (alist-get 'global-subrs cff2))
           (metadata
            `((char-strings . ((data . ,char-strings)))
              (global-subrs . ((index . ((count . ,(length global-subrs))))
                               (data . ,global-subrs)))
              (local-subrs . ((data . ,local-subrs)))
              (variation-store
               . ,(alist-get 'variation-store cff2)))))
      (fontsloth-otf-cff-parse-char-string
       char-string metadata glyph-id outliner :cff2 t :coords coords))))

(provide 'fontsloth-otf-cff2)

;;; fontsloth-otf-cff2.el ends here
