;;; fontsloth-otf-cff-dict.el --- Parse CFF dictionary data -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, glyph, glyf, ttf, otf

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

;; Part of fontsloth

;; fontsloth-otf-cff-dict (this file): Parse CFF dictionary data

;;; Code:

(require 'cl-lib)

(require 'fontsloth-log)

(cl-defstruct
    (fontsloth-otf-cff-dict-parser
     (:constructor fontsloth-otf-cff-dict-parser-create))
  (data nil :type 'vector)
  (offset 0 :type 'natnum)
  (operands-offset 0 :type 'natnum)
  (operands nil :type 'vector)
  (operands-len 0 :type 'natnum))

(defconst fontsloth-otf-cff-dict-parser-two-byte-op-mark 12)
(defconst fontsloth-otf-cff-dict-parser-end-of-float #xf)

(defun fontsloth-otf-cff-dict-parser-one-byte-op-p (b)
  "Return t if B represents a CFF dict one byte operator."
  (cond ((and (<= 0 b) (>= 27 b)) t)
        ((and (<= 28 b) (>= 30 b)) nil)
        ((= 31 b) t)
        ((= 255 b) t)))

(defun fontsloth-otf-cff-dict-parse-skip (b data i)
  "Skip CFF dict operator B in DATA starting at I."
  (cl-case b
    (28 2)
    (29 4)
    (30 (progn (fontsloth:debug* fontsloth-log "Found float op at %s" i)
               (cl-loop for ni from i
                        for next = (aref data (1+ ni))
                        until (>= i (1- (length data)))
                        when (let ((nibble1 (ash next -4))
                                   (nibble2 (logand next #xf)))
                               (or (= fontsloth-otf-cff-dict-parser-end-of-float
                                      nibble1)
                                   (= fontsloth-otf-cff-dict-parser-end-of-float
                                      nibble2)))
                        return (- ni i))))
    ((247 248 249 250 251 253 254) 1)
    (t 0)))

(defun fontsloth-otf-cff-dict-parser-next (parser)
  "Parse the next operator in a CFF dict with PARSER."
  (pcase-let (((cl-struct fontsloth-otf-cff-dict-parser offset data) parser))
    (setf (fontsloth-otf-cff-dict-parser-operands-offset parser) offset)
    (cl-loop for i = offset then (+ i i-inc)
             until (>= i (length data))
             for b = (aref data i)
             with i-inc = 1
             if (fontsloth-otf-cff-dict-parser-one-byte-op-p b)
             return
             (prog2 (setf (fontsloth-otf-cff-dict-parser-offset parser) (1+ i))
                 (if (= b fontsloth-otf-cff-dict-parser-two-byte-op-mark)
                     (progn (cl-incf
                             (fontsloth-otf-cff-dict-parser-offset parser))
                            (+ 1200 (aref data (1+ i))))
                   b)
               (fontsloth:debug* fontsloth-log "Found op %s, new offset %s"
                                 b (fontsloth-otf-cff-dict-parser-offset parser)))
             else do
             (progn (fontsloth:debug* fontsloth-log "Skipping op %s" b)
                    (setq i-inc (1+ (fontsloth-otf-cff-dict-parse-skip b data i)))))))

(defun fontsloth-otf-cff-dict-parse-operands (parser)
  "Parse the current dict operands with PARSER."
  (setf (fontsloth-otf-cff-dict-parser-operands-len parser) 0)
  (pcase-let (((cl-struct fontsloth-otf-cff-dict-parser operands-offset data)
               parser))
    (cl-loop for i = operands-offset then (+ i i-inc)
             for b = (aref data i)
             with i-inc = 1
             until (or (>= i (length data))
                       (fontsloth-otf-cff-dict-parser-one-byte-op-p b)
                       (<= (length
                            (fontsloth-otf-cff-dict-parser-operands parser))
                           (fontsloth-otf-cff-dict-parser-operands-len parser)))
             do
             (pcase-let ((`(,op ,size)
                          (fontsloth-otf-cff-dict-parse-number b data i)))
               (fontsloth:debug* fontsloth-log "Found %s operand %s at %s, size %s" b op i size)
               (aset (fontsloth-otf-cff-dict-parser-operands parser)
                     (fontsloth-otf-cff-dict-parser-operands-len parser)
                     op)
               (setq i-inc (1+ (or size 0)))
               (cl-incf (fontsloth-otf-cff-dict-parser-operands-len parser))))))

(defun fontsloth-otf-cff-dict-parse-offset (parser)
  "Parse a CFF dict offset with PARSER."
  (fontsloth-otf-cff-dict-parse-operands parser)
  (when (= 1 (fontsloth-otf-cff-dict-parser-operands-len parser))
    (elt (fontsloth-otf-cff-dict-parser-operands parser) 0)))

(defun fontsloth-otf-cff-dict-parse-range (parser)
  "Parse a CFF dict range with PARSER."
  (fontsloth-otf-cff-dict-parse-operands parser)
  (when (= 2 (fontsloth-otf-cff-dict-parser-operands-len parser))
    (let* ((len (elt (fontsloth-otf-cff-dict-parser-operands parser) 0))
           (start (elt (fontsloth-otf-cff-dict-parser-operands parser) 1))
           (end (+ start len)))
      (when (<= 0 start end)
        `(,start ,end)))))

(defun fontsloth-otf-cff-dict--read-num (nbytes data i)
  "Read as an unsigned integer NBYTES from DATA starting at I."
  (cl-loop for idx from 0 below nbytes
           for ash = (ash (- (1- nbytes) idx) 3)
           for val = (ash (elt data (+ i idx)) ash)
           sum val))

(defun fontsloth-otf-cff-dict--as-sint (bitlen n)
  "Interpret N as a signed integer with BITLEN."
  (let* ((max (ash 1 (1- bitlen)))
         (wrap (+ max max)))
    (if (>= n max)
        (- n wrap)
      n)))

(defun fontsloth-otf-cff-dict--read-sint (bitlen data i)
  "Read a signed integer of BITLEN bits from DATA starting at I."
  (let ((bytelen (ash bitlen -3)))
    (fontsloth-otf-cff-dict--as-sint
     bitlen
     (fontsloth-otf-cff-dict--read-num bytelen data i))))

(defun fontsloth-otf-cff-dict-parse-number (b data i)
  "Parse a CFF dict number from B in DATA starting at I."
  (cond ((= 28 b) `(,(fontsloth-otf-cff-dict--read-sint 16 data (1+ i)) 2))
        ((= 29 b) `(,(fontsloth-otf-cff-dict--read-sint 32 data (1+ i)) 4))
        ((= 30 b)
         (or (cl-loop for ni from i
                      for next = (aref data (1+ ni))
                      until (>= i (1- (length data)))
                      when (let ((nibble1 (ash next -4))
                                 (nibble2 (logand next #xf)))
                             (or (= fontsloth-otf-cff-dict-parser-end-of-float
                                    nibble1)
                                 (= fontsloth-otf-cff-dict-parser-end-of-float
                                    nibble2)))
                      return `(0 ,(1+ (- ni i))))
             '(0 0)))
        ((and (<= 32 b) (>= 246 b))
         `(,(- b 139) 0))
        ((and (<= 247 b) (>= 250 b))
         `(,(let ((b1 (elt data (1+ i))))
              (+ (* 256 (- b 247))
                 b1 108)) 1))
        ((and (<= 251 b) (>= 254 b))
         `(,(let ((b1 (elt data (1+ i))))
              (- (* -256 (- b 251))
                 b1 108)) 1))))

(provide 'fontsloth-otf-cff-dict)
;;; fontsloth-otf-cff-dict.el ends here
