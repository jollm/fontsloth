;;; fontsloth-otf-gvar.el --- GVAR deltas for simple glyphs -*- lexical-binding: t -*-

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

;; GVAR parsing and deltas for simple glyf outlines.  Composite deltas and
;; phantom points are not applied yet.

;;; Code:

(require 'cl-lib)

(defconst fontsloth-otf-gvar--embedded-peak #x8000)
(defconst fontsloth-otf-gvar--intermediate-region #x4000)
(defconst fontsloth-otf-gvar--private-points #x2000)
(defconst fontsloth-otf-gvar--tuple-index-mask #x0fff)
(defconst fontsloth-otf-gvar--tuples-share-points #x8000)
(defconst fontsloth-otf-gvar--points-are-words #x80)
(defconst fontsloth-otf-gvar--point-run-mask #x7f)
(defconst fontsloth-otf-gvar--delta-run-mask #x3f)
(defconst fontsloth-otf-gvar--delta-size-mask #xc0)
(defconst fontsloth-otf-gvar--delta-zero #x80)
(defconst fontsloth-otf-gvar--delta-words #x40)
(defconst fontsloth-otf-gvar--delta-longs #xc0)

(defun fontsloth-otf-gvar--u16 (data offset)
  "Read a big-endian uint16 from DATA at OFFSET."
  (+ (ash (aref data offset) 8) (aref data (1+ offset))))

(defun fontsloth-otf-gvar--s16 (data offset)
  "Read a big-endian sint16 from DATA at OFFSET."
  (let ((n (fontsloth-otf-gvar--u16 data offset)))
    (if (>= n #x8000) (- n #x10000) n)))

(defun fontsloth-otf-gvar--u32 (data offset)
  "Read a big-endian uint32 from DATA at OFFSET."
  (+ (ash (fontsloth-otf-gvar--u16 data offset) 16)
     (fontsloth-otf-gvar--u16 data (+ offset 2))))

(defun fontsloth-otf-gvar--read-uint (data offset size)
  "Read SIZE bytes of unsigned integer from DATA at OFFSET."
  (cl-loop for i below size
           sum (ash (aref data (+ offset i)) (* 8 (- size 1 i)))))

(defun fontsloth-otf-gvar--f2dot14 (n)
  "Convert fixed N with 14 fractional bits to a float."
  (let ((signed (if (>= n #x8000) (- n #x10000) n)))
    (/ signed 16384.0)))

(defun fontsloth-otf-gvar--parse (data)
  "Parse a GVAR table from DATA."
  (let* ((major (fontsloth-otf-gvar--u16 data 0))
         (minor (fontsloth-otf-gvar--u16 data 2))
         (axis-count (fontsloth-otf-gvar--u16 data 4))
         (shared-count (fontsloth-otf-gvar--u16 data 6))
         (shared-offset (fontsloth-otf-gvar--u32 data 8))
         (glyph-count (fontsloth-otf-gvar--u16 data 12))
         (flags (fontsloth-otf-gvar--u16 data 14))
         (data-offset (fontsloth-otf-gvar--u32 data 16))
         (offset-size (if (/= 0 (logand flags 1)) 4 2))
         (offsets (make-vector (1+ glyph-count) 0))
         (shared-tuples (make-vector shared-count nil)))
    (dotimes (i (1+ glyph-count))
      (aset offsets i
            (let ((offset (fontsloth-otf-gvar--read-uint
                           data (+ 20 (* i offset-size)) offset-size)))
              ;; In the short format offsets are stored in 16-bit words.
              (if (= offset-size 2) (* offset 2) offset))))
    (dotimes (i shared-count)
      (let ((coords (make-vector axis-count 0.0)))
        (dotimes (j axis-count)
          (aset coords j
                (fontsloth-otf-gvar--f2dot14
                 (fontsloth-otf-gvar--s16
                  data (+ shared-offset (* 2 (+ j (* i axis-count))))))))
        (aset shared-tuples i coords)))
    `((major . ,major)
      (minor . ,minor)
      (axis-count . ,axis-count)
      (glyph-count . ,glyph-count)
      (data . ,data)
      (data-offset . ,data-offset)
      (offsets . ,offsets)
      (shared-tuples . ,shared-tuples))))

(defun fontsloth-otf-gvar--glyph-data (gvar glyph-id)
  "Return GLYPH-ID's variation data from GVAR, or nil."
  (let* ((offsets (alist-get 'offsets gvar))
         (count (alist-get 'glyph-count gvar)))
    (when (< glyph-id count)
      (let* ((start (+ (alist-get 'data-offset gvar)
                       (aref offsets glyph-id)))
             (end (+ (alist-get 'data-offset gvar)
                     (aref offsets (1+ glyph-id))))
             (data (alist-get 'data gvar)))
        (when (< start end)
          (substring data start end))))))

(defun fontsloth-otf-gvar--decode-points (data offset point-count)
  "Decode packed points from DATA at OFFSET for POINT-COUNT."
  (let* ((header (aref data offset))
         (word? (/= 0 (logand header fontsloth-otf-gvar--points-are-words)))
         (count (if word?
                    (+ (ash (logand header fontsloth-otf-gvar--point-run-mask) 8)
                       (aref data (1+ offset)))
                  header))
         (pos (if word? (+ offset 2) (1+ offset))))
    (if (= count 0)
        (cons (cl-loop for i below point-count collect i) pos)
      (let ((points nil)
            (last 0))
        (while (< (length points) count)
          (let* ((run (aref data pos))
                 (run-count (1+ (logand run fontsloth-otf-gvar--point-run-mask)))
                 (run-word? (/= 0 (logand run fontsloth-otf-gvar--points-are-words)))
                 (entry-size (if run-word? 2 1)))
            (cl-incf pos)
            (dotimes (_ run-count)
              (let ((delta (fontsloth-otf-gvar--read-uint
                            data pos entry-size)))
                (cl-incf last delta)
                (push last points)
                (cl-incf pos entry-size)))))
        (cons (nreverse points) pos)))))

(defun fontsloth-otf-gvar--decode-deltas (data offset count)
  "Decode COUNT packed deltas from DATA at OFFSET.
Return a cons of the delta list and the next offset."
  (let ((deltas nil)
        (pos offset))
    (while (< (length deltas) count)
      (let* ((header (aref data pos))
             (run-count (1+ (logand header fontsloth-otf-gvar--delta-run-mask)))
             (kind (logand header fontsloth-otf-gvar--delta-size-mask)))
        (cl-incf pos)
        (if (= kind fontsloth-otf-gvar--delta-zero)
            (dotimes (_ run-count) (push 0 deltas))
          (let* ((size (cond ((= kind fontsloth-otf-gvar--delta-longs) 4)
                             ((= kind fontsloth-otf-gvar--delta-words) 2)
                             (t 1))))
            (dotimes (_ run-count)
              (let ((value (fontsloth-otf-gvar--read-uint data pos size)))
                (when (>= value (ash 1 (1- (* size 8))))
                  (setq value (- value (ash 1 (* size 8)))))
                (push value deltas)
                (cl-incf pos size)))))))
    (cons (nreverse deltas) pos)))

(defun fontsloth-otf-gvar--scalar (peak start end coords)
  "Return the region scalar for PEAK/START/END at COORDS."
  (let ((scalar 1.0))
    (cl-loop for i below (length peak)
             for p = (aref peak i)
             for s = (aref start i)
             for e = (aref end i)
             for value = (if (and coords (< i (length coords)))
                             (aref coords i)
                           0.0)
             do
             (setq scalar
                   (* scalar
                      (cond ((= p 0.0) 1.0)
                            ((or (< value s) (> value e)) 0.0)
                            ((= value p) 1.0)
                            ((< value p)
                             (if (= s p) 0.0 (/ (- value s) (- p s))))
                            (t (if (= e p) 0.0
                                 (/ (- e value) (- e p)))))))
             until (= scalar 0.0))
    scalar))

(defun fontsloth-otf-gvar--iup-axis (orig end-pts deltas)
  "Interpolate untouched DELTAS along ORIG using END-PTS."
  (let ((start 0))
    (dolist (end (append end-pts nil))
      (let* ((n (1+ end))
             (touched (cl-loop for i from start to end
                               when (aref deltas i) collect i)))
        (cond
         ((null touched))
         ((= 1 (length touched))
          (let ((d (aref deltas (car touched))))
            (cl-loop for i from start to end
                     unless (aref deltas i) do (aset deltas i d))))
         (t
          (cl-loop for rest on touched
                   for i0 = (car rest)
                   for i1 = (or (cadr rest) (car touched))
                   do
                   (let* ((c0 (aref orig i0))
                          (c1 (aref orig i1))
                          (d0 (aref deltas i0))
                          (d1 (aref deltas i1))
                          (k (mod (1+ i0) n)))
                     (while (/= k i1)
                       (unless (aref deltas k)
                         (let* ((c (aref orig k))
                                (d (cond
                                    ((= c0 c1) d0)
                                    ((< c0 c1)
                                     (cond ((<= c c0) d0)
                                           ((>= c c1) d1)
                                           (t (+ d0 (* (- c c0)
                                                       (/ (- d1 d0)
                                                          (- c1 c0)))))))
                                    (t
                                     (cond ((>= c c0) d0)
                                           ((<= c c1) d1)
                                           (t (+ d0 (* (- c c0)
                                                       (/ (- d1 d0)
                                                          (- c1 c0))))))))))
                           (aset deltas k d)))
                       (setq k (mod (1+ k) n)))))))
        (setq start (1+ end))))))

(defun fontsloth-otf-gvar--parse-tuple-header (glyph-data gvar header-pos)
  "Parse one tuple header at HEADER-POS.
Return (DATA-SIZE FLAGS PEAK START END NEXT-HEADER-POS)."
  (let* ((data-size (fontsloth-otf-gvar--u16 glyph-data header-pos))
         (flags (fontsloth-otf-gvar--u16 glyph-data (+ header-pos 2)))
         (axis-count (alist-get 'axis-count gvar))
         (peak (make-vector axis-count 0.0))
         (start (make-vector axis-count 0.0))
         (end (make-vector axis-count 0.0))
         (pos (+ header-pos 4))
         (embedded (/= 0 (logand flags
                                 fontsloth-otf-gvar--embedded-peak))))
    (if embedded
        (dotimes (i axis-count)
          (aset peak i
                (fontsloth-otf-gvar--f2dot14
                 (fontsloth-otf-gvar--s16 glyph-data (+ pos (* i 2)))))
          (setq pos (+ pos (* axis-count 2))))
      (setq peak (aref (alist-get 'shared-tuples gvar)
                       (logand flags fontsloth-otf-gvar--tuple-index-mask))))
    (if (/= 0 (logand flags fontsloth-otf-gvar--intermediate-region))
        (progn
          (dotimes (i axis-count)
            (aset start i
                  (fontsloth-otf-gvar--f2dot14
                   (fontsloth-otf-gvar--s16 glyph-data (+ pos (* i 2)))))
            (aset end i
                  (fontsloth-otf-gvar--f2dot14
                   (fontsloth-otf-gvar--s16
                    glyph-data (+ pos (* axis-count 2) (* i 2))))))
          (setq pos (+ pos (* axis-count 4))))
      (dotimes (i axis-count)
        (aset start i (min 0.0 (aref peak i)))
        (aset end i (max 0.0 (aref peak i)))))
    (list data-size flags peak start end pos)))

(defun fontsloth-otf-gvar--decode-tuple-deltas
    (glyph-data data-pos data-size flags point-count shared-points)
  "Decode one tuple's points and deltas.
Return (POINTS DX-LIST DY-LIST)."
  (let* ((point-data (substring glyph-data data-pos (+ data-pos data-size)))
         (private (/= 0 (logand flags fontsloth-otf-gvar--private-points)))
         (decoded-points (when private
                           (fontsloth-otf-gvar--decode-points
                            point-data 0 point-count)))
         (points (if private
                     (car decoded-points)
                   (or shared-points
                       (cl-loop for i below point-count collect i))))
         (delta-start (if private (cdr decoded-points) 0))
         (decoded-x (fontsloth-otf-gvar--decode-deltas
                     point-data delta-start (length points)))
         (dx-list (car decoded-x))
         (dy-list (car (fontsloth-otf-gvar--decode-deltas
                        point-data (cdr decoded-x) (length points)))))
    (list points dx-list dy-list)))

(defun fontsloth-otf-gvar--apply-tuple
    (total points dx-list dy-list scalar num-points end-pts
           x-coords y-coords)
  "Apply one tuple's deltas to TOTAL."
  (let ((dx (make-vector num-points nil))
        (dy (make-vector num-points nil)))
    (cl-loop for p in points
             for x in dx-list
             for y in dy-list
             when (< p num-points)
             do (progn
                  (aset dx p (* x scalar))
                  (aset dy p (* y scalar))))
    (fontsloth-otf-gvar--iup-axis x-coords end-pts dx)
    (fontsloth-otf-gvar--iup-axis y-coords end-pts dy)
    (dotimes (i num-points)
      (when (or (aref dx i) (aref dy i))
        (let ((pair (aref total i)))
          (aset total i
                (cons (+ (car pair) (or (aref dx i) 0.0))
                      (+ (cdr pair) (or (aref dy i) 0.0)))))))))

(defun fontsloth-otf-gvar-glyph-deltas
    (gvar glyph-id num-points end-pts x-coords y-coords coords)
  "Return per-point deltas for simple GLYPH-ID.
Returns a vector of (dx . dy) for the actual glyph points, or nil when
the glyph has no variation data."
  (let ((glyph-data (fontsloth-otf-gvar--glyph-data gvar glyph-id))
        (total (make-vector num-points (cons 0.0 0.0))))
    (when glyph-data
      (let* ((point-count (+ num-points 4))
             (tuple-count (fontsloth-otf-gvar--u16 glyph-data 0))
             (data-pos (fontsloth-otf-gvar--u16 glyph-data 2))
             (header-pos 4)
             (shared-points nil)
             (tuple-n (logand tuple-count
                              fontsloth-otf-gvar--tuple-index-mask)))
        (when (/= 0 (logand tuple-count
                            fontsloth-otf-gvar--tuples-share-points))
          (let ((decoded (fontsloth-otf-gvar--decode-points
                          glyph-data data-pos point-count)))
            (setq shared-points (car decoded)
                  data-pos (cdr decoded))))
        (dotimes (_ tuple-n)
          (pcase-let ((`(,data-size ,flags ,peak ,start ,end ,next-header)
                       (fontsloth-otf-gvar--parse-tuple-header
                        glyph-data gvar header-pos)))
            (let ((scalar (fontsloth-otf-gvar--scalar
                           peak start end coords)))
              (when (> scalar 0.0)
                (pcase-let ((`(,points ,dx-list ,dy-list)
                             (fontsloth-otf-gvar--decode-tuple-deltas
                              glyph-data data-pos data-size flags
                              point-count shared-points)))
                  (fontsloth-otf-gvar--apply-tuple
                   total points dx-list dy-list scalar num-points
                   end-pts x-coords y-coords))))
            (setq header-pos next-header)
            (setq data-pos (+ data-pos data-size))))
        total))))

(provide 'fontsloth-otf-gvar)
;;; fontsloth-otf-gvar.el ends here
