;;; benchmark.el --- Reproducible fontsloth load/raster benchmark -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

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

;;; Commentary:

;; Usage:
;;   emacs --batch -L . -l scripts/benchmark.el -- [FONT...]
;;
;; With no FONT arguments, benchmarks the checked-in fixtures and, if set,
;; the font named by FONTSLOTH_BENCH_FONT.  FONTSLOTH_BENCH_PX controls the
;; raster pixel size (default 32.0), and FONTSLOTH_BENCH_RASTER_LIMIT can
;; cap the number of code points rasterized per pass.
;;
;; Reports wall time, bytes allocated (from memory-use-counts), and retained
;; heap (from garbage-collect) for parsing, high-level load + outline, and
;; two raster passes.  Run this before and after memory work.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'fontsloth)

(defconst benchmark--sizes
  [16 8 8 48 1 56 32]
  "Approximate byte size of each memory-use-counts element.
Order: conses, floats, vector-cells, symbols, string-chars, intervals,
strings.  These match the sizes reported by `garbage-collect' on 64-bit
Emacs; the retained-heap figure comes from `garbage-collect' directly.")

(defun benchmark--used-bytes ()
  "Return the approximate retained heap in bytes from `garbage-collect'."
  (cl-loop for (type size used) in (garbage-collect)
           sum (* size used)))

(defun benchmark--alloc-bytes (before after)
  "Return approximate bytes allocated between counts vectors BEFORE and AFTER."
  (cl-loop for i below (length benchmark--sizes)
           sum (* (aref benchmark--sizes i)
                  (max 0 (- (nth i after) (nth i before))))))

(cl-defun benchmark--measure (fn)
  "Run FN and return a plist with :value, :time, :alloc, and :retained."
  (garbage-collect)
  (let ((used-before (benchmark--used-bytes))
        (counts-before (memory-use-counts))
        (start (float-time)))
    (let ((value (funcall fn)))
      (list :value value
            :time (- (float-time) start)
            :alloc (benchmark--alloc-bytes counts-before (memory-use-counts))
            :retained (- (benchmark--used-bytes) used-before)))))

(defun benchmark--print (label result)
  "Print LABEL and the measurements in RESULT."
  (princ (format "  %-14s %9.3fs  alloc=%9.2fMB  retained=%9.2fMB\n"
                 label
                 (plist-get result :time)
                 (/ (plist-get result :alloc) 1048576.0)
                 (/ (plist-get result :retained) 1048576.0))))

(defun benchmark--env-number (name default)
  "Return environment variable NAME as a number, or DEFAULT."
  (let ((value (getenv name)))
    (if value (string-to-number value) default)))

(defun benchmark--raster-pass (font code-points px)
  "Rasterize CODE-POINTS from FONT at PX, discarding the results."
  (dolist (code-point code-points)
    (let ((glyph-id (fontsloth-font-glyph-id font code-point)))
      (when glyph-id
        (fontsloth-font-rasterize font glyph-id px)))))

(defun benchmark--font (path)
  "Measure PATH."
  (princ (format "FONT %s (%d bytes)\n" path
                 (file-attribute-size
                  (file-attributes (file-truename path)))))
  (let ((parsed nil))
    (benchmark--print "parse"
                      (benchmark--measure
                       (lambda ()
                         (setq parsed
                               (fontsloth-otf-load-font-data path)))))
    (princ (format "  parsed glyphs=%s\n"
                   (fontsloth-otf-num-glyphs parsed)))
    (setq parsed nil))
  (let* ((lazy? (not (= 0 (benchmark--env-number "FONTSLOTH_BENCH_LAZY" 1))))
         (settings (fontsloth-font-settings-create :lazy-outline? lazy?))
         (font nil))
    (princ (format "  outline-mode=%s
" (if lazy? "lazy" "eager")))
    (benchmark--print (if lazy? "load(lazy)" "load+outline")
                      (benchmark--measure
                       (lambda ()
                         (setq font (fontsloth-load-font
                                     path :cache 'bypass
                                     :font-settings settings)))))
    (unless (getenv "FONTSLOTH_BENCH_SKIP_FORCE")
      (benchmark--print "force-all"
                        (benchmark--measure
                         (lambda ()
                           (fontsloth-font-all-glyphs font)))))
    (let* ((all-code-points (fontsloth-font-code-points font))
           (limit (benchmark--env-number "FONTSLOTH_BENCH_RASTER_LIMIT"
                                         (length all-code-points)))
           (code-points (seq-take all-code-points limit))
           (px (benchmark--env-number "FONTSLOTH_BENCH_PX" 32.0)))
      (princ (format "  glyphs=%d code-points=%d rasterized=%d px=%s\n"
                     (length (fontsloth-font-glyphs font))
                     (length all-code-points)
                     (length code-points)
                     px))
      (benchmark--print "raster-first"
                        (benchmark--measure
                         (lambda ()
                           (benchmark--raster-pass font code-points px))))
      (benchmark--print "raster-second"
                        (benchmark--measure
                         (lambda ()
                           (benchmark--raster-pass font code-points px)))))
    (setq font nil))
  (garbage-collect))

(defun benchmark--default-paths ()
  "Return the fixture paths and the optional FONTSLOTH_BENCH_FONT."
  (append
   '("test/fonts/fontsloth-test.ttf"
     "test/fonts/fontsloth-test.otf"
     "test/fonts/fontsloth-test.woff"
     "test/fonts/IBMPlexSerif-Regular-subset.otf")
   (let ((extra (getenv "FONTSLOTH_BENCH_FONT")))
     (when (and extra (not (string-empty-p extra)))
       (list extra)))))

(let* ((args (if (equal (car command-line-args-left) "--")
                 (cdr command-line-args-left)
               command-line-args-left))
       (paths (or args (benchmark--default-paths))))
  (princ (format "EMACS %s\nDATE %s\n"
                 emacs-version
                 (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
  (dolist (path paths)
    (if (file-exists-p path)
        (benchmark--font path)
      (princ (format "SKIP %s (not found)\n" path)))))

(provide 'benchmark)
;;; benchmark.el ends here
