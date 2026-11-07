;;; fontsloth-invariant-test.el --- Outline and raster invariants -*- lexical-binding: t -*-

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

;; fontsloth-invariant-test.el:
;; Invariants that should survive the lazy-outlining and memory work:
;; outline signatures match between the high-level font and a fresh parser,
;; match committed golden hashes, bounds contain all line endpoints, rasters
;; are stable across sizes/repeats, and the cache round-trips outlines.
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'seq)
(require 'fontsloth)
(require 'fontsloth-test-util)

(defvar fontsloth-invariant-test--fixtures
  '("fontsloth-test.ttf"
    "fontsloth-test.otf"
    "fontsloth-test.woff"
    "IBMPlexSerif-Regular-subset.otf"))

(defvar fontsloth-invariant-test--golden-file
  (expand-file-name
   "test/data/outline-golden.el"
   (file-name-directory (locate-library "fontsloth-test-util")))
  "Committed outline hashes, relative to the repository root.")

(defvar fontsloth-invariant-test--golden nil
  "Alist of fixture name to (glyph-id . hash) pairs, when loaded.")

(when (file-exists-p fontsloth-invariant-test--golden-file)
  (load fontsloth-invariant-test--golden-file nil t))

(defmacro fontsloth-invariant-test--with-cache (&rest body)
  "Run BODY with an isolated pcache directory."
  `(let* ((dir (make-temp-file "fontsloth-invariant-cache-" t))
          (pcache-directory dir)
          (*pcache-repositories* (make-hash-table :test 'equal))
          (fontsloth-cache--initialized? nil)
          (fontsloth-cache-idle-save-time 3600))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

(ert-deftest fontsloth-invariant-test-outline-matches-parser ()
  "Check the high-level outline against a fresh parser for every glyph."
  (dolist (name fontsloth-invariant-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (file-exists-p path))
      (let ((font (fontsloth-load-font path :cache 'bypass)))
        (should (equal (fontsloth-test--outline-signatures-from-data path)
                       (fontsloth-test--outline-signatures font)))))))

(ert-deftest fontsloth-invariant-test-golden-outlines ()
  "Check every fixture glyph against the committed outline hashes."
  (skip-unless fontsloth-invariant-test--golden)
  (dolist (name fontsloth-invariant-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name))
          (golden (alist-get name fontsloth-invariant-test--golden
                             nil nil #'equal)))
      (skip-unless (and (file-exists-p path) golden))
      (let* ((font (fontsloth-load-font path :cache 'bypass))
             (glyphs (fontsloth-font-all-glyphs font)))
        (should (= (length golden)
                   (length (fontsloth-test--outline-signatures font))))
        (dolist (pair golden)
          (let ((glyph (aref glyphs (car pair))))
            (should glyph)
            (should (equal (fontsloth-test--glyph-hash glyph)
                           (cdr pair)))))))))

(ert-deftest fontsloth-invariant-test-bounds-contain-lines ()
  "Check that every line endpoint lies within the glyph bounds."
  (dolist (name fontsloth-invariant-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (file-exists-p path))
      (let* ((font (fontsloth-load-font path :cache 'bypass))
             (glyphs (fontsloth-font-all-glyphs font)))
        (dolist (glyph-id (fontsloth-test--glyph-ids
                           (fontsloth-font-char-to-glyph font)))
          (let* ((glyph (aref glyphs glyph-id))
                 (bounds (fontsloth-glyph-bounds glyph))
                 ;; Glyph line coordinates are relative to the glyph's
                 ;; bounding box (repositioning subtracts the origin).
                 (width (fontsloth-glyph-outline-bounds-width bounds))
                 (height (fontsloth-glyph-outline-bounds-height bounds))
                 (epsilon 1.0e-6)
                 (lines (append (append (fontsloth-glyph-v-lines glyph) nil)
                                (append (fontsloth-glyph-m-lines glyph) nil))))
            (dolist (line lines)
              (let ((coords (fontsloth-line-coords line)))
                (dolist (x (list (fontsloth-coords-x0 coords)
                                 (fontsloth-coords-x1 coords)))
                  (should (<= (- epsilon) x))
                  (should (<= x (+ width epsilon))))
                (dolist (y (list (fontsloth-coords-y0 coords)
                                 (fontsloth-coords-y1 coords)))
                  (should (<= (- epsilon) y))
                  (should (<= y (+ height epsilon))))))))))))

(ert-deftest fontsloth-invariant-test-line-orientation ()
  "Check vertical lines are vertical and m-lines are not vertical."
  (dolist (name fontsloth-invariant-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (file-exists-p path))
      (let* ((font (fontsloth-load-font path :cache 'bypass))
             (glyphs (fontsloth-font-all-glyphs font)))
        (dolist (glyph-id (fontsloth-test--glyph-ids
                           (fontsloth-font-char-to-glyph font)))
          (let ((glyph (aref glyphs glyph-id)))
            (dolist (line (append (fontsloth-glyph-v-lines glyph) nil))
              (let ((coords (fontsloth-line-coords line)))
                (should (equal (fontsloth-coords-x0 coords)
                               (fontsloth-coords-x1 coords)))))
            (dolist (line (append (fontsloth-glyph-m-lines glyph) nil))
              (let ((coords (fontsloth-line-coords line)))
                (should-not (equal (fontsloth-coords-x0 coords)
                                   (fontsloth-coords-x1 coords)))))))))))

(ert-deftest fontsloth-invariant-test-raster-stability ()
  "Check repeated rasters at several sizes are identical."
  (dolist (name fontsloth-invariant-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (file-exists-p path))
      (let* ((font (fontsloth-load-font path :cache 'bypass))
             (ids (fontsloth-test--glyph-ids
                   (fontsloth-font-char-to-glyph font)))
             (ids (seq-take ids (min 20 (length ids)))))
        (dolist (px '(12.0 17.5 32.0 64.0))
          (dolist (glyph-id ids)
            (let ((first (fontsloth-test--raster-signature font glyph-id px)))
              (should (equal first
                             (fontsloth-test--raster-signature
                              font glyph-id px))))))))))

(ert-deftest fontsloth-invariant-test-cache-roundtrip ()
  "Check that a cached font preserves every outline."
  (dolist (name fontsloth-invariant-test--fixtures)
    (let ((path (fontsloth-test--fixture-font name)))
      (skip-unless (file-exists-p path))
      (fontsloth-invariant-test--with-cache
       (let ((font (fontsloth-load-font path))
             (cached nil))
         (setq cached (fontsloth-cache-get path))
         (should cached)
         (should (fontsloth-font-source cached))
         ;; Drop the transient parser data so forcing outlines must
         ;; reparse via the cached font's source path.
         (remhash cached fontsloth--lazy-fonts)
         (should (equal (fontsloth-test--outline-signatures cached)
                        (fontsloth-test--outline-signatures-from-data path)))
         (should (equal (fontsloth-test--outline-signatures cached)
                        (fontsloth-test--outline-signatures font))))))))

(provide 'fontsloth-invariant-test)
;;; fontsloth-invariant-test.el ends here
