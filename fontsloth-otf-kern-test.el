;;; fontsloth-otf-kern-test.el --- Tests for kern table parsing -*- lexical-binding: t -*-

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

;; fontsloth-otf-kern-test.el:
;; Tests for kern format 0 and format 3 parsing using synthetic tables.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-otf-kern)

(defun fontsloth-otf-kern-test--u16 (value)
  "Return VALUE as a big-endian uint16 list."
  (list (logand (ash value -8) #xff) (logand value #xff)))

(defun fontsloth-otf-kern-test--u32 (value)
  "Return VALUE as a big-endian uint32 list."
  (append (fontsloth-otf-kern-test--u16 (logand (ash value -16) #xffff))
          (fontsloth-otf-kern-test--u16 (logand value #xffff))))

(defun fontsloth-otf-kern-test--s16 (value)
  "Return VALUE as a big-endian sint16 list."
  (fontsloth-otf-kern-test--u16 (logand value #xffff)))

(defun fontsloth-otf-kern-test--bytes (&rest lists)
  "Concatenate LISTS into a unibyte string."
  (apply #'unibyte-string (apply #'append lists)))

(defun fontsloth-otf-kern-test--format0-data ()
  "Return a format 0 subtable payload with two pairs."
  (append (fontsloth-otf-kern-test--u16 2)
          '(0 0 0 0 0 0)        ; searchRange, entrySelector, rangeShift
          (fontsloth-otf-kern-test--u16 1)
          (fontsloth-otf-kern-test--u16 2)
          (fontsloth-otf-kern-test--s16 -50)
          (fontsloth-otf-kern-test--u16 3)
          (fontsloth-otf-kern-test--u16 4)
          (fontsloth-otf-kern-test--s16 10)))

(defun fontsloth-otf-kern-test--format3-data ()
  "Return a format 3 subtable payload with two classes."
  (append (fontsloth-otf-kern-test--u16 2) ; glyph count
          '(2 2 2 0)                    ; values, left classes, right classes
          (fontsloth-otf-kern-test--s16 -5)
          (fontsloth-otf-kern-test--s16 10)
          '(0 1)                        ; left classes
          '(0 1)                        ; right classes
          '(0 0 0 1)))                  ; index for (1 . 1) -> value 1

(ert-deftest fontsloth-otf-kern-test-format0 ()
  "Check format 0 pair mapping."
  (let* ((data (fontsloth-otf-kern-test--format0-data))
         (table (bindat-unpack fontsloth-otf-kern--format0-spec
                               (fontsloth-otf-kern-test--bytes data)))
         (mappings (alist-get 'mappings table)))
    (should (= (alist-get 'num-pairs table) 2))
    (should (= (length (alist-get 'pairs table)) 2))
    (should (= (gethash (logior (ash 1 16) 2) mappings) -50))
    (should (= (gethash (logior (ash 3 16) 4) mappings) 10))
    (should (= (hash-table-count mappings) 2))))

(ert-deftest fontsloth-otf-kern-test-format3 ()
  "Check format 3 class mapping, including class 0 exclusion."
  (let* ((data (fontsloth-otf-kern-test--format3-data))
         (table (bindat-unpack fontsloth-otf-kern--format3-spec
                               (fontsloth-otf-kern-test--bytes data)))
         (mappings (alist-get 'mappings table)))
    (should (= (alist-get 'glyph-count table) 2))
    (should (equal (alist-get 'kerning-values table) [-5 10]))
    (should (= (gethash (logior (ash 1 16) 1) mappings) 10))
    ;; Class 0 means no kerning; those pairs must not be indexed.
    (should-not (gethash (logior (ash 0 16) 1) mappings))
    (should-not (gethash (logior (ash 1 16) 0) mappings))
    (should (= (hash-table-count mappings) 1))))

(ert-deftest fontsloth-otf-kern-test-format3-direct ()
  "Check the format 3 mapping function directly."
  (let ((mappings (fontsloth-otf-kern--format3-mappings
                   2 [0 1] [0 1] 2 2 [0 0 0 1] [-5 10])))
    (should (= (gethash (logior (ash 1 16) 1) mappings) 10))
    (should (= (hash-table-count mappings) 1))))

(ert-deftest fontsloth-otf-kern-test-spec-format0 ()
  "Check the version 0 outer kern spec with a format 0 subtable."
  (let* ((data (fontsloth-otf-kern-test--format0-data))
         (length (+ 6 (length data)))
         (subtable (append (fontsloth-otf-kern-test--u16 0)
                           (fontsloth-otf-kern-test--u16 length)
                           '(0 0))) ; format 0, coverage 0
         (table (bindat-unpack
                 fontsloth-otf-kern-spec
                 (fontsloth-otf-kern-test--bytes
                  (fontsloth-otf-kern-test--u16 0)
                  (fontsloth-otf-kern-test--u16 1)
                  subtable data)))
         (mappings (alist-get 'mappings table)))
    (should (= (alist-get 'major-version table) 0))
    (should (= (alist-get 'num-sub-tables table) 1))
    (should (= (gethash (logior (ash 1 16) 2) mappings) -50))
    (should (= (gethash (logior (ash 3 16) 4) mappings) 10))))

(ert-deftest fontsloth-otf-kern-test-spec-format3 ()
  "Check the version 0 outer kern spec with a format 3 subtable."
  (let* ((data (fontsloth-otf-kern-test--format3-data))
         (length (+ 6 (length data)))
         (subtable (append (fontsloth-otf-kern-test--u16 0)
                           (fontsloth-otf-kern-test--u16 length)
                           '(3 0))) ; format 3, coverage 0
         (table (bindat-unpack
                 fontsloth-otf-kern-spec
                 (fontsloth-otf-kern-test--bytes
                  (fontsloth-otf-kern-test--u16 0)
                  (fontsloth-otf-kern-test--u16 1)
                  subtable data)))
         (mappings (alist-get 'mappings table)))
    (should (= (alist-get 'major-version table) 0))
    (should (= (gethash (logior (ash 1 16) 1) mappings) 10))))

(ert-deftest fontsloth-otf-kern-test-spec-version1-header ()
  "Check the version 1 subtable header layout."
  (let* ((data (fontsloth-otf-kern-test--format0-data))
         (length (+ 8 (length data)))
         (subtable (append (fontsloth-otf-kern-test--u32 length)
                           '(0)                ; coverage
                           '(0)                ; format 0
                           (fontsloth-otf-kern-test--u16 0))) ; tuple index
         (table (bindat-unpack
                 fontsloth-otf-kern-spec
                 (fontsloth-otf-kern-test--bytes
                  (fontsloth-otf-kern-test--u16 1)
                  (fontsloth-otf-kern-test--u16 0)
                  (fontsloth-otf-kern-test--u32 1)
                  subtable data)))
         (mappings (alist-get 'mappings table)))
    (should (= (alist-get 'major-version table) 1))
    (should (= (alist-get 'minor-version table) 0))
    (should (= (gethash (logior (ash 1 16) 2) mappings) -50))))

(provide 'fontsloth-otf-kern-test)
;;; fontsloth-otf-kern-test.el ends here
