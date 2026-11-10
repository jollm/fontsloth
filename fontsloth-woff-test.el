;;; fontsloth-woff-test.el --- Tests for WOFF parsing -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.21.0
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

;; fontsloth-woff-test.el:
;; Tests for the WOFF header, table directory, and decompression.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-woff)
(require 'fontsloth-test-util)

(declare-function zlib-available-p "decompress.c")

(defun fontsloth-woff-test--u16 (bytes index value)
  "Store VALUE as a big-endian uint16 in BYTES at INDEX."
  (aset bytes index (logand (ash value -8) #xff))
  (aset bytes (1+ index) (logand value #xff)))

(defun fontsloth-woff-test--u32 (bytes index value)
  "Store VALUE as a big-endian uint32 in BYTES at INDEX."
  (fontsloth-woff-test--u16 bytes index (logand (ash value -16) #xffff))
  (fontsloth-woff-test--u16 bytes (+ index 2) (logand value #xffff)))

(defun fontsloth-woff-test--string (bytes index string)
  "Store STRING as bytes in BYTES at INDEX."
  (dotimes (i (length string))
    (aset bytes (+ index i) (aref string i))))

(ert-deftest fontsloth-woff-test-header ()
  "Check the WOFF header spec."
  (let ((bytes (make-vector 44 0)))
    (fontsloth-woff-test--string bytes 0 "wOFF")
    (fontsloth-woff-test--string bytes 4 "OTTO")
    (fontsloth-woff-test--u32 bytes 8 1234)
    (fontsloth-woff-test--u16 bytes 12 3)
    (fontsloth-woff-test--u32 bytes 16 999)
    (fontsloth-woff-test--u16 bytes 20 1)
    (fontsloth-woff-test--u32 bytes 24 100)
    (fontsloth-woff-test--u32 bytes 28 10)
    (fontsloth-woff-test--u32 bytes 32 20)
    (fontsloth-woff-test--u32 bytes 36 30)
    (fontsloth-woff-test--u32 bytes 40 40)
    (let ((header (bindat-unpack fontsloth-woff--header-spec bytes)))
      (should (equal (alist-get 'signature header) "wOFF"))
      (should (equal (alist-get 'sfnt-version header) "OTTO"))
      (should (= (alist-get 'length header) 1234))
      (should (= (alist-get 'num-tables header) 3))
      (should (= (alist-get 'total-sfnt-size header) 999))
      (should (= (alist-get 'major-version header) 1))
      (should (= (alist-get 'meta-offset header) 100))
      (should (= (alist-get 'meta-length header) 10))
      (should (= (alist-get 'meta-orig-length header) 20))
      (should (= (alist-get 'priv-offset header) 30))
      (should (= (alist-get 'priv-length header) 40)))))

(ert-deftest fontsloth-woff-test-table-props ()
  "Check a single WOFF table directory entry."
  (let ((bytes (make-vector 20 0)))
    (fontsloth-woff-test--string bytes 0 "cmap")
    (fontsloth-woff-test--u32 bytes 4 10)
    (fontsloth-woff-test--u32 bytes 8 5)
    (fontsloth-woff-test--u32 bytes 12 5)
    (fontsloth-woff-test--u32 bytes 16 123)
    (let ((props (bindat-unpack fontsloth-woff--table-props-spec bytes)))
      (should (equal (alist-get 'tag props) "cmap"))
      (should (= (alist-get 'offset props) 10))
      (should (= (alist-get 'comp-length props) 5))
      (should (= (alist-get 'orig-length props) 5))
      (should (= (alist-get 'orig-checksum props) 123)))))

(ert-deftest fontsloth-woff-test-maybe-decompress-uncompressed ()
  "Check that uncompressed tables are returned unchanged."
  (let ((props '((comp-length . 5) (orig-length . 5) (offset . 0))))
    (should (equal (fontsloth-woff--maybe-decompress-table props "hello")
                   "hello")))
  (should (equal (fontsloth-woff--maybe-decompress-table nil "hello")
                 "hello")))

(ert-deftest fontsloth-woff-test-maybe-decompress-compressed ()
  "Check zlib decompression of a compressed table."
  (skip-unless (fontsloth-test--zlib-available-p))
  ;; zlib.compress(b"hello")
  (let* ((compressed [120 156 203 72 205 201 201 7 0 6 44 2 21])
         (props `((comp-length . ,(length compressed))
                  (orig-length . 5)
                  (offset . 0)))
         (bytes (apply #'unibyte-string (append compressed nil))))
    (should (equal (fontsloth-woff--maybe-decompress-table props bytes)
                   "hello")))
  ;; The table region is replaced in place; the rest of the font bytes
  ;; before and after it are preserved.
  (let* ((compressed [120 156 203 72 205 201 201 7 0 6 44 2 21])
         (props `((comp-length . ,(length compressed))
                  (orig-length . 5)
                  (offset . 1)))
         (bytes (concat "X" (apply #'unibyte-string (append compressed nil))
                        "Y")))
    (should (equal (fontsloth-woff--maybe-decompress-table props bytes)
                   "XhelloY"))))

(ert-deftest fontsloth-woff-test-maybe-decompress-garbage ()
  "Check that corrupt compressed table data signals an error."
  (skip-unless (fontsloth-test--zlib-available-p))
  (should-error
   (fontsloth-woff--maybe-decompress-table
    '((comp-length . 4) (orig-length . 5) (offset . 0))
    (unibyte-string 0 0 0 0))))

(provide 'fontsloth-woff-test)

;;; fontsloth-woff-test.el ends here
