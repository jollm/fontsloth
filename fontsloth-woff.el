;;; fontsloth-woff.el --- Unpack/pack Web Open Font Format (WOFF)  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.17.0
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, bindat, ttf, otf, parsing

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

;; Part of fontsloth: the slowest font renderer in the world written in pure
;; elisp.  inspired by fontdue

;; fontsloth-woff (this file): uses bindat to parse woff files

;; To use this module by itself, load and enable it as follows:
;;   (use-package fontsloth-woff)

;;; Code:

(require 'bindat)

(defvar fontsloth-woff--header-spec
  (bindat-type
    (signature str 4)
    (sfnt-version str 4)
    (length uint 32)
    (num-tables uint 16)
    (_ fill 2)
    (total-sfnt-size uint 32)
    (major-version uint 16)
    (minor-version uint 16)
    (meta-offset uint 32)
    (meta-length uint 32)
    (meta-orig-length uint 32)
    (priv-offset uint 32)
    (priv-length uint 32))
  "A spec for a WOFF file header.
see URL https://www.w3.org/TR/WOFF/#WOFFHeader")

(defvar fontsloth-woff--table-props-spec
  (bindat-type
    (tag str 4)
    (offset uint 32)
    (comp-length uint 32)
    (orig-length uint 32)
    (orig-checksum uint 32))
  "Bindat spec for a single entry in the WOFF table directory.
see URL https://www.w3.org/TR/WOFF/#TableDirectory")

(declare-function zlib-available-p "decompress.c")
(declare-function zlib-decompress-region "decompress.c")

(defun fontsloth-woff--maybe-decompress-table (table-props bytes)
  "Return font BYTES with table referenced by TABLE-PROPS decompressed."
  (unless (zlib-available-p)
    (error "woff requires an Emacs with zlib"))
  (if-let* ((comp-length (alist-get 'comp-length table-props))
            (orig-length (alist-get 'orig-length table-props))
            (offset (alist-get 'offset table-props))
            (compressed? (/= comp-length orig-length)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert bytes)
        (if (zlib-decompress-region (1+ offset) (+ (1+ offset) comp-length))
            (buffer-string)
          (error "failed to decompress %s" table-props)))
    bytes))

(provide 'fontsloth-woff)
;;; fontsloth-woff.el ends here
