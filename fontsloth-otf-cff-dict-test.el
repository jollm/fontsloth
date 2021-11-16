;;; fontsloth-otf-cff-dict-test.el --- Test a CFF dictionary parser -*- lexical-binding: t -*-

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

;; fontsloth-otf-cff-dict-test (this file): Tests for a CFF dictionary parser

;;; Code:

(require 'ert)
(require 'fontsloth-otf-cff-dict)

(ert-deftest fontsloth-otf-cff-dict-test-parse-number ()
  (should (equal (fontsloth-otf-cff-dict-parse-number #xfa [#xfa #x7c] 0)
                 '(1000 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number #xfe [#xfe #x7c] 0)
                 '(-1000 1)))
  (should (equal (fontsloth-otf-cff-dict-parse-number #x1c [#x1c #x27 #x10] 0)
                 '(10000 2)))
  (should (equal (fontsloth-otf-cff-dict-parse-number #x1c [#x1c #xd8 #xf0] 0)
                 '(-10000 2)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                  #x1d [#x1d #x00 #x01 #x86 #xa0] 0)
                 '(100000 4)))
  (should (equal (fontsloth-otf-cff-dict-parse-number
                       #x1d [#x1d #xff #xfe #x79 #x60] 0)
                 '(-100000 4))))

(provide 'fontsloth-otf-cff-dict-test)
;;; fontsloth-otf-cff-dict-test.el ends here
