;;; fontsloth-test-util.el --- Utilities for fontsloth tests -*- lexical-binding: t -*-

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

;; fontsloth-test-util.el:
;; Utilities for fontsloth tests.
;;

;;; Code:


(defvar fontsloth-test--font-directory
  (or (getenv "FONTSLOTH_TEST_FONT_DIR")
      (expand-file-name
       "test/fonts"
       (file-name-directory
        (or (locate-library "fontsloth-test-util")
            load-file-name
            buffer-file-name))))
  "Directory containing the font fixtures used by the test suite.
Override with the FONTSLOTH_TEST_FONT_DIR environment variable.")

(defun fontsloth-test--fixture-font (name)
  "Return the absolute path of fixture font NAME.
Tests should skip when the returned path does not exist."
  (expand-file-name name fontsloth-test--font-directory))

(defun fontsloth-test--fixture-font-p (name)
  "Return non-nil if fixture font NAME exists."
  (file-exists-p (fontsloth-test--fixture-font name)))

(provide 'fontsloth-test-util)
;;; fontsloth-test-util.el ends here
