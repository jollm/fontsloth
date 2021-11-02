;;; fontsloth--common-types.el --- Useful types -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, ttf, otf

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

;; fontsloth--common-types (this file): Broadly useful types

;;; Code:

(require 'cl-lib)

(cl-float-limits)

(cl-defstruct
    (fontsloth-coords
     (:constructor fontsloth-coords-create)
     (:copier nil)
     (:type list))
  x0 y0 x1 y1)

(cl-defstruct
    (fontsloth-nudge
     (:constructor fontsloth-nudge-create)
     (:copier nil)
     (:type list))
  start-x-nudge start-y-nudge end-x-nudge end-y-nudge)

(cl-defstruct
    (fontsloth-adj
     (:constructor fontsloth-adj-create)
     (:copier nil)
     (:type list))
  x-first-adj y-first-adj)

(cl-defstruct
    (fontsloth-params
     (:constructor fontsloth-params-create)
     (:copier nil)
     (:type list))
  tdx tdy dx dy)

(cl-defstruct
    (fontsloth-line
     (:constructor fontsloth-make-line)
     (:copier nil))
  (coords nil :type 'fontsloth-coords)
  (nudge nil :type 'fontsloth-nudge)
  (adjustment nil :type 'fontsloth-adj)
  (params nil :type 'fontsloth-params))

(require 'fontsloth-bbox)
(require 'fontsloth-point)
(require 'fontsloth-line)

(provide 'fontsloth--common-types)
;;; fontsloth--common-types.el ends here
