;;; fontsloth-otf-.el --- Common utilities for an Elisp otf/ttf bindat parser -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
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

;; fontsloth-otf- (this file): common utilities for an Elisp otf/ttf bindat
;; parser

;;; Code:

(require 'cl-lib)

(defun fontsloth-otf--with-offset-fn (o rewind s)
  "Offset spec S by O and go back to start if REWIND is t (the fn version).

This function is intended to be used within macros."
  (let ((start (cl-gensym "start")))
    `(let ((,start))
       (bindat-type
         (_ unit (progn (setq ,start bindat-idx bindat-idx ,o) nil))
         (_ type  ,s)
         (_ unit (progn (when ,rewind (setq bindat-idx ,start)) nil))))))

(defmacro fontsloth-otf--with-offset (o rewind s)
  "Offset spec S by O and go back to start if REWIND is t (the macro version)."
  (fontsloth-otf--with-offset-fn o rewind s))

(provide 'fontsloth-otf-)
;;; fontsloth-otf-.el ends here
