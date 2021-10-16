;;; fontsloth-log.el --- Fontsloth logging -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.0.1
;; Package-Requires: ((logito "0.1") (emacs "26.1"))
;; Keywords: true-type, font, ttf, otf

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

;; fontsloth-log.el:
;; Provides logging functionality for fontsloth
;;

;;; Code:

(require 'logito)

(logito-def-level error 0 fontsloth)
(logito-def-level info 5 fontsloth)
(logito-def-level verbose 10 fontsloth)
(logito-def-level debug 15 fontsloth)

(defcustom fontsloth-log-buffer-name "*fontsloth-log*"
  "Fontsloth log buffer name."
  :type 'string
  :group 'fontsloth)

(defvar fontsloth-log--buffer-log
  (make-instance 'logito-buffer-object
                 :buffer fontsloth-log-buffer-name
                 :level fontsloth:error-level)
  "The `logito-buffer-object' to dispatch on when logging.")

(defun fontsloth-log-level-error ()
  "Set fontsloth logging level to error."
  (interactive)
  (oset fontsloth-log--buffer-log level fontsloth:error-level))
(defun fontsloth-log-level-info ()
  "Set fontsloth logging level to info."
  (interactive)
  (oset fontsloth-log--buffer-log level fontsloth:info-level))
(defun fontsloth-log-level-verbose ()
  "Set fontsloth logging level to verbose."
  (interactive)
  (oset fontsloth-log--buffer-log level fontsloth:verbose-level))
(defun fontsloth-log-level-debug ()
  "Set fontsloth logging level to debug."
  (interactive)
  (oset fontsloth-log--buffer-log level fontsloth:debug-level))

;;; usage e.g:
;; (fontsloth:info fontsloth-log--buffer-log "an info %s" an-obj)

(provide 'fontsloth-log)
;;; fontsloth-log.el ends here
