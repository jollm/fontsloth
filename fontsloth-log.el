;;; fontsloth-log.el --- Fontsloth logging -*- lexical-binding: t -*-

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

(defcustom fontsloth-log-default-level 'fontsloth:info-level
  "Default logging level for fontsloth."
  :type '(choice (variable-item :tag "Error" fontsloth:error-level)
                 (variable-item :tag "Info" fontsloth:info-level)
                 (variable-item :tag "Verbose" fontsloth:verbose-level)
                 (variable-item :tag "Debug" fontsloth:debug-level))
  :group 'fontsloth)

(defvaralias 'fontsloth-log 'fontsloth-log--buffer-log
  "The logito log to dispatch on when logging.")

(defvar fontsloth-log--buffer-log
  (make-instance 'logito-buffer-object
                 :buffer fontsloth-log-buffer-name
                 :level (symbol-value fontsloth-log-default-level))
  "The symbol `logito-buffer-object' to dispatch on when logging.")

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

;;; add macro to avoid obj eval/method dispatch for logging noops

(defmacro fontsloth-log--def-log-macro (level-name)
  "Define a eval-avoiding helper macro for a logito logger.
E.g. if the level-name is \"info\" this creates fontsloth:info* to wrap
the logito generated fontsloth:info macro
LEVEL-NAME the level name as string such as info, debug, verbose, error"
  (let* ((prefix "fontsloth:")
         (suffix "*")
         (level (intern (concat prefix level-name "-level")))
         (logger (intern (concat prefix level-name))))
    `(defmacro ,(intern (concat prefix level-name suffix))
         (log string &rest objects)
       ,(format "Macro for `fontsloth:%s' to avoid eval if not %s logging."
                level-name level-name)
       `(when (<= ,',level (oref ,log level))
          (,',logger ,log ,string ,@objects)))))

(fontsloth-log--def-log-macro "error")
(fontsloth-log--def-log-macro "info")
(fontsloth-log--def-log-macro "verbose")
(fontsloth-log--def-log-macro "debug")

(provide 'fontsloth-log)
;;; fontsloth-log.el ends here
