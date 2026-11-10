;;; fontsloth-cache.el --- Fontsloth pcache -*- lexical-binding: t -*-

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

;; fontsloth-cache.el:
;; Provides cache functionality for fontsloth
;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'pcache)

(defconst fontsloth-cache-internal-version-constant 2)
(defconst fontsloth-cache-version-constant
  (format "%s/fontsloth-%s"
          pcache-version-constant fontsloth-cache-internal-version-constant)
  "Version constant used for cache invalidation.")

(defcustom fontsloth-cache-idle-save-time 60
  "Idle time in seconds after which cache entries are saved.

A nil value or zero indicates to save immediately."
  :type '(choice natnum (const :tag "Immediate" nil))
  :group 'fontsloth)

(defclass fontsloth-cache-pcache (pcache-repository)
  ((entries :initarg :entries :initform (make-hash-table :test 'equal))))

(oset-default 'fontsloth-cache-pcache version-constant
              fontsloth-cache-version-constant)

;;; TODO: separate caches for fonts and for raster output

(defconst fontsloth-cache--pcache-prefix "fontsloth-"
  "The pcache file name prefix for all fontsloth related repositories.")

(defsubst fontsloth-cache--pcache-path-name (file-path &optional collection-index)
  "Given FILE-PATH and COLLECTION-INDEX, generate the pcache path name."
  (format "%s%s%s"
          fontsloth-cache--pcache-prefix
          (replace-regexp-in-string "[/\\.]" "_" file-path)
          (if (and collection-index (/= collection-index 0))
              (format "_c%s" collection-index)
            "")))

(defvar fontsloth-cache--initialized? nil
  "Nil if fontsloth cache repositories are not yet loaded.")

(defun fontsloth-cache--avoid-save-on-kill (orig-fun &rest args)
  "Avoid saving fontsloth related pcache repositories when killing Emacs.

The entries are read-only and saved onced on first put.  This around
advice should preserve the existing pcache logic for all other
repositories.

ORIG-FUN ::= the function being advised
ARGS ::= any arguments passed to that function"
  (let ((fontsloth-repos
         (cl-loop for k being the hash-keys of *pcache-repositories*
                  when (string-prefix-p fontsloth-cache--pcache-prefix k)
                  collect
                  `(,k . ,(gethash k *pcache-repositories*))
                  and do
                  (remhash k *pcache-repositories*))))
    (apply orig-fun args)
    (cl-loop for (k . v) in fontsloth-repos
             do
             (puthash k v *pcache-repositories*))))

(declare-function fontsloth-font-settings-collection-index "fontsloth" (settings))

(defun fontsloth-cache--load-repos (&optional force? filter)
  "Load any pcache repos whose names start with `fontsloth-cache--pcache-prefix'.

FORCE? if non-nil, re-load even if already initialized
FILTER a list of (PATH . FONT-SETTINGS) to load if available"
  (let ((partial? (and filter (consp filter))))
    (when (or (not fontsloth-cache--initialized?) force?)
      (advice-add 'pcache-kill-emacs-hook :around #'fontsloth-cache--avoid-save-on-kill)
      (when-let* ((pcache-files (and (file-directory-p pcache-directory)
                                    (directory-files pcache-directory))))
        (cl-loop for f in pcache-files
                 with filter = (when partial?
                                 (mapcar
                                  (lambda (source)
                                    (fontsloth-cache--pcache-path-name
                                     (car source)
                                     (fontsloth-font-settings-collection-index
                                      (cdr source))))
                                  filter))
                 unless (gethash f *pcache-repositories*)
                 when (string-prefix-p fontsloth-cache--pcache-prefix f)
                 when (or (null filter) (cl-member f filter :test #'equal))
                 do (make-instance 'fontsloth-cache-pcache
                                   :object-name f)))
      ;; A filtered load must not prevent a later full cache scan.
      (unless partial?
        (setq fontsloth-cache--initialized? t)))))

(declare-function fontsloth--font-cache-snapshot "fontsloth" (font))
(declare-function fontsloth-font-p "fontsloth" (object))

(defun fontsloth-cache-put (file-path font &optional collection-index)
  "Put `fontsloth-font' FONT in cache using lookup key FILE-PATH.
COLLECTION-INDEX selects the TTC face.  Lazy fonts are stored as compact
snapshots rather than serializing whatever glyphs and kern map happen to
be loaded at save time."
  (fontsloth-cache--load-repos)
  (let* ((repo-name (fontsloth-cache--pcache-path-name
                     file-path collection-index))
         (cache (or (gethash repo-name *pcache-repositories*)
                    (make-instance 'fontsloth-cache-pcache
                                   :object-name repo-name)))
         (value (if (and (fboundp 'fontsloth--font-cache-snapshot)
                         (fboundp 'fontsloth-font-p)
                         (fontsloth-font-p font))
                    (fontsloth--font-cache-snapshot font)
                  font)))
    (pcache-put cache file-path value)
    ;; fonts objects are read-only, so just try once to save them
    (run-with-idle-timer
     (or fontsloth-cache-idle-save-time 0) nil
     (lambda ()
       (pcache-save cache t)))))

(defun fontsloth-cache-has (file-path &optional collection-index)
  "Return non-nil if cache has a font for FILE-PATH at COLLECTION-INDEX."
  (when-let* ((repo-name (fontsloth-cache--pcache-path-name
                          file-path collection-index))
              (cache (gethash repo-name *pcache-repositories*)))
    (pcache-has cache file-path)))

(defun fontsloth-cache-get (file-path &optional collection-index)
  "Retrieve a cached font for FILE-PATH at COLLECTION-INDEX, if any.

The value is nil if no entry is found."
  (fontsloth-cache--load-repos)
  (when-let* ((repo-name (fontsloth-cache--pcache-path-name
                          file-path collection-index))
              (cache (gethash repo-name *pcache-repositories*)))
    (pcache-get cache file-path)))

(defun fontsloth-cache-invalidate (file-path &optional collection-index)
  "Invalidate a single font for FILE-PATH at COLLECTION-INDEX."
  (fontsloth-cache--load-repos)
  (when-let* ((repo-name (fontsloth-cache--pcache-path-name
                          file-path collection-index))
              (cache (gethash repo-name *pcache-repositories*)))
    (pcache-invalidate cache file-path)
    (pcache-destroy-repository repo-name)))

(defun fontsloth-cache-clear ()
  "Clear all fontsloth related repositories from pcache."
  (fontsloth-cache--load-repos)
  (cl-loop for k being the hash-keys of *pcache-repositories*
           when (string-prefix-p fontsloth-cache--pcache-prefix k)
           do (pcache-destroy-repository k)))

(provide 'fontsloth-cache)
;;; fontsloth-cache.el ends here
