;;; package-lint.el --- batch package-lint for elwl -*- lexical-binding: t; -*-

;; Copyright (C) 2026  fontsloth contributors

;;; Commentary:

;; Batch package-lint runner for the `maak package-lint' task.  Files to
;; check are passed on the command line; the package metadata comes from
;; elwl.el.

;;; Code:

(unless (require 'package-lint nil t)
  (error "package-lint is not installed; install it before running this task"))

(package-initialize)

(setq package-lint-main-file
      (expand-file-name "fontsloth.el" default-directory))

;;; package-lint.el ends here
