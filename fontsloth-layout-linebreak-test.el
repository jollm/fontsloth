;;; fontsloth-layout-linebreak-test.el --- Tests for line breaking -*- lexical-binding: t -*-

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

;; fontsloth-layout-linebreak-test.el:
;; Tests for the line break data masks, character classification, and the
;; state machine.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-layout-linebreak)

(defun fontsloth-layout-linebreak-test--bits (string)
  "Return the break bits produced while stepping through STRING."
  (let ((linebreaker (fontsloth-layout-linebreak-er-create)))
    (fontsloth-layout-linebreak-er-reset linebreaker)
    (cl-loop for char across string
             collect (fontsloth-layout-linebreak-data-bits
                      (fontsloth-layout-linebreak-er-next linebreaker char)))))

(ert-deftest fontsloth-layout-linebreak-test-data-from-mask ()
  "Check soft/hard break mask construction."
  (should (= (fontsloth-layout-linebreak-data-bits
              (fontsloth-layout-linebreak-data-from-mask nil nil nil))
             0))
  (should (= (fontsloth-layout-linebreak-data-bits
              (fontsloth-layout-linebreak-data-from-mask t nil nil))
             0))
  (should (= (fontsloth-layout-linebreak-data-bits
              (fontsloth-layout-linebreak-data-from-mask t t nil))
             fontsloth-layout-linebreak--hard))
  (should (= (fontsloth-layout-linebreak-data-bits
              (fontsloth-layout-linebreak-data-from-mask t nil t))
             fontsloth-layout-linebreak--soft))
  (should (= (fontsloth-layout-linebreak-data-bits
              (fontsloth-layout-linebreak-data-from-mask t t t))
             (logior fontsloth-layout-linebreak--soft
                     fontsloth-layout-linebreak--hard))))

(ert-deftest fontsloth-layout-linebreak-test-predicates ()
  "Check the exact soft/hard predicates and mask intersection."
  (should (fontsloth-layout-linebreak-data-soft-p
           fontsloth-layout-linebreak-soft))
  (should-not (fontsloth-layout-linebreak-data-soft-p
               fontsloth-layout-linebreak-hard))
  (should (fontsloth-layout-linebreak-data-hard-p
           fontsloth-layout-linebreak-hard))
  (should-not (fontsloth-layout-linebreak-data-hard-p
               fontsloth-layout-linebreak-soft))
  (let ((both (fontsloth-layout-linebreak-data-create
               :bits (logior fontsloth-layout-linebreak--soft
                             fontsloth-layout-linebreak--hard))))
    (should (= (fontsloth-layout-linebreak-data-bits
                (fontsloth-layout-linebreak-data-mask both
                                                      fontsloth-layout-linebreak-soft))
               fontsloth-layout-linebreak--soft))
    (should (= (fontsloth-layout-linebreak-data-bits
                (fontsloth-layout-linebreak-data-mask both
                                                      fontsloth-layout-linebreak-none))
               0))))

(ert-deftest fontsloth-layout-linebreak-test-er-reset ()
  "Check linebreaker state reset."
  (let ((linebreaker (fontsloth-layout-linebreak-er-create :state 5)))
    (fontsloth-layout-linebreak-er-reset linebreaker)
    (should (= (fontsloth-layout-linebreak-er-state linebreaker) 0))))

(ert-deftest fontsloth-layout-linebreak-test-u8->i8 ()
  "Check signed-byte interpretation."
  (should (= (fontsloth-layout-linebreak--u8->i8 0) 0))
  (should (= (fontsloth-layout-linebreak--u8->i8 127) 127))
  (should (= (fontsloth-layout-linebreak--u8->i8 128) -128))
  (should (= (fontsloth-layout-linebreak--u8->i8 255) -1)))

(ert-deftest fontsloth-layout-linebreak-test-state-machine ()
  "Check representative line-breaking sequences."
  (should (equal (fontsloth-layout-linebreak-test--bits "a b")
                 '(0 0 1)))
  (should (equal (fontsloth-layout-linebreak-test--bits "a\tb")
                 '(0 0 1)))
  (should (equal (fontsloth-layout-linebreak-test--bits "a\nb")
                 '(0 0 2)))
  (should (equal (fontsloth-layout-linebreak-test--bits "-a")
                 '(0 1)))
  ;; No break around a non-breaking space.
  (should (equal (fontsloth-layout-linebreak-test--bits "a\u00a0b")
                 '(0 0 0)))
  ;; Soft break opportunity after a hyphen followed by a letter.
  (should (equal (fontsloth-layout-linebreak-test--bits "a-b")
                 '(0 0 1))))

(ert-deftest fontsloth-layout-linebreak-test-char-data-classify ()
  "Check character metadata flags."
  (let ((classify #'fontsloth-layout-linebreak-char-data-classify))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?a 1))
               0))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?a 0))
               fontsloth-layout-linebreak-char-data-missing))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\s 1))
               fontsloth-layout-linebreak-char-data-whitespace))
    ;; Tab and newline are both whitespace and control characters.
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\t 1))
               (logior fontsloth-layout-linebreak-char-data-whitespace
                       fontsloth-layout-linebreak-char-data-control)))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\n 1))
               (logior fontsloth-layout-linebreak-char-data-whitespace
                       fontsloth-layout-linebreak-char-data-control)))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\x01 1))
               fontsloth-layout-linebreak-char-data-control))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\x7f 1))
               fontsloth-layout-linebreak-char-data-control))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\s 0))
               (logior fontsloth-layout-linebreak-char-data-missing
                       fontsloth-layout-linebreak-char-data-whitespace)))
    (should (= (fontsloth-layout-linebreak-char-data-bits
                (funcall classify ?\x01 0))
               (logior fontsloth-layout-linebreak-char-data-missing
                       fontsloth-layout-linebreak-char-data-control)))))

(ert-deftest fontsloth-layout-linebreak-test-char-data-predicates ()
  "Check whitespace, control, and missing predicates."
  (let ((whitespace (fontsloth-layout-linebreak-char-data-classify ?\s 1))
        (control (fontsloth-layout-linebreak-char-data-classify ?\x01 1))
        (missing (fontsloth-layout-linebreak-char-data-classify ?a 0))
        (plain (fontsloth-layout-linebreak-char-data-classify ?a 1)))
    (should (fontsloth-layout-linebreak-char-data-whitespace-p whitespace))
    (should-not (fontsloth-layout-linebreak-char-data-whitespace-p plain))
    (should (fontsloth-layout-linebreak-char-data-control-p control))
    (should-not (fontsloth-layout-linebreak-char-data-control-p plain))
    (should (fontsloth-layout-linebreak-char-data-missing-p missing))
    (should-not (fontsloth-layout-linebreak-char-data-missing-p plain))))

(provide 'fontsloth-layout-linebreak-test)
;;; fontsloth-layout-linebreak-test.el ends here
