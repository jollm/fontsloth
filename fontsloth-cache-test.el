;;; fontsloth-cache-test.el --- Tests for the font cache -*- lexical-binding: t -*-

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

;; fontsloth-cache-test.el:
;; Tests for the pcache-backed font cache, using an isolated temporary
;; pcache directory so the user's cache is untouched.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-cache)

(defmacro fontsloth-cache-test--with-cache (&rest body)
  "Run BODY with an isolated pcache directory and repository table."
  `(let* ((dir (make-temp-file "fontsloth-cache-test-" t))
          (pcache-directory dir)
          (*pcache-repositories* (make-hash-table :test 'equal))
          (fontsloth-cache--initialized? nil)
          ;; Avoid trying to persist while the test is running.
          (fontsloth-cache-idle-save-time 3600))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p dir)
         (delete-directory dir t)))))

(ert-deftest fontsloth-cache-test-path-name ()
  "Check the repository name derived from a font path."
  (should (equal (fontsloth-cache--pcache-path-name "/tmp/a.b.ttf")
                 "fontsloth-_tmp_a_b_ttf"))
  (should (equal (fontsloth-cache--pcache-path-name "/usr/share/f.ttf")
                 "fontsloth-_usr_share_f_ttf")))

(ert-deftest fontsloth-cache-test-put-get ()
  "Check putting, getting, and overwriting an entry."
  (fontsloth-cache-test--with-cache
   (should-not (fontsloth-cache-has "/tmp/a.ttf"))
   (should-not (fontsloth-cache-get "/tmp/a.ttf"))
   (fontsloth-cache-put "/tmp/a.ttf" 'first)
   (should (fontsloth-cache-has "/tmp/a.ttf"))
   (should (eq (fontsloth-cache-get "/tmp/a.ttf") 'first))
   (fontsloth-cache-put "/tmp/a.ttf" 'second)
   (should (eq (fontsloth-cache-get "/tmp/a.ttf") 'second))))

(ert-deftest fontsloth-cache-test-invalidate ()
  "Check invalidating one entry leaves others alone."
  (fontsloth-cache-test--with-cache
   (fontsloth-cache-put "/tmp/a.ttf" 'a)
   (fontsloth-cache-put "/tmp/b.ttf" 'b)
   (fontsloth-cache-invalidate "/tmp/a.ttf")
   (should-not (fontsloth-cache-has "/tmp/a.ttf"))
   (should-not (fontsloth-cache-get "/tmp/a.ttf"))
   (should (eq (fontsloth-cache-get "/tmp/b.ttf") 'b))))

(ert-deftest fontsloth-cache-test-clear ()
  "Check that clearing removes all fontsloth entries."
  (fontsloth-cache-test--with-cache
   (fontsloth-cache-put "/tmp/a.ttf" 'a)
   (fontsloth-cache-put "/tmp/b.ttf" 'b)
   (fontsloth-cache-clear)
   (should-not (fontsloth-cache-get "/tmp/a.ttf"))
   (should-not (fontsloth-cache-get "/tmp/b.ttf"))))

(ert-deftest fontsloth-cache-test-avoid-save-on-kill ()
  "Check that fontsloth repositories are hidden from pcache's kill hook."
  (let ((*pcache-repositories* (make-hash-table :test 'equal)))
    (puthash "fontsloth-a" 'a *pcache-repositories*)
    (puthash "other" 'other *pcache-repositories*)
    (fontsloth-cache--avoid-save-on-kill
     (lambda (&rest _)
       (should-not (gethash "fontsloth-a" *pcache-repositories*))
       (should (eq (gethash "other" *pcache-repositories*) 'other)))
     t)
    (should (eq (gethash "fontsloth-a" *pcache-repositories*) 'a))
    (should (eq (gethash "other" *pcache-repositories*) 'other))))

(provide 'fontsloth-cache-test)
;;; fontsloth-cache-test.el ends here
