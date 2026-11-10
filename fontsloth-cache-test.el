;;; fontsloth-cache-test.el --- Tests for the font cache -*- lexical-binding: t -*-

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

;; fontsloth-cache-test.el:
;; Tests for the pcache-backed font cache, using an isolated temporary
;; pcache directory so the user's cache is untouched.
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-cache)
(require 'fontsloth-test-util)

(defmacro fontsloth-cache-test--with-cache (&rest body)
  "Run BODY with an isolated pcache directory and repository table."
  `(let* ((dir (make-temp-file "fontsloth-cache-test-" t))
          (pcache-directory (file-name-as-directory dir))
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

(ert-deftest fontsloth-cache-test-lazy-snapshot ()
  "Check that cache-put stores a compact lazy snapshot."
  (let ((path (fontsloth-test--fixture-font
               "IBMPlexSerif-Regular-subset.otf")))
    (skip-unless (f-exists-p path))
    (fontsloth-cache-test--with-cache
     (let* ((font (fontsloth-load-font path :cache 'bypass))
            (glyph-id (fontsloth-font-glyph-id font ?&)))
       (should glyph-id)
       ;; Fill transient state before caching.
       (fontsloth-font-glyph font glyph-id)
       (fontsloth-font-kern-map font)
       (should (aref (fontsloth-font-glyphs font) glyph-id))
       (should (fontsloth-font-horizontal-kern-loaded? font))
       (fontsloth-cache-put path font)
       (let ((cached (fontsloth-cache-get path)))
         (should cached)
         (should-not (aref (fontsloth-font-glyphs cached) glyph-id))
         (should-not (fontsloth-font-horizontal-kern cached))
         (should-not (fontsloth-font-horizontal-kern-loaded? cached))
         (should (equal (fontsloth-test--outline-signatures cached)
                        (fontsloth-test--outline-signatures font))))))))

(ert-deftest fontsloth-cache-test-lazy-snapshot-persists ()
  "Check that a compact lazy snapshot survives a pcache save/reload."
  (let ((path (fontsloth-test--fixture-font
               "IBMPlexSerif-Regular-subset.otf")))
    (skip-unless (f-exists-p path))
    (fontsloth-cache-test--with-cache
     (let* ((font (fontsloth-load-font path :cache 'bypass))
            (glyph-id (fontsloth-font-glyph-id font ?&))
            (repo-name (fontsloth-cache--pcache-path-name path)))
       (fontsloth-font-glyph font glyph-id)
       (fontsloth-cache-put path font)
       (pcache-save (gethash repo-name *pcache-repositories*) t)
       (clrhash *pcache-repositories*)
       (setq fontsloth-cache--initialized? nil)
       (fontsloth-cache--load-repos)
       (let ((cached (fontsloth-cache-get path)))
         (should cached)
         (should-not (aref (fontsloth-font-glyphs cached) glyph-id))
         (should-not (fontsloth-font-horizontal-kern-loaded? cached))
         (should (equal (fontsloth-test--outline-signatures cached)
                        (fontsloth-test--outline-signatures font))))))))

(ert-deftest fontsloth-cache-test-collection-index ()
  "Check that different TTC faces use independent cache entries."
  (fontsloth-cache-test--with-cache
   (let ((path "/tmp/fontsloth-test.ttc"))
     (fontsloth-cache-put path 'face-0 0)
     (fontsloth-cache-put path 'face-1 1)
     (should (eq (fontsloth-cache-get path 0) 'face-0))
     (should (eq (fontsloth-cache-get path 1) 'face-1))
     (should (fontsloth-cache-has path 1))
     (fontsloth-cache-invalidate path 0)
     (should-not (fontsloth-cache-has path 0))
     (should (eq (fontsloth-cache-get path 1) 'face-1)))))

(ert-deftest fontsloth-cache-test-collection-fonts ()
  "Check that loaded TTC faces round-trip through the cache independently."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test.ttc")))
    (skip-unless (f-exists-p path))
    (fontsloth-cache-test--with-cache
     (let* ((settings-0 (fontsloth-font-settings-create :collection-index 0))
            (settings-1 (fontsloth-font-settings-create :collection-index 1))
            (font-0 (fontsloth-load-font path :font-settings settings-0))
            (font-1 (fontsloth-load-font path :font-settings settings-1)))
       (should (gethash "glyf"
                        (fontsloth-otf--font-tables
                         (fontsloth--font-data font-0))))
       (should (gethash "CFF "
                        (fontsloth-otf--font-tables
                         (fontsloth--font-data font-1))))
       (should (fontsloth-cache-has path 0))
       (should (fontsloth-cache-has path 1))))))

(ert-deftest fontsloth-cache-test-load-repos-filter ()
  "Load only the requested repositories without blocking a later scan."
  (fontsloth-cache-test--with-cache
   (let* ((path-a "/tmp/fontsloth-filter-a.ttf")
          (path-b "/tmp/fontsloth-filter-b.ttf")
          (settings (fontsloth-font-settings-create))
          (repo-a (fontsloth-cache--pcache-path-name path-a))
          (repo-b (fontsloth-cache--pcache-path-name path-b)))
     (fontsloth-cache-put path-a 'a)
     (fontsloth-cache-put path-b 'b)
     (pcache-save (gethash repo-a *pcache-repositories*) t)
     (pcache-save (gethash repo-b *pcache-repositories*) t)
     (clrhash *pcache-repositories*)
     (setq fontsloth-cache--initialized? nil)
     (fontsloth-cache--load-repos
      t (list (cons path-a
                    (fontsloth-font-settings-collection-index settings))))
     (should (gethash repo-a *pcache-repositories*))
     (should-not (gethash repo-b *pcache-repositories*))
     (should-not fontsloth-cache--initialized?)
     (fontsloth-cache--load-repos)
     (should fontsloth-cache--initialized?)
     (should (gethash repo-b *pcache-repositories*)))))

(ert-deftest fontsloth-cache-test-async-mixed-settings ()
  "Load mixed per-source settings through the async cache path."
  (let ((ttf (fontsloth-test--fixture-font "fontsloth-test.ttf"))
        (ttc (fontsloth-test--fixture-font "fontsloth-test.ttc"))
        (init-file (make-temp-file "fontsloth-async-init-" nil ".el")))
    (skip-unless (and (file-exists-p ttf) (file-exists-p ttc)))
    (unwind-protect
        (fontsloth-cache-test--with-cache
         (let* ((settings-1 (fontsloth-font-settings-create
                             :collection-index 1))
                (child-load-path
                 (mapcar (lambda (path)
                           (if (file-name-absolute-p path)
                               path
                             (expand-file-name path)))
                         load-path))
                (sources (list ttf (cons ttc settings-1))))
           (with-temp-file init-file
             (insert (format "(setq load-path '%S)\n" child-load-path))
             (insert (format "(setq pcache-directory %S)\n"
                             pcache-directory)))
           (let ((async-child-init init-file))
             (with-timeout (30 (ert-fail "async cache load timed out"))
               (async-wait
                (fontsloth-async-load-and-cache-fonts sources))))
           (should (fontsloth-cache-has ttf))
           (should (fontsloth-cache-has ttc 1))
           (should-not (fontsloth-cache-has ttc 0))
           (let* ((font (fontsloth-cache-get ttc 1))
                  (settings (fontsloth-font-settings font)))
             (should font)
             (should (= 1 (fontsloth-font-settings-collection-index
                           settings))))))
      (when (file-exists-p init-file) (delete-file init-file)))))

(provide 'fontsloth-cache-test)


;;; fontsloth-cache-test.el ends here
