;;; fontsloth-otf-cff2-test.el --- CFF2 table tests -*- lexical-binding: t -*-

;;; Commentary:
;; Table-level tests for CFF2 fonts. Outlining CFF2 charstrings is not
;; implemented yet.

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-geometry)
(require 'fontsloth-otf)
(require 'fontsloth-otf-cff2)
(require 'fontsloth-test-util)

(ert-deftest fontsloth-otf-cff2-test-table ()
  "Check CFF2 header, charstrings, FDArray, and variation store parsing."
  (let ((path (fontsloth-test--fixture-font "Cantarell-VF-subset.otf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-otf-load-font-data path))
           (cff2 (fontsloth-otf-cff2 font)))
      (should cff2)
      (should (= 2 (alist-get 'major cff2)))
      (should (= 96 (fontsloth-otf-cff2-num-glyphs cff2)))
      (should (= 1 (length (alist-get 'font-dicts cff2))))
      (should (= 96 (length (fontsloth-otf-cff2-fd-select cff2))))
      (should (seq-every-p (lambda (fd) (= fd 0))
                           (fontsloth-otf-cff2-fd-select cff2)))
      (should (> (length (fontsloth-otf-cff2-char-string cff2 1)) 0))
      (let* ((font-dict (fontsloth-otf-cff2-font-dict cff2 1))
             (private-dict (alist-get 'private-dict font-dict)))
        (should private-dict)
        (should (alist-get 'local-subrs-offset private-dict))
        (should (alist-get 'local-subrs font-dict)))
      (should (alist-get 'variation-store cff2))
      (should (> (alist-get 'count (alist-get 'variation-store cff2)) 0))
      (let ((axis (fontsloth-otf-fvar-axis "wght" font)))
        (should axis)
        (should (= 100.0 (alist-get 'min-value axis)))
        (should (= 400.0 (alist-get 'default-value axis)))
        (should (= 800.0 (alist-get 'max-value axis)))))))

(ert-deftest fontsloth-otf-cff2-test-rejects-cff1-header ()
  "A non-CFF2 major version is rejected."
  (should-error (fontsloth-otf-cff2--parse (vector 1 0 5 0 0))))

(ert-deftest fontsloth-otf-cff2-test-outline ()
  "Check CFF2 charstring outlining, including blend at the default instance."
  (let ((path (fontsloth-test--fixture-font "Cantarell-VF-subset.otf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-otf-load-font-data path))
           (cff2 (fontsloth-otf-cff2 font))
           (geom (fontsloth-geometry-create 1.0 1000)))
      (fontsloth-otf-cff2-outline cff2 1 geom)
      (should (> (+ (length (fontsloth-geometry-v-lines geom))
                    (length (fontsloth-geometry-m-lines geom)))
                 0)))
    ;; The high-level path should dispatch to CFF2 as well.
    (let* ((font (fontsloth-load-font path :cache 'bypass))
           (glyph-id (fontsloth-font-glyph-id font ?A))
           (glyph (fontsloth-font-glyph font glyph-id)))
      (should glyph)
      (should (> (+ (length (fontsloth-glyph-v-lines glyph))
                    (length (fontsloth-glyph-m-lines glyph)))
                 0)))))

(provide 'fontsloth-otf-cff2-test)

;;; fontsloth-otf-cff2-test.el ends here
