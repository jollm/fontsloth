;;; fontsloth-otf-ttc-test.el --- TTC face selection tests -*- lexical-binding: t -*-

;;; Commentary:
;; TTC header and face-selection tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-otf)
(require 'fontsloth-test-util)

(ert-deftest fontsloth-otf-ttc-test-face-selection ()
  "Select the TTF and CFF faces from a TTC."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test.ttc")))
    (skip-unless (f-exists-p path))
    (let* ((ttf (fontsloth-otf-load-font-data path :coll-index 0))
           (cff (fontsloth-otf-load-font-data path :coll-index 1))
           (ttf-tables (fontsloth-otf--font-tables ttf))
           (cff-tables (fontsloth-otf--font-tables cff)))
      (should (gethash "glyf" ttf-tables))
      (should-not (gethash "CFF " ttf-tables))
      (should (gethash "CFF " cff-tables))
      (should-not (gethash "glyf" cff-tables))
      (should (= (fontsloth-otf-num-glyphs ttf)
                 (fontsloth-otf-num-glyphs cff)))
      (should (= 2 (fontsloth-otf-ttc-num-fonts path)))
      (should-error (fontsloth-otf-load-font-data path :coll-index 2)))))

(ert-deftest fontsloth-otf-ttc-test-high-level-face ()
  "Load a selected TTC face through the public font API."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test.ttc")))
    (skip-unless (f-exists-p path))
    (let* ((settings (fontsloth-font-settings-create :collection-index 1))
           (font (fontsloth-load-font path :cache 'bypass
                                      :font-settings settings))
           (data (fontsloth--font-data font))
           (glyph-id (fontsloth-font-glyph-id font ?A))
           (glyph (fontsloth-font-glyph font glyph-id)))
      (should (gethash "CFF " (fontsloth-otf--font-tables data)))
      (should glyph-id)
      (should glyph)
      (should (> (+ (length (fontsloth-glyph-v-lines glyph))
                    (length (fontsloth-glyph-m-lines glyph)))
                 0)))))

(defun fontsloth-otf-ttc-test--header (&rest tail)
  "Build a synthetic TTC header with TAIL bytes."
  (apply #'unibyte-string (append (string-to-list "ttcf") tail)))

(ert-deftest fontsloth-otf-ttc-test-invalid-headers ()
  "Reject malformed and unsupported TTC headers."
  (dolist (bytes
           (list (unibyte-string ?t ?t ?c ?f)
                 ;; zero faces
                 (fontsloth-otf-ttc-test--header 0 1 0 0 0 0 0 0)
                 ;; two faces but only one offset
                 (fontsloth-otf-ttc-test--header 0 1 0 0 0 0 0 2 0 0 0 20)
                 ;; face offset points into the header
                 (fontsloth-otf-ttc-test--header 0 1 0 0 0 0 0 1 0 0 0 12)
                 ;; unsupported version
                 (fontsloth-otf-ttc-test--header 0 3 0 0 0 0 0 1 0 0 0 20)))
    (should-error (fontsloth-otf--ttc-header bytes)))
  (let ((path (fontsloth-test--fixture-font "fontsloth-test.ttf")))
    (skip-unless (f-exists-p path))
    (should-error (fontsloth-otf-ttc-num-fonts path))))

(ert-deftest fontsloth-otf-ttc-test-version-2 ()
  "Accept TTC version 2 headers."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test.ttc")))
    (skip-unless (f-exists-p path))
    (let* ((bytes (fontsloth-otf--read-font-bytes path))
           (copy (copy-sequence bytes)))
      (aset copy 4 0)
      (aset copy 5 2)
      (should (fontsloth-otf--ttc-header copy)))))

(ert-deftest fontsloth-otf-ttc-test-compat-wrapper ()
  "Select a face through the compatibility loader."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test.ttc")))
    (skip-unless (f-exists-p path))
    (let ((fontsloth-otf--current-tables nil))
      (fontsloth-otf-load-font path :coll-index 1)
      (should (gethash "CFF " fontsloth-otf--current-tables)))))

(ert-deftest fontsloth-otf-ttc-test-cff2-face ()
  "Select a CFF2 face from a collection."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test-cff2.ttc")))
    (skip-unless (f-exists-p path))
    (let* ((data (fontsloth-otf-load-font-data path :coll-index 1))
           (tables (fontsloth-otf--font-tables data)))
      (should (gethash "CFF2" tables))
      (should (fontsloth-otf-cff2 data))
      (should (= 2 (fontsloth-otf-ttc-num-fonts path))))))

(provide 'fontsloth-otf-ttc-test)

;;; fontsloth-otf-ttc-test.el ends here
