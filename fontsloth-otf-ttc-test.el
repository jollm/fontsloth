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

(provide 'fontsloth-otf-ttc-test)
;;; fontsloth-otf-ttc-test.el ends here
