;;; fontsloth-otf-hvar-test.el --- HVAR fixture checks -*- lexical-binding: t -*-

;;; Commentary:
;; The HVAR/gvar fixture is shared by the future HVAR and GVAR tests.

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-otf)
(require 'fontsloth-test-util)

(ert-deftest fontsloth-otf-hvar-test-fixture ()
  "Check that the deterministic TrueType variable fixture has HVAR and gvar."
  (let ((path (fontsloth-test--fixture-font
               "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-otf-load-font-data path))
           (tables (fontsloth-otf--font-tables font))
           (directory (gethash "table-directory" tables)))
      (should (gethash "HVAR" directory))
      (should (gethash "gvar" directory))
      (should (fontsloth-otf-fvar-axis "wght" font)))))

(ert-deftest fontsloth-otf-hvar-test-advance-delta ()
  "Check HVAR advance-width deltas and the threaded metric path."
  (let ((path (fontsloth-test--fixture-font
               "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-otf-load-font-data path))
           (hvar (fontsloth-otf-hvar font))
           (glyph-id (fontsloth-otf-glyph-id-for-code-point ?A font))
           (base (fontsloth-otf-glyph-hor-advance glyph-id font))
           (delta (fontsloth-otf-hvar-advance-delta
                   hvar glyph-id (vector 1.0))))
      (should hvar)
      (should (= 0.0 (fontsloth-otf-hvar-advance-delta
                      hvar glyph-id (vector 0.0))))
      (should (> delta 0.0))
      (should (= (+ base delta)
                 (fontsloth-otf-glyph-hor-advance
                  glyph-id font (vector 1.0))))
      ;; The high-level font path applies the same delta.
      (let* ((settings (fontsloth-font-settings-create
                        :variation-coords '(("wght" . 1.0))))
             (varied-font (fontsloth-load-font
                           path :cache 'bypass :font-settings settings))
             (varied-glyph (fontsloth-font-glyph varied-font glyph-id)))
        (should (= (fontsloth-glyph-advance-width varied-glyph)
                   (+ base delta)))))))

(provide 'fontsloth-otf-hvar-test)

;;; fontsloth-otf-hvar-test.el ends here
