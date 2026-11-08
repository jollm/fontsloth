;;; fontsloth-otf-hvar-test.el --- HVAR fixture checks -*- lexical-binding: t -*-

;;; Commentary:
;; The HVAR/gvar fixture is shared by the future HVAR and GVAR tests.

;;; Code:

(require 'ert)
(require 'f)
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

(provide 'fontsloth-otf-hvar-test)
;;; fontsloth-otf-hvar-test.el ends here
