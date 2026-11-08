;;; fontsloth-otf-gvar-test.el --- GVAR tests -*- lexical-binding: t -*-

;;; Commentary:
;; Simple-glyph GVAR deltas and the high-level variation path.

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-otf)
(require 'fontsloth-otf-gvar)
(require 'fontsloth-test-util)

(defun fontsloth-otf-gvar-test--font-data ()
  "Load the shared variable TrueType fixture."
  (fontsloth-otf-load-font-data
   (fontsloth-test--fixture-font "SourceSansVariable-Roman-subset.ttf")))

(ert-deftest fontsloth-otf-gvar-test-simple-deltas ()
  "Check simple-glyph deltas against the fixture's expected values."
  (let* ((path (fontsloth-test--fixture-font
                "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((data (fontsloth-otf-gvar-test--font-data))
           (glyph-id (fontsloth-otf-glyph-id-for-code-point ?A data))
           (glyf (gethash "glyf" (fontsloth-otf--font-tables data)))
           (glyph-data (alist-get 'data (elt (alist-get 'glyphs glyf)
                                             glyph-id)))
           (deltas (fontsloth-otf-gvar-glyph-deltas
                    (fontsloth-otf-gvar data) glyph-id
                    (alist-get 'num-points glyph-data)
                    (alist-get 'end-pts glyph-data)
                    (alist-get 'x-coords glyph-data)
                    (alist-get 'y-coords glyph-data)
                    (vector 1.0))))
      (should deltas)
      (should (= 18 (length deltas)))
      (should (equal (seq-take (append deltas nil) 5)
                     '((-20.0 . 0.0) (-58.0 . -10.0)
                       (122.0 . -10.0) (84.0 . 0.0)
                       (-64.0 . 0.0)))))))

(ert-deftest fontsloth-otf-gvar-test-iup ()
  "Check that points not explicitly varied are filled by IUP."
  (let* ((path (fontsloth-test--fixture-font
                "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((data (fontsloth-otf-gvar-test--font-data))
           (glyph-id (fontsloth-otf-glyph-id-for-code-point ?E data))
           (glyf (gethash "glyf" (fontsloth-otf--font-tables data)))
           (glyph-data (alist-get 'data (elt (alist-get 'glyphs glyf)
                                             glyph-id)))
           (deltas (fontsloth-otf-gvar-glyph-deltas
                    (fontsloth-otf-gvar data) glyph-id
                    (alist-get 'num-points glyph-data)
                    (alist-get 'end-pts glyph-data)
                    (alist-get 'x-coords glyph-data)
                    (alist-get 'y-coords glyph-data)
                    (vector 1.0))))
      (should deltas)
      ;; E has untouched points in its gvar tuples; IUP should fill them.
      (should (seq-every-p #'consp (append deltas nil))))))

(ert-deftest fontsloth-otf-gvar-test-high-level-variation ()
  "Check that the public font path applies GVAR to simple glyphs."
  (let ((path (fontsloth-test--fixture-font
               "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((default (fontsloth-load-font path :cache 'bypass))
           (settings (fontsloth-font-settings-create
                      :variation-coords '(("wght" . 1.0))))
           (varied (fontsloth-load-font path :cache 'bypass
                                        :font-settings settings))
           (glyph-id (fontsloth-font-glyph-id default ?A))
           (default-glyph (fontsloth-font-glyph default glyph-id))
           (varied-glyph (fontsloth-font-glyph varied glyph-id)))
      (should default-glyph)
      (should varied-glyph)
      (should-not (equal (fontsloth-test--glyph-signature default-glyph)
                         (fontsloth-test--glyph-signature varied-glyph))))))

(provide 'fontsloth-otf-gvar-test)
;;; fontsloth-otf-gvar-test.el ends here
