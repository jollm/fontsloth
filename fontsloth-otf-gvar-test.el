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
      ;; Expected values are from fontTools instancing at wght=900.
      (should (equal (append deltas nil)
                     '((-28.0 . 0.0) (-28.0 . -10.0)
                       (38.0 . -10.0) (38.0 . -126.0)
                       (112.0 . -126.0) (112.0 . 38.0)
                       (54.0 . 38.0) (54.0 . -78.0)
                       (112.0 . -78.0) (112.0 . 116.0)
                       (38.0 . 116.0) (38.0 . 0.0)))))))

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

(ert-deftest fontsloth-otf-gvar-test-composite-and-phantom ()
  "Check composite component deltas and phantom-point deltas."
  (let ((path (fontsloth-test--fixture-font
               "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((data (fontsloth-otf-gvar-test--font-data))
           (gvar (fontsloth-otf-gvar data))
           (glyph-id (fontsloth-otf-glyph-id-for-code-point ?i data))
           (deltas (fontsloth-otf-gvar-composite-deltas
                    gvar glyph-id 2 (vector 1.0)))
           (phantoms (fontsloth-otf-gvar-phantom-deltas
                      gvar glyph-id 6 (vector 1.0))))
      (should (equal (append deltas nil)
                     '((0.0 . 0.0) (32.0 . 0.0)
                       (0.0 . 0.0) (66.0 . 0.0)
                       (0.0 . 0.0) (0.0 . 0.0))))
      (should (equal phantoms
                     '((0.0 . 0.0) (66.0 . 0.0)
                       (0.0 . 0.0) (0.0 . 0.0))))
      (should (= 66.0 (fontsloth-otf-gvar-advance-delta
                       gvar glyph-id 6 (vector 1.0))))
      ;; High-level composite outlines must use component deltas too.
      (let* ((default (fontsloth-load-font path :cache 'bypass))
             (settings (fontsloth-font-settings-create
                        :variation-coords '(("wght" . 1.0))))
             (varied (fontsloth-load-font path :cache 'bypass
                                          :font-settings settings))
             (gid (fontsloth-font-glyph-id default ?i))
             (default-glyph (fontsloth-font-glyph default gid))
             (varied-glyph (fontsloth-font-glyph varied gid)))
        (should-not (equal
                     (fontsloth-test--glyph-signature default-glyph)
                     (fontsloth-test--glyph-signature varied-glyph)))))))

(ert-deftest fontsloth-otf-gvar-test-phantom-advance-fallback ()
  "Check that GVAR phantom deltas drive advances when HVAR is absent."
  (let* ((path (fontsloth-test--fixture-font
                "SourceSansVariable-Roman-subset.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((data (fontsloth-otf-gvar-test--font-data))
           (gid (fontsloth-otf-glyph-id-for-code-point ?A data))
           (data-tables (fontsloth-otf--font-tables data))
           (glyf (gethash "glyf" data-tables))
           (glyph-data (alist-get 'data (elt (alist-get 'glyphs glyf) gid)))
           (point-count (+ (alist-get 'num-points glyph-data) 4))
           (delta (fontsloth-otf-gvar-advance-delta
                   (fontsloth-otf-gvar data) gid point-count (vector 1.0)))
           (base (fontsloth-otf-glyph-hor-advance gid data)))
      ;; Simulate a font without HVAR; GVAR phantom points must take over.
      (remhash "HVAR" data-tables)
      (should (= (+ base delta)
                 (fontsloth-otf-glyph-hor-advance
                  gid data (vector 1.0)))))))

(provide 'fontsloth-otf-gvar-test)


;;; fontsloth-otf-gvar-test.el ends here
