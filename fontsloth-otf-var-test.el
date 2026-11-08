;;; fontsloth-otf-var-test.el --- tests -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for fvar and ItemVariationStore parsing.

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth-otf)
(require 'fontsloth-otf-var)
(require 'fontsloth-test-util)

(defun fontsloth-otf-var-test--u16 (n)
  (list (logand (ash n -8) #xff) (logand n #xff)))

(defun fontsloth-otf-var-test--u32 (n)
  (append (fontsloth-otf-var-test--u16 (logand (ash n -16) #xffff))
          (fontsloth-otf-var-test--u16 (logand n #xffff))))

(defun fontsloth-otf-var-test--fixed (x)
  (fontsloth-otf-var-test--u32 (round (* x 65536.0))))

(defun fontsloth-otf-var-test--f2dot14 (x)
  (fontsloth-otf-var-test--u16 (logand (round (* x 16384.0)) #xffff)))

(defun fontsloth-otf-var-test--fvar-bytes ()
  (vconcat
   (append
    ;; header: major, minor, axes offset, reserved, axis count, axis size,
    ;; instance count, instance size
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u16 0)
    (fontsloth-otf-var-test--u16 16)
    (fontsloth-otf-var-test--u16 0)
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u16 20)
    (fontsloth-otf-var-test--u16 2)
    (fontsloth-otf-var-test--u16 10)
    ;; wght axis
    (list ?w ?g ?h ?t)
    (fontsloth-otf-var-test--fixed 100.0)
    (fontsloth-otf-var-test--fixed 400.0)
    (fontsloth-otf-var-test--fixed 900.0)
    (fontsloth-otf-var-test--u16 0)
    (fontsloth-otf-var-test--u16 256)
    ;; instances
    (fontsloth-otf-var-test--u16 257)
    (fontsloth-otf-var-test--u16 0)
    (fontsloth-otf-var-test--fixed 400.0)
    (fontsloth-otf-var-test--u16 258)
    (fontsloth-otf-var-test--u16 259)
    (fontsloth-otf-var-test--u16 0)
    (fontsloth-otf-var-test--fixed 700.0)
    (fontsloth-otf-var-test--u16 260))))

(ert-deftest fontsloth-otf-var-test-fvar ()
  (let* ((table (bindat-unpack fontsloth-otf-var--fvar-spec
                               (fontsloth-otf-var-test--fvar-bytes)))
         (axes (alist-get 'axes (alist-get 'axes table)))
         (instances (alist-get 'instances (alist-get 'instances table)))
         (axis (aref axes 0))
         (first (aref instances 0))
         (second (aref instances 1)))
    (should (= 1 (length axes)))
    (should (equal "wght" (alist-get 'tag axis)))
    (should (= 100.0 (alist-get 'min-value axis)))
    (should (= 400.0 (alist-get 'default-value axis)))
    (should (= 900.0 (alist-get 'max-value axis)))
    (should (= 256 (alist-get 'name-id axis)))
    (should (= 2 (length instances)))
    (should (= 257 (alist-get 'subfamily-name-id first)))
    (should (= 400.0 (aref (alist-get 'coordinates first) 0)))
    (should (= 258 (alist-get 'postscript-name-id first)))
    (should (= 700.0 (aref (alist-get 'coordinates second) 0)))
    (should (= 260 (alist-get 'postscript-name-id second)))
    (should (= -0.5 (fontsloth-otf-var-normalize axis 250.0)))
    (should (= 0.6 (fontsloth-otf-var-normalize axis 700.0)))))

(defun fontsloth-otf-var-test--ivs-bytes ()
  (vconcat
   (append
    ;; header
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u32 12)
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u32 28)
    ;; region list: one axis, two regions
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u16 2)
    (fontsloth-otf-var-test--f2dot14 0.0)
    (fontsloth-otf-var-test--f2dot14 1.0)
    (fontsloth-otf-var-test--f2dot14 1.0)
    (fontsloth-otf-var-test--f2dot14 0.0)
    (fontsloth-otf-var-test--f2dot14 0.5)
    (fontsloth-otf-var-test--f2dot14 1.0)
    ;; variation data
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u16 2)
    (fontsloth-otf-var-test--u16 2)
    (fontsloth-otf-var-test--u16 0)
    (fontsloth-otf-var-test--u16 1)
    (fontsloth-otf-var-test--u16 100)
    (fontsloth-otf-var-test--u16 40))))

(ert-deftest fontsloth-otf-var-test-item-variation-store ()
  (let* ((store (bindat-unpack fontsloth-otf-var--item-variation-store-spec
                               (fontsloth-otf-var-test--ivs-bytes)))
         (regions (alist-get 'regions (alist-get 'regions store)))
         (data (aref (alist-get 'variation-data store) 0)))
    (should (= 2 (length regions)))
    (should (= 2 (alist-get 'region-count (alist-get 'regions store))))
    (should (= 1 (alist-get 'item-count data)))
    (should (= 90.0 (fontsloth-otf-var-item-delta
                     store 0 0 (vector 0.5))))
    (should (= 0.0 (fontsloth-otf-var-item-delta
                    store 0 0 (vector 0.0))))
    (should (= 100.0 (fontsloth-otf-var-item-delta
                      store 0 0 (vector 1.0))))))

(ert-deftest fontsloth-otf-var-test-fvar-from-font ()
  "Check that fvar is loaded with the font table."
  (let ((path (fontsloth-test--fixture-font "fontsloth-test-vf.ttf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-otf-load-font-data path))
           (axes (fontsloth-otf-fvar-axes font))
           (instances (fontsloth-otf-fvar-instances font))
           (axis (fontsloth-otf-fvar-axis "wght" font)))
      (should (= 1 (length axes)))
      (should (equal "wght" (alist-get 'tag axis)))
      (should (= 100.0 (alist-get 'min-value axis)))
      (should (= 400.0 (alist-get 'default-value axis)))
      (should (= 900.0 (alist-get 'max-value axis)))
      (should (= 2 (length instances)))
      (should (= 400.0 (aref (alist-get 'coordinates (aref instances 0)) 0)))
      (should (= 700.0 (aref (alist-get 'coordinates (aref instances 1)) 0))))))

(ert-deftest fontsloth-otf-var-test-memoized-types ()
  "Check that inner variation types are memoized by their parameters."
  (should (eq (fontsloth-otf-var--fvar-axis-type 20)
              (fontsloth-otf-var--fvar-axis-type 20)))
  (should-not (eq (fontsloth-otf-var--fvar-axis-type 20)
                  (fontsloth-otf-var--fvar-axis-type 24)))
  (should (eq (fontsloth-otf-var--fvar-instance-type 10 1)
              (fontsloth-otf-var--fvar-instance-type 10 1)))
  (should-not (eq (fontsloth-otf-var--fvar-instance-type 10 1)
                  (fontsloth-otf-var--fvar-instance-type 12 1)))
  (should (eq (fontsloth-otf-var--region-type 1)
              (fontsloth-otf-var--region-type 1)))
  (should (eq (fontsloth-otf-var--delta-type nil 2 0)
              (fontsloth-otf-var--delta-type nil 2 0)))
  (should-not (eq (fontsloth-otf-var--delta-type nil 2 0)
                  (fontsloth-otf-var--delta-type t 2 0))))

(provide 'fontsloth-otf-var-test)


;;; fontsloth-otf-var-test.el ends here
