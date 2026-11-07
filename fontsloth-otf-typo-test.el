;;; fontsloth-otf-typo-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

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

;; fontsloth-otf-typo-test.el:
;; Tests for OTF advanced typography loading.
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-test-util)


(ert-deftest fontsloth-otf-typo-test-gpos-build-kern-mappings ()
  "Check that the kern index built from GPOS doesn't have any invalid entries.
The checks are done by using the method prescribed by GPOS to lookup
positioning data."
  ;; first load a font with both pair set and class def pair position data
  (let ((path (fontsloth-test--fixture-font
               "IBMPlexSerif-Regular-subset.otf")))
    (skip-unless (f-exists-p path))
    (fontsloth-load-font path :cache 'bypass))
  (let ((mappings (fontsloth-otf-typo-gpos-build-kern-mappings)))
    (should (> (hash-table-count mappings) 0))
    (cl-loop for k in (map-keys mappings)
             for l = (ash (logand #xffff0000 k) -16)
             for r = (logand #x0000ffff k) do
             (should (= (gethash k mappings)
                        (fontsloth-otf-typo-gpos-hkern l r))))))

(ert-deftest fontsloth-otf-typo-test-gpos-zero-does-not-mask-later-kern ()
  "A zero-valued PairPos match must not mask a later non-zero lookup.
In the fixture, (&, delta) matches a class pair with adjustment 0 in an
earlier lookup and -5 in a later lookup.  The builder and the direct
lookup must both report -5."
  (let ((path (fontsloth-test--fixture-font
               "IBMPlexSerif-Regular-subset.otf")))
    (skip-unless (f-exists-p path))
    (let* ((font (fontsloth-load-font path :cache 'bypass))
           (left (fontsloth-font-glyph-id font ?&))
           (right (fontsloth-font-glyph-id font ?δ))
           (key (logior (ash left 16) right)))
      (should (= (fontsloth-otf-typo-gpos-hkern left right) -5))
      (should (= (gethash key
                          (fontsloth-otf-typo-gpos-build-kern-mappings))
                 -5)))))

(provide 'fontsloth-otf-typo-test)
;;; fontsloth-otf-typo-test.el ends here
