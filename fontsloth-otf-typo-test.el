;;; fontsloth-otf-typo-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.14.1
;; Package-Requires: ((fontsloth "0.14.1") (emacs "26.1"))
;; Keywords: true-type, font, ttf, otf

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
(require 'fontsloth)


(ert-deftest fontsloth-otf-typo-test-gpos-build-kern-mappings ()
  "Check that the kern index built from GPOS doesn't have any invalid entries.
The checks are done by using the method prescribed by GPOS to lookup
positioning data."
  ;; first load a font with both pair set and class def pair position data
  (fontsloth-load-font "/usr/share/fonts/TTF/IBMPlexSerif-Regular.ttf"
                       :cache 'bypass)
  (let ((mappings (fontsloth-otf-gpos-build-kern-mappings)))
    (cl-loop for k in (map-keys mappings)
             for l = (ash (logand #xffff0000 k) -16)
             for r = (logand #x0000ffff k) do
             (should (= (gethash k mappings)
                        (fontsloth-otf-gpos-hkern l r))))))

(provide 'fontsloth-otf-typo-test)
;;; fontsloth-otf-typo-test.el ends here
