;;; fontsloth-otf-cff.el --- Outlining glyphs using cff -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, glyph, glyf, ttf, otf

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

;; Part of fontsloth

;; fontsloth-otf-cff (this file): Glyph outline implementation that relies on
;; the Adobe OTF CFF 1/2 table

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'map)
(require 'stream)

(require 'fontsloth-bbox)
(require 'fontsloth-otf-)
(require 'fontsloth-otf-cff-dict)

;;; Standard encoding defined in Adobe Technical Note #5176 Appendix B
(defconst fontsloth-otf-cff--standard-encoding
  [0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
   17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
   33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48
   49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
   65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
   81  82  83  84  85  86  87  88  89  90  91  92  93  94  95   0
   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   0  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
   0 111 112 113 114   0 115 116 117 118 119 120 121 122   0 123
   0 124 125 126 127 128 129 130 131   0 132 133   0 134 135 136
   137   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   0 138   0 139   0   0   0   0 140 141 142 143   0   0   0   0
   0 144   0   0   0 145   0   0 146 147 148 149   0   0   0   0]
  "Standard encoding defined in Adobe Technical Note #5176 Appendix B.")

;;; Expert Subset Encoding conversion defined in Adobe Technical Note #5176
;;; Appendix C.

(defconst fontsloth-otf-cff--expert-encoding
  [0    1    229  230  231  232  233  234  235  236  237  238   13   14   15
   99   239  240  241  242  243  244  245  246  247  248   27   28  249  250
   251  252  253  254  255  256  257  258  259  260  261  262  263  264  265
   266  109  110  267  268  269  270  271  272  273  274  275  276  277  278
   279  280  281  282  283  284  285  286  287  288  289  290  291  292  293
   294  295  296  297  298  299  300  301  302  303  304  305  306  307  308
   309  310  311  312  313  314  315  316  317  318  158  155  163  319  320
   321  322  323  324  325  326  150  164  169  327  328  329  330  331  332
   333  334  335  336  337  338  339  340  341  342  343  344  345  346  347
   348  349  350  351  352  353  354  355  356  357  358  359  360  361  362
   363  364  365  366  367  368  369  370  371  372  373  374  375  376  377
   378]
  "Expert Subset Encoding conversion.
Defined in Adobe Technical Note #5176 Appendix C.")

;;; limits according to Adobe Technical Note #5177 Appendix B
(defconst fontsloth-otf-cff-stack-limit 10)
(defconst fontsloth-otf-cff-max-args-stack-len 48)

;;; operator constants Adobe Technical Note #5177
(defconst fontsloth-otf-cff-horizontal-stem 1)
(defconst fontsloth-otf-cff-vertical-stem 3)
(defconst fontsloth-otf-cff-vertical-move-to 4)
(defconst fontsloth-otf-cff-line-to 5)
(defconst fontsloth-otf-cff-horizontal-line-to 6)
(defconst fontsloth-otf-cff-vertical-line-to 7)
(defconst fontsloth-otf-cff-curve-to 8)
(defconst fontsloth-otf-cff-call-local-subr 10)
(defconst fontsloth-otf-cff-return 11)
(defconst fontsloth-otf-cff-endchar 14)
(defconst fontsloth-otf-cff-horizontal-stem-hint-mask 18)
(defconst fontsloth-otf-cff-hint-mask 19)
(defconst fontsloth-otf-cff-counter-mask 20)
(defconst fontsloth-otf-cff-move-to 21)
(defconst fontsloth-otf-cff-horizontal-move-to 22)
(defconst fontsloth-otf-cff-vertical-stem-hint-mask 23)
(defconst fontsloth-otf-cff-curve-line 24)
(defconst fontsloth-otf-cff-line-curve 25)
(defconst fontsloth-otf-cff-vv-curve-to 26)
(defconst fontsloth-otf-cff-hh-curve-to 27)
(defconst fontsloth-otf-cff-short-int 28)
(defconst fontsloth-otf-cff-call-global-subr 29)
(defconst fontsloth-otf-cff-vh-curve-to 30)
(defconst fontsloth-otf-cff-hv-curve-to 31)
(defconst fontsloth-otf-cff-hflex 34)
(defconst fontsloth-otf-cff-flex 35)
(defconst fontsloth-otf-cff-hflex1 36)
(defconst fontsloth-otf-cff-flex1 37)
(defconst fontsloth-otf-cff-fixed-16-16 255)

;;; top dict operators Adobe Technical Note #5176
(defconst fontsloth-otf-cff-top-dict-charset-offset 15)
(defconst fontsloth-otf-cff-top-dict-char-strings-offset 17)
(defconst fontsloth-otf-cff-top-dict-private-dict-size-and-offset 18)
(defconst fontsloth-otf-cff-top-dict-variation-store-offset 24)
(defconst fontsloth-otf-cff-top-dict-ros 1230)
(defconst fontsloth-otf-cff1-top-dict-fd-array 1236)
(defconst fontsloth-otf-cff2-top-dict-font-dict-index-offset 1236)
(defconst fontsloth-otf-cff-top-dict-fd-select 1237)

;;; charsets Adobe Technical Note #5176, Table 22
(defconst fontsloth-otf-cff-charset-iso-adobe 0)
(defconst fontsloth-otf-cff-charset-expert 1)
(defconst fontsloth-otf-cff-charset-expert-subset 2)

;;; Enumerates some operators defined in the Adobe Technical Note #5176, Table
;;; 23 Private DICT Operators
(defconst fontsloth-otf-cff-private-dict-local-subrs-offset 19)

(cl-defun fontsloth-otf-cff-charset-sid->gid (charset sid)
  "Try to find a glyph-id for the given SID in CHARSET."
  (when (= 0 sid) (cl-return 0))
  (pcase charset
    ((or (pred (eq fontsloth-otf-cff-charset-iso-adobe))
         (pred (eq fontsloth-otf-cff-charset-expert))
         (pred (eq fontsloth-otf-cff-charset-expert-subset))) nil)
    ;; TODO: bin search
    (`(1 ,charset) (cl-loop for glyph-id = 1 then (+ glyph-id left 1)
                            for (first . left) in charset
                            for last = (+ first left)
                            if (and (>= sid first) (>= last sid))
                            return (+ glyph-id (- sid first))))))

(defvar fontsloth-otf-cff--index-spec
  (bindat-type
    (index-start unit bindat-idx)
    (count uint 16)
    (offset-size uint 8)
    (offset-array-start unit bindat-idx)
    (offsets-len unit (* (1+ count) offset-size))
    (offsets vec (1+ count) uint (ash offset-size 3)))
  "A CFF index spec.")

(defun fontsloth-otf-cff--stream-uint (s size)
  "Consume an unsigned int of SIZE bytes from S."
  (when (< 0 size)
    `(,(cl-loop for b from (1- size) downto 0 while (<= 0 b)
                   for ash = (ash b 3) then (- ash 8)
                   for part = (stream-pop s)
                   for val = (if (numberp part) (ash part ash)
                               (error "Premature end of stream"))
                   sum val) ,s)))

(defun fontsloth-otf-cff--as-sint (bitlen n)
  "Interpret N as a signed int with BITLEN."
  (let* ((max (ash 1 (1- bitlen)))
         (wrap (+ max max)))
    (if (>= n max)
        (- n wrap)
      n)))

(cl-defun fontsloth-otf-cff--stream-slice
    (seq start &optional (end (length seq)))
  "Stream from SEQ starting at START and ending at END."
  (when (<= start end)
    (stream-cons (when (< start end) (elt seq start))
                 (fontsloth-otf-cff--stream-slice seq (1+ start) end))))

(cl-defmacro fontsloth-otf-cff--make-dict-spec (s &optional rewind)
  "Let S read dictionary data in a lexical context of the dict index.

This assumes that bindat-idx is already positioned at the index start.

REWIND is t if bindat-idx should be positioned back to start"
  `(bindat-type
     (index type fontsloth-otf-cff--index-spec)
     (_ type (pcase-let (((map offset-array-start count
                               offset-size offsets-len offsets) index))
               ,(fontsloth-otf--with-offset-fn
                 '(+ offset-array-start offsets-len) rewind s)))))

(defvar fontsloth-otf-cff--name-dict-spec
  (fontsloth-otf-cff--make-dict-spec
   (bindat-type
     (_ unit (ignore offset-array-start offset-size offsets-len))
     (names vec count str (- (elt offsets (1+ bindat--i))
                             (elt offsets bindat--i)))
     (names-end unit bindat-idx)))
  "A CFF name dict spec.")

(defvar fontsloth-otf-cff--top-dict-spec
  (fontsloth-otf-cff--make-dict-spec
   (bindat-type
     (_ unit (ignore offset-array-start offset-size offsets-len))
     (data vec count type
           (bindat-type vec (- (elt offsets (1+ bindat--i))
                               (elt offsets bindat--i)) uint 8))
     (top-dict-end unit bindat-idx)))
  "A CFF top dict spec.")

(defvar fontsloth-otf-cff--char-strings-spec
  (fontsloth-otf-cff--make-dict-spec
   (bindat-type
     (_ unit (ignore offset-array-start offset-size offsets-len))
     (data vec count type
           (bindat-type repeat (- (elt offsets (1+ bindat--i))
                               (elt offsets bindat--i)) uint 8))
     (char-strings-end unit bindat-idx)) t)
  "CFF char strings spec.")

(defun fontsloth-otf-cff1--parse-top-dict (data)
  "Try to parse a CFF 1 top dict from DATA."
  (let* ((data (elt data 0))
         (max-operands 48)
         (operands-buffer (make-vector max-operands 0))
         (parser (fontsloth-otf-cff-dict-parser-create
                  :data data :operands operands-buffer)))
    (cl-loop for op = (fontsloth-otf-cff-dict-parser-next parser)
             while op collect
             (progn
               (fontsloth:debug* fontsloth-log "Top dict: found op %s" op)
               (pcase op
                 ((pred (eq fontsloth-otf-cff-top-dict-charset-offset))
                  `(charset-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 ((pred (eq fontsloth-otf-cff-top-dict-char-strings-offset))
                  `(char-strings-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 ((pred (eq fontsloth-otf-cff-top-dict-private-dict-size-and-offset))
                  `(private-dict-range
                    . ,(fontsloth-otf-cff-dict-parse-range parser)))
                 ((pred (eq fontsloth-otf-cff-top-dict-ros))
                  '(has-ros . t))
                 ((pred (eq fontsloth-otf-cff1-top-dict-fd-array))
                  '(fd-array-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 ((pred (eq fontsloth-otf-cff-top-dict-fd-select))
                  '(fd-select-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 (_ nil))))))

(defun fontsloth-otf-cff2--parse-top-dict (data)
  "Try to parse a CFF 2 top dict from DATA."
  (let* ((max-operands 513)
         (operands-buffer (make-vector max-operands 0))
         (parser (fontsloth-otf-cff-dict-parser-create
                  :data data :operands operands-buffer)))
    (cl-loop for op = (fontsloth-otf-cff-dict-parser-next parser)
             while op collect
             (progn
               (fontsloth:debug* fontsloth-log "Top dict: found op %s" op)
               (pcase op
                 ((pred (eq fontsloth-otf-cff-top-dict-char-strings-offset))
                  `(char-strings-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 ((pred (eq fontsloth-otf-cff2-top-dict-font-dict-index-offset))
                  `(font-dict-index-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 ((pred (eq fontsloth-otf-cff-top-dict-variation-store-offset))
                  `(font-dict-variation-store-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 (_ nil))))))

(defun fontsloth-otf-cff1--parse-private-dict (data)
  "Parse a CFF 1 private dict from DATA."
  (let* ((max-operands 48)
         (operands-buffer (make-vector max-operands 0))
         (parser (fontsloth-otf-cff-dict-parser-create
                  :data data :operands operands-buffer)))
    (cl-loop for op = (fontsloth-otf-cff-dict-parser-next parser)
             while op collect
             (progn
               (fontsloth:debug* fontsloth-log "Private dict: found op %s" op)
               (pcase op
                 ((pred (eq fontsloth-otf-cff-private-dict-local-subrs-offset))
                  `(local-subrs-offset
                    . ,(fontsloth-otf-cff-dict-parse-offset parser)))
                 (_ nil))))))

(defun fontsloth-otf-cff-parse-charset (num-glyphs data offset)
  "Given NUM-GLYPHS, DATA, and OFFSET, parse a CFF charset."
  (unless (> 2 num-glyphs)
    (let ((format (aref data offset)))
      (cl-case format
        (1 `(1
             ;; TODO: rewrite as a stream
             ,(cl-loop with count = 0
                       with total-left = (1- num-glyphs)
                       for i from (1+ offset) by 3
                       while (< 0 total-left) collect
                       (let ((sid (fontsloth-otf-cff-dict--read-num 2 data i))
                             (left (aref data (+ 2 i))))
                         (setq total-left (- total-left (1+ left)))
                         (cl-incf count)
                         `(,sid . ,left)))))))))

(defvar fontsloth-otf-cff--spec
  (bindat-type
    (table-start unit bindat-idx)
    (major-version uint 8)
    (minor-version uint 8)
    (header-size uint 8)
    (_ type (cl-case major-version
              (1 (bindat-type
                   (abs-offset uint 8)
                   (_ fill (- header-size 4))
                   (name-dict type fontsloth-otf-cff--name-dict-spec)
                   (top-dict-data type fontsloth-otf-cff--top-dict-spec)
                   (top-dict unit (fontsloth-otf-cff1--parse-top-dict
                                   (alist-get 'data top-dict-data)))
                   (strings type fontsloth-otf-cff--name-dict-spec)
                   (global-subrs type fontsloth-otf-cff--char-strings-spec)
                   (char-strings-offset
                    unit
                    (+ table-start
                       (alist-get 'char-strings-offset top-dict)))
                   (char-strings
                    type (fontsloth-otf--with-offset
                          (+ table-start
                             (alist-get 'char-strings-offset top-dict))
                          nil fontsloth-otf-cff--char-strings-spec))
                   (num-glyphs unit (map-nested-elt char-strings '(index count)))
                   (charset
                    unit
                    (pcase (alist-get 'charset-offset top-dict)
                      ((pred (eq fontsloth-otf-cff-charset-iso-adobe))
                       fontsloth-otf-cff-charset-iso-adobe)
                      ((pred (eq fontsloth-otf-cff-charset-expert))
                       fontsloth-otf-cff-charset-expert)
                      ((pred (eq fontsloth-otf-cff-charset-expert-subset))
                       fontsloth-otf-cff-charset-expert-subset)
                      ('nil fontsloth-otf-cff-charset-iso-adobe)
                      (offset
                       (fontsloth-otf-cff-parse-charset
                        num-glyphs bindat-raw (+ table-start offset)))))
                   (private-dict-range
                    unit (seq-map (lambda (i) (+ table-start i))
                                  (alist-get 'private-dict-range top-dict)))
                   (private-dict
                    unit (when private-dict-range
                           (pcase-let* ((`(,start ,end) private-dict-range)
                                        (length (- end start))
                                        (data (make-vector length 0)))
                             (dotimes (i length)
                               (aset data i (aref bindat-raw (+ start i))))
                             (fontsloth-otf-cff1--parse-private-dict data))))
                   (local-subrs-offset
                    unit (when private-dict
                           (+ (car private-dict-range)
                              (alist-get 'local-subrs-offset private-dict))))
                   (local-subrs type
                                (if local-subrs-offset
                                    (fontsloth-otf--with-offset
                                     local-subrs-offset
                                     nil fontsloth-otf-cff--char-strings-spec)
                                  (bindat-type unit nil)))))
              (2 (bindat-type
                   (top-dict-length uint 16)
                   (_ fill (- header-size 5))
                   (top-dict-data vec top-dict-length uint 8)
                   (top-dict unit (fontsloth-otf-cff2--parse-top-dict top-dict-data)))))))
  "A spec for Adobe CFF1/2 tables.")

;;; TODO this type will be used also for cff2
(cl-defstruct
    (fontsloth-otf-cff-builder
     (:constructor fontsloth-otf-cff-builder-create)
     (:copier nil))
  (outliner nil)
  (bbox (fontsloth-bbox-create) :type 'fontsloth-bbox))

(defun fontsloth-otf-cff-builder-move-to (builder x y)
  "Move outline BUILDER to X Y."
  (pcase-let (((cl-struct fontsloth-otf-cff-builder outliner bbox) builder))
    (fontsloth-bbox-extend-by bbox x y)
    (fontsloth-otf-move-to outliner x y)))

(defun fontsloth-otf-cff-builder-line-to (builder x y)
  "Line outline BUILDER at X Y."
  (pcase-let (((cl-struct fontsloth-otf-cff-builder outliner bbox) builder))
    (fontsloth-bbox-extend-by bbox x y)
    (fontsloth-otf-line-to outliner x y)))

(defun fontsloth-otf-cff-builder-curve-to (builder x1 y1 x2 y2 x y)
  "Curve outline BUILDER at X Y with controls X1 Y1 and X2 Y2."
  (pcase-let (((cl-struct fontsloth-otf-cff-builder outliner bbox) builder))
    (fontsloth-bbox-extend-by bbox x1 y1)
    (fontsloth-bbox-extend-by bbox x2 y2)
    (fontsloth-bbox-extend-by bbox x y)
    (fontsloth-otf-curve-to outliner x1 y1 x2 y2 x y)))

(defun fontsloth-otf-cff-builder-close (builder)
  "Close outline BUILDER contour."
  (pcase-let (((cl-struct fontsloth-otf-cff-builder outliner) builder))
    (fontsloth-otf-close-contour outliner)))

(cl-defstruct
    (fontsloth-otf-cff-parse-context
     (:constructor fontsloth-otf-cff-parse-context-create))
  (metadata nil)
  (width-parsed nil :type 'boolean)
  (stems-len 0 :type 'natnum)
  (has-endchar nil :type 'boolean)
  (has-seac nil :type 'boolean)
  (glyph-id 0 :type 'natnum)
  (local-subrs nil))

(cl-defstruct (fontsloth-otf-cff-arguments-stack
               (:constructor fontsloth-otf-cff-arguments-stack-create))
  (data nil :type 'vector)
  (len 0 :type 'natnum)
  (max-len 0 :type 'natnum))

(defsubst fontsloth-otf-cff-arguments-stack-empty-p (s)
  "Return t if S is empty."
  (= 0 (fontsloth-otf-cff-arguments-stack-len s)))

(defun fontsloth-otf-cff-arguments-stack-push (s n)
  "Push N onto S."
  (when (= (fontsloth-otf-cff-arguments-stack-len s)
           (fontsloth-otf-cff-arguments-stack-max-len s))
    (error "Argument stack limit reached"))
  (cl-incf (fontsloth-otf-cff-arguments-stack-len s))
  (push n (fontsloth-otf-cff-arguments-stack-data s)))

(defun fontsloth-otf-cff-arguments-stack-pop (s)
  "Pop S."
  (cl-assert (not (fontsloth-otf-cff-arguments-stack-empty-p s)))
  (cl-decf (fontsloth-otf-cff-arguments-stack-len s))
  (pop (fontsloth-otf-cff-arguments-stack-data s)))

(defsubst fontsloth-otf-cff-arguments-stack-reverse (s)
  "Reverse this stack S."
  (unless (fontsloth-otf-cff-arguments-stack-empty-p s)
    (setf (fontsloth-otf-cff-arguments-stack-data s)
          (nreverse (fontsloth-otf-cff-arguments-stack-data s)))))

(defsubst fontsloth-otf-cff-arguments-stack-clear (s)
  "Clear this stack S."
  (setf (fontsloth-otf-cff-arguments-stack-data s) nil
        (fontsloth-otf-cff-arguments-stack-len s) 0))

(cl-defstruct (fontsloth-otf-cff-char-string-parser
               (:constructor fontsloth-otf-cff-char-string-parser-create))
  (stack nil :type 'fontsloth-otf-cff-arguments-stack)
  (builder nil :type 'fontsloth-otf-cff-builder)
  (x 0.0 :type 'float)
  (y 0.0 :type 'float)
  (has-move-to nil :type 'boolean)
  (1st-move-to t :type 'boolean))

(defsubst fontsloth-otf-cff--calc-subtroutine-bias (len)
  "Calculate the subroutine bias given LEN."
  (cond ((> 1240 len) 107)
        ((> 33900 len) 1131)
        (t 32768)))

(defsubst fontsloth-otf-cff--conv-subr-index (index bias)
  "Given subroutine BIAS convert subroutine INDEX."
  (truncate (+ index bias)))

(defun fontsloth-otf-cff--parse-move-to (p offset)
  "Parse postscript move-to with parser P in stack at OFFSET."
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (/= (+ 2 offset) (stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (cl-loop repeat offset do (stack-pop s))
      (if (fontsloth-otf-cff-char-string-parser-1st-move-to p)
          (setf (fontsloth-otf-cff-char-string-parser-1st-move-to p) nil)
        (fontsloth-otf-cff-builder-close builder))
      (setf (fontsloth-otf-cff-char-string-parser-has-move-to p) t)
      (setf (fontsloth-otf-cff-char-string-parser-x p)
            (+ (parser-x p) (stack-pop s))
            (fontsloth-otf-cff-char-string-parser-y p)
            (+ (parser-y p) (stack-pop s)))
      (fontsloth-otf-cff-builder-move-to builder (parser-x p) (parser-y p)))))

(defun fontsloth-otf-cff--parse-horizontal-move-to (p offset)
  "Parse postscript horizontal-move-to with parser P in stack at OFFSET."
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (/= (1+ offset) (stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (cl-loop repeat offset do (stack-pop s))
      (if (fontsloth-otf-cff-char-string-parser-1st-move-to p)
          (setf (fontsloth-otf-cff-char-string-parser-1st-move-to p) nil)
        (fontsloth-otf-cff-builder-close builder))
      (setf (fontsloth-otf-cff-char-string-parser-has-move-to p) t)
      (setf (fontsloth-otf-cff-char-string-parser-x p)
            (+ (parser-x p) (stack-pop s)))
      (fontsloth-otf-cff-builder-move-to builder (parser-x p) (parser-y p)))))

(defun fontsloth-otf-cff--parse-vertical-move-to (p offset)
  "Parse postscript vertical-move-to with parser P in stack at OFFSET."
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (/= (1+ offset) (stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (cl-loop repeat offset do (stack-pop s))
      (if (fontsloth-otf-cff-char-string-parser-1st-move-to p)
          (setf (fontsloth-otf-cff-char-string-parser-1st-move-to p) nil)
        (fontsloth-otf-cff-builder-close builder))
      (setf (fontsloth-otf-cff-char-string-parser-has-move-to p) t)
      (setf (fontsloth-otf-cff-char-string-parser-y p)
            (+ (parser-y p) (stack-pop s)))
      (fontsloth-otf-cff-builder-move-to builder (parser-x p) (parser-y p)))))

(defun fontsloth-otf-cff--parse-line-to (p)
  "Parse postscript line-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (cl-oddp (stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (not (stack-empty-p s))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ (parser-x p) (stack-pop s))
              (fontsloth-otf-cff-char-string-parser-y p)
              (+ (parser-y p) (stack-pop s)))
        (fontsloth-otf-cff-builder-line-to
         builder (parser-x p) (parser-y p))))))

(cl-defun fontsloth-otf-cff--parse-horizontal-line-to (p)
  "Parse postscript horizontal-line-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (stack-empty-p s)
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (not (stack-empty-p s))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ (parser-x p) (stack-pop s)))
        (fontsloth-otf-cff-builder-line-to
         builder (parser-x p) (parser-y p))
        (when (stack-empty-p s)
          (cl-return-from fontsloth-otf-cff--parse-horizontal-line-to))
        (setf (fontsloth-otf-cff-char-string-parser-y p)
              (+ (parser-y p) (stack-pop s)))
        (fontsloth-otf-cff-builder-line-to
         builder (parser-x p) (parser-y p))))))

(cl-defun fontsloth-otf-cff--parse-vertical-line-to (p)
  "Parse postscript vertical-line-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (stack-empty-p s)
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (not (stack-empty-p s))
        (setf (fontsloth-otf-cff-char-string-parser-y p)
              (+ (parser-y p) (stack-pop s)))
        (fontsloth-otf-cff-builder-line-to
         builder (parser-x p) (parser-y p))
        (when (stack-empty-p s)
          (cl-return-from fontsloth-otf-cff--parse-vertical-line-to))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ (parser-x p) (stack-pop s)))
        (fontsloth-otf-cff-builder-line-to
         builder (parser-x p) (parser-y p))))))

(defun fontsloth-otf-cff--parse-curve-to (p)
  "Parse postscript curve-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (unless (= 0 (mod (stack-len s) 6))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (not (stack-empty-p s))
        (let* ((x1 (+ (parser-x p) (stack-pop s)))
               (y1 (+ (parser-y p) (stack-pop s)))
               (x2 (+ x1 (stack-pop s)))
               (y2 (+ y1 (stack-pop s))))
          (setf (fontsloth-otf-cff-char-string-parser-x p)
                (+ x2 (stack-pop s))
                (fontsloth-otf-cff-char-string-parser-y p)
                (+ y2 (stack-pop s)))
          (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p)))))))

(defun fontsloth-otf-cff--parse-curve-line (p)
  "Parse postscript curve-line with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (or (> 8 (stack-len s)) (/= 0 (mod (- (stack-len s) 2) 6)))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (> (stack-len s) 2)
        (let* ((x1 (+ (parser-x p) (stack-pop s)))
               (y1 (+ (parser-y p) (stack-pop s)))
               (x2 (+ x1 (stack-pop s)))
               (y2 (+ y1 (stack-pop s))))
          (setf (fontsloth-otf-cff-char-string-parser-x p)
                (+ x2 (stack-pop s))
                (fontsloth-otf-cff-char-string-parser-y p)
                (+ y2 (stack-pop s)))
          (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p))))
      (setf (fontsloth-otf-cff-char-string-parser-x p)
            (+ (parser-x p) (stack-pop s))
            (fontsloth-otf-cff-char-string-parser-y p)
            (+ (parser-y p) (stack-pop s)))
      (fontsloth-otf-cff-builder-line-to builder (parser-x p) (parser-y p))
      (cl-assert (stack-empty-p s)))))

(defun fontsloth-otf-cff--parse-line-curve (p)
  "Parse postscript line-curve with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (when (or (> 8 (stack-len s)) (cl-oddp (- (stack-len s) 6)))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (> (stack-len s) 6)
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ (parser-x p) (stack-pop s))
              (fontsloth-otf-cff-char-string-parser-y p)
              (+ (parser-y p) (stack-pop s)))
        (fontsloth-otf-cff-builder-line-to builder (parser-x p) (parser-y p)))
      (let* ((x1 (+ (parser-x p) (stack-pop s)))
             (y1 (+ (parser-y p) (stack-pop s)))
             (x2 (+ x1 (stack-pop s)))
             (y2 (+ y1 (stack-pop s))))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ x2 (stack-pop s))
              (fontsloth-otf-cff-char-string-parser-y p)
              (+ y2 (stack-pop s)))
        (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p)))
      (cl-assert (stack-empty-p s)))))

(cl-defun fontsloth-otf-cff--parse-hh-curve-to (p)
  "Parse postscript hh-curve-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (when (cl-oddp (stack-len s))
        (setf (fontsloth-otf-cff-char-string-parser-y p)
              (+ (parser-y p) (stack-pop s))))
      (unless (= 0 (mod (stack-len s) 4))
        (error "Invalid arguments stack length %s" p))
      (while (not (stack-empty-p s))
        (let* ((x1 (+ (parser-x p) (stack-pop s)))
               (y1 (parser-y p))
               (x2 (+ x1 (stack-pop s)))
               (y2 (+ y1 (stack-pop s))))
          (setf (fontsloth-otf-cff-char-string-parser-x p)
                (+ x2 (stack-pop s))
                (fontsloth-otf-cff-char-string-parser-y p) y2)
          (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p))))
      (cl-assert (stack-empty-p s)))))

(cl-defun fontsloth-otf-cff--parse-vv-curve-to (p)
  "Parse postscript vv-curve-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (when (cl-oddp (stack-len s))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ (parser-x p) (stack-pop s))))
      (unless (= 0 (mod (stack-len s) 4))
        (error "Invalid arguments stack length %s" p))
      (while (not (stack-empty-p s))
        (let* ((x1 (parser-x p))
               (y1 (+ (parser-y p) (stack-pop s)))
               (x2 (+ x1 (stack-pop s)))
               (y2 (+ y1 (stack-pop s))))
          (setf (fontsloth-otf-cff-char-string-parser-x p) x2
                (fontsloth-otf-cff-char-string-parser-y p)
                (+ y2 (stack-pop s)))
          (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p))))
      (cl-assert (stack-empty-p s)))))

(cl-defun fontsloth-otf-cff--parse-hv-curve-to (p)
  "Parse postscript hv-curve-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (not (stack-empty-p s))
        (when (> 4 (stack-len s))
          (error "Invalid arguments stack length %s" p))
        (let* ((x1 (+ (parser-x p) (stack-pop s)))
               (y1 (parser-y p))
               (x2 (+ x1 (stack-pop s)))
               (y2 (+ y1 (stack-pop s))))
          (setf (fontsloth-otf-cff-char-string-parser-y p)
                (+ y2 (stack-pop s))
                (fontsloth-otf-cff-char-string-parser-x p)
                (+ x2 (if (= 1 (stack-len s)) (stack-pop s) 0.0)))
          (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p))
          (when (stack-empty-p s)
            (cl-return-from fontsloth-otf-cff--parse-hv-curve-to))
          (when (> 4 (stack-len s))
            (error "Invalid arguments stack length %s" p))
          (let* ((x1 (parser-x p))
                 (y1 (+ (parser-y p) (stack-pop s)))
                 (x2 (+ x1 (stack-pop s)))
                 (y2 (+ y1 (stack-pop s))))
            (setf (fontsloth-otf-cff-char-string-parser-x p)
                  (+ x2 (stack-pop s))
                  (fontsloth-otf-cff-char-string-parser-y p)
                  (+ y2 (if (= 1 (stack-len s)) (stack-pop s) 0.0)))
            (fontsloth-otf-cff-builder-curve-to
             builder x1 y1 x2 y2 (parser-x p) (parser-y p)))))
      (cl-assert (stack-empty-p s)))))

(cl-defun fontsloth-otf-cff--parse-vh-curve-to (p)
  "Parse postscript vh-curve-to with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (stack-len #'fontsloth-otf-cff-arguments-stack-len)
            (stack-empty-p #'fontsloth-otf-cff-arguments-stack-empty-p)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (while (not (stack-empty-p s))
        (when (> 4 (stack-len s))
          (error "Invalid arguments stack length %s" p))
        (let* ((x1 (parser-x p))
               (y1 (+ (parser-y p) (stack-pop s)))
               (x2 (+ x1 (stack-pop s)))
               (y2 (+ y1 (stack-pop s))))
          (setf (fontsloth-otf-cff-char-string-parser-x p)
                (+ x2 (stack-pop s))
                (fontsloth-otf-cff-char-string-parser-y p)
                (+ y2 (if (= 1 (stack-len s)) (stack-pop s) 0.0)))
          (fontsloth-otf-cff-builder-curve-to
           builder x1 y1 x2 y2 (parser-x p) (parser-y p))
          (when (stack-empty-p s)
            (cl-return-from fontsloth-otf-cff--parse-vh-curve-to))
          (when (> 4 (stack-len s))
            (error "Invalid arguments stack length %s" p))
          (let* ((x1 (+ (parser-x p) (stack-pop s)))
                 (y1 (parser-y p))
                 (x2 (+ x1 (stack-pop s)))
                 (y2 (+ y1 (stack-pop s))))
            (setf (fontsloth-otf-cff-char-string-parser-y p)
                  (+ y2 (stack-pop s))
                  (fontsloth-otf-cff-char-string-parser-x p)
                  (+ x2 (if (= 1 (stack-len s)) (stack-pop s) 0.0)))
            (fontsloth-otf-cff-builder-curve-to
             builder x1 y1 x2 y2 (parser-x p) (parser-y p)))))
      (cl-assert (stack-empty-p s)))))

(defun fontsloth-otf-cff--parse-flex (p)
  "Parse postscript flex with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (unless (= 13 (fontsloth-otf-cff-arguments-stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (let* ((dx1 (+ (parser-x p) (stack-pop s)))
             (dy1 (+ (parser-y p) (stack-pop s)))
             (dx2 (+ dx1 (stack-pop s)))
             (dy2 (+ dy1 (stack-pop s)))
             (dx3 (+ dx2 (stack-pop s)))
             (dy3 (+ dy2 (stack-pop s)))
             (dx4 (+ dx3 (stack-pop s)))
             (dy4 (+ dy3 (stack-pop s)))
             (dx5 (+ dx4 (stack-pop s)))
             (dy5 (+ dy4 (stack-pop s))))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ dx5 (stack-pop s))
              (fontsloth-otf-cff-char-string-parser-y p)
              (+ dy5 (stack-pop s)))
        (fontsloth-otf-cff-builder-curve-to builder dx1 dy1 dx2 dy2 dx3 dy3)
        (fontsloth-otf-cff-builder-curve-to
         builder dx4 dy4 dx5 dy5 (parser-x p) (parser-y p))))))

(defun fontsloth-otf-cff--parse-flex1 (p)
  "Parse postscript flex1 with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (unless (= 11 (fontsloth-otf-cff-arguments-stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (let* ((dx1 (+ (parser-x p) (stack-pop s)))
             (dy1 (+ (parser-y p) (stack-pop s)))
             (dx2 (+ dx1 (stack-pop s)))
             (dy2 (+ dy1 (stack-pop s)))
             (dx3 (+ dx2 (stack-pop s)))
             (dy3 (+ dy2 (stack-pop s)))
             (dx4 (+ dx3 (stack-pop s)))
             (dy4 (+ dy3 (stack-pop s)))
             (dx5 (+ dx4 (stack-pop s)))
             (dy5 (+ dy4 (stack-pop s))))
        (if (> (abs (- dx5 (parser-x p)))
               (abs (- dy5 (parser-y p))))
            (setf (fontsloth-otf-cff-char-string-parser-x p)
                  (+ dx5 (stack-pop s)))
          (setf (fontsloth-otf-cff-char-string-parser-y p)
                (+ dy5 (stack-pop s))))
        (fontsloth-otf-cff-builder-curve-to builder dx1 dy1 dx2 dy2 dx3 dy3)
        (fontsloth-otf-cff-builder-curve-to
         builder dx4 dy4 dx5 dy5 (parser-x p) (parser-y p))))))

(defun fontsloth-otf-cff--parse-hflex (p)
  "Parse postscript hflex with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (unless (= 7 (fontsloth-otf-cff-arguments-stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (let* ((dx1 (+ (parser-x p) (stack-pop s)))
             (dy1 (parser-y p))
             (dx2 (+ dx1 (stack-pop s)))
             (dy2 (+ dy1 (stack-pop s)))
             (dx3 (+ dx2 (stack-pop s)))
             (dy3 dy2)
             (dx4 (+ dx3 (stack-pop s)))
             (dy4 dy2)
             (dx5 (+ dx4 (stack-pop s)))
             (dy5 (parser-y p)))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ dx5 (stack-pop s)))
        (fontsloth-otf-cff-builder-curve-to builder dx1 dy1 dx2 dy2 dx3 dy3)
        (fontsloth-otf-cff-builder-curve-to
         builder dx4 dy4 dx5 dy5 (parser-x p) (parser-y p))))))

(defun fontsloth-otf-cff--parse-hflex1 (p)
  "Parse postscript hflex1 with parser P."
  (unless (fontsloth-otf-cff-char-string-parser-has-move-to p)
    (error "Missing move-to %s" p))
  (cl-flet ((stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (parser-x #'fontsloth-otf-cff-char-string-parser-x)
            (parser-y #'fontsloth-otf-cff-char-string-parser-y))
    (let ((s (fontsloth-otf-cff-char-string-parser-stack p))
          (builder (fontsloth-otf-cff-char-string-parser-builder p)))
      (unless (= 9 (fontsloth-otf-cff-arguments-stack-len s))
        (error "Invalid arguments stack length %s" p))
      (fontsloth-otf-cff-arguments-stack-reverse s)
      (let* ((dx1 (+ (parser-x p) (stack-pop s)))
             (dy1 (+ (parser-y p) (stack-pop s)))
             (dx2 (+ dx1 (stack-pop s)))
             (dy2 (+ dy1 (stack-pop s)))
             (dx3 (+ dx2 (stack-pop s)))
             (dy3 dy2)
             (dx4 (+ dx3 (stack-pop s)))
             (dy4 dy2)
             (dx5 (+ dx4 (stack-pop s)))
             (dy5 (+ dy4 (stack-pop s))))
        (setf (fontsloth-otf-cff-char-string-parser-x p)
              (+ dx5 (stack-pop s)))
        (fontsloth-otf-cff-builder-curve-to builder dx1 dy1 dx2 dy2 dx3 dy3)
        (fontsloth-otf-cff-builder-curve-to
         builder dx4 dy4 dx5 dy5 (parser-x p) (parser-y p))))))

(defsubst fontsloth-otf-cff--parse-int1 (p op)
  "Parse postscript int1 OP with parser P."
  (let* ((n (- op 139)))
    (fontsloth:debug* fontsloth-log "Pushing int1 %s" n)
    (fontsloth-otf-cff-arguments-stack-push
     (fontsloth-otf-cff-char-string-parser-stack p)
     (* 1.0 n))))

(defsubst fontsloth-otf-cff--parse-int2 (p op s)
  "Parse postscript int2 OP with parser P and stream S."
  (let* ((b1 (stream-pop s))
         (n (+ (* 256 (- op 247)) b1 108)))
    (cl-assert (and (>= 1131 n) (<= 108 n)))
    (fontsloth:debug* fontsloth-log "Pushing int2 %s" n)
    (fontsloth-otf-cff-arguments-stack-push
     (fontsloth-otf-cff-char-string-parser-stack p)
     (* 1.0 n))
    s))

(defsubst fontsloth-otf-cff--parse-int3 (p op s)
  "Parse postscript int3 OP with parser P and stream S."
  (let* ((b1 (stream-pop s))
         (n (- (* -256 (- op 251)) b1 108)))
    (cl-assert (and (<= -1131 n) (>= -108 n)))
    (fontsloth:debug* fontsloth-log "Pushing int3 %s" n)
    (fontsloth-otf-cff-arguments-stack-push
     (fontsloth-otf-cff-char-string-parser-stack p)
     (* 1.0 n))
    s))

(defun fontsloth-otf-cff--parse-fixed (p s)
  "Parse postscript fixed with parser P and stream S."
  (pcase-let* ((`(,v ,s) (fontsloth-otf-cff--stream-uint s 4))
               (n (+ (/ v (ash 1 16))
                     (/ (* 1.0 (logand v #xffff)) #x10000))))
    (fontsloth:debug* fontsloth-log "Pushing fixed %s" n)
    (fontsloth-otf-cff-arguments-stack-push
     (fontsloth-otf-cff-char-string-parser-stack p) n)
    s))

(defun fontsloth-otf-cff-seac->glyphid (charset n)
  "Try to convert an encoded string id N to a glyph id in CHARSET."
  (let* ((sid (aref fontsloth-otf-cff--standard-encoding n)))
    (pcase charset
      ((pred (eq fontsloth-otf-cff-charset-iso-adobe)) (when (>= 228 n) sid))
      ((or (pred (eq fontsloth-otf-cff-charset-expert))
           (pred (eq fontsloth-otf-cff-charset-expert-subset))) nil)
      (_ (fontsloth-otf-cff-charset-sid->gid charset sid)))))

(defun fontsloth-otf-cff--char-string-endchar (ctx depth p)
  "Parse a postscript endchar with context CTX at DEPTH with parser P."
  (cl-flet ((seac->glyphid #'fontsloth-otf-cff-seac->glyphid)
            (stack-pop #'fontsloth-otf-cff-arguments-stack-pop)
            (-parse-char-string #'fontsloth-otf-cff--parse-char-string))
    (let* ((s (fontsloth-otf-cff-char-string-parser-stack p))
           (s-len (fontsloth-otf-cff-arguments-stack-len s))
           (width-parsed (fontsloth-otf-cff-parse-context-width-parsed ctx))
           (metadata (fontsloth-otf-cff-parse-context-metadata ctx))
           (char-strings (alist-get 'char-strings metadata))
           (charset (alist-get 'charset metadata)))
      (if (or (= 4 s-len) (and (not width-parsed) (= 5 s-len)))
          (let ((accent-char (seac->glyphid charset (stack-pop s)))
                (base-char (seac->glyphid charset (stack-pop s)))
                (dy (stack-pop s))
                (dx (stack-pop s)))
            (fontsloth:debug*
             fontsloth-log "Endchar with base and accent char")
            (unless width-parsed
              (stack-pop s)
              (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t))
            (setf (fontsloth-otf-cff-parse-context-has-seac ctx) t)
            (let ((base-char-string (stream (elt char-strings base-char))))
              (-parse-char-string ctx base-char-string (1+ depth) p)
              (setf (fontsloth-otf-cff-char-string-parser-x p) dx
                    (fontsloth-otf-cff-char-string-parser-y p) dy))
            (let ((accent-char-string
                   (stream (elt char-strings accent-char))))
              (-parse-char-string ctx accent-char-string (1+ depth) p)))
        (when (and (= 1 s-len) (not width-parsed))
          (stack-pop s)
          (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t)))
      (unless (fontsloth-otf-cff-char-string-parser-1st-move-to p)
        (setf (fontsloth-otf-cff-char-string-parser-1st-move-to p) t)
        (fontsloth-otf-cff-builder-close
         (fontsloth-otf-cff-char-string-parser-builder p)))
      (setf (fontsloth-otf-cff-parse-context-has-endchar ctx) t))))

(defun fontsloth-otf-cff--parse-char-string (ctx char-string depth p)
  "Parse a postscript CHAR-STRING in CTX at DEPTH with parser P."
  (fontsloth:debug* fontsloth-log "Parse-char-string, depth %s" depth)
  (cl-loop
   until (stream-empty-p char-string)
   for op = (stream-pop char-string)
   with s = (fontsloth-otf-cff-char-string-parser-stack p)
   with metadata = (fontsloth-otf-cff-parse-context-metadata ctx)
   with width-parsed = (fontsloth-otf-cff-parse-context-width-parsed ctx) do
   (cl-block loop
     (fontsloth:debug* fontsloth-log "Checking op %s" op)
     (pcase op
       ((or 0 2 9 13 15 16 17) (error "Invalid operator %s" op))
       ((or (pred (eq fontsloth-otf-cff-horizontal-stem))
            (pred (eq fontsloth-otf-cff-vertical-stem))
            (pred (eq fontsloth-otf-cff-horizontal-stem-hint-mask))
            (pred (eq fontsloth-otf-cff-vertical-stem-hint-mask)))
        (fontsloth:debug* fontsloth-log "H/v-stem/hint-mask %s" op)
        (let* ((s-len (fontsloth-otf-cff-arguments-stack-len s))
               (len
                (if (and (cl-oddp s-len) (not width-parsed))
                    (progn
                      (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t)
                      (1- s-len))
                  s-len)))
          (setf (fontsloth-otf-cff-parse-context-stems-len ctx)
                (+ (ash len -1) (fontsloth-otf-cff-parse-context-stems-len ctx)))
          (fontsloth-otf-cff-arguments-stack-clear s)))
       ((pred (eq fontsloth-otf-cff-vertical-move-to))
        (fontsloth:debug* fontsloth-log "Vertical-move-to %s" op)
        (let ((s-len (fontsloth-otf-cff-arguments-stack-len s))
              (i 0))
          (when (and (= 2 s-len) (not width-parsed))
            (progn (setq i (1+ i))
                   (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t)))
          (fontsloth-otf-cff--parse-vertical-move-to p i)))
       ((pred (eq fontsloth-otf-cff-line-to))
        (fontsloth:debug* fontsloth-log "Line-to %s" op)
        (fontsloth-otf-cff--parse-line-to p))
       ((pred (eq fontsloth-otf-cff-horizontal-line-to))
        (fontsloth:debug* fontsloth-log "Horizontal-line-to %s" op)
        (fontsloth-otf-cff--parse-horizontal-line-to p))
       ((pred (eq fontsloth-otf-cff-vertical-line-to))
        (fontsloth:debug* fontsloth-log "Vertical-line-to %s" op)
        (fontsloth-otf-cff--parse-vertical-line-to p))
       ((pred (eq fontsloth-otf-cff-curve-to))
        (fontsloth:debug* fontsloth-log "Curve-to %s" op)
        (fontsloth-otf-cff--parse-curve-to p))
       ((pred (eq fontsloth-otf-cff-call-local-subr))
        (fontsloth:debug* fontsloth-log "Local-subr %s" op)
        (when (fontsloth-otf-cff-arguments-stack-empty-p s)
          (error "Invalid arguments stack length"))
        (when (= depth fontsloth-otf-cff-stack-limit)
          (error "Nesting limit reached"))
        ;; (unless (fontsloth-otf-cff-parse-context-local-subrs ctx)
        ;;   ;; TODO: parse cid subrs
        ;;   )
        (if-let* ((lsubrs
                     (fontsloth-otf-cff-parse-context-local-subrs
                      ctx))
                    (bias (fontsloth-otf-cff--calc-subtroutine-bias
                           (length lsubrs)))
                    (index (fontsloth-otf-cff--conv-subr-index
                            (fontsloth-otf-cff-arguments-stack-pop s) bias))
                    (char-string (stream (aref lsubrs index))))
            (fontsloth-otf-cff--parse-char-string ctx char-string (1+ depth) p)
          (error "No local subroutines"))
        (when (and (fontsloth-otf-cff-parse-context-has-endchar
                    ctx)
                   (not (fontsloth-otf-cff-parse-context-has-seac
                         ctx)))
          (unless (stream-empty-p char-string)
            (error "Data after endchar"))
          (cl-return-from loop)))
       ((pred (eq fontsloth-otf-cff-return))
        (fontsloth:debug* fontsloth-log "Return %s" op)
        (cl-return-from loop))
       ((pred (eq fontsloth-otf-cff-dict-parser-two-byte-op-mark))
        (fontsloth:debug* fontsloth-log "Two-byte op %s" op)
        (let ((op2 (stream-pop char-string)))
          (pcase op2
            ((pred (eq fontsloth-otf-cff-hflex))
             (fontsloth:debug* fontsloth-log "Hflex %s %s" op op2)
             (fontsloth-otf-cff--parse-hflex p))
            ((pred (eq fontsloth-otf-cff-flex))
             (fontsloth:debug* fontsloth-log "Flex %s %s" op op2)
             (fontsloth-otf-cff--parse-flex p))
            ((pred (eq fontsloth-otf-cff-hflex1))
             (fontsloth:debug* fontsloth-log "Hflex1 %s %s" op op2)
             (fontsloth-otf-cff--parse-hflex1 p))
            ((pred (eq fontsloth-otf-cff-flex1))
             (fontsloth:debug* fontsloth-log "Flex1 %s %s" op op2)
             (fontsloth-otf-cff--parse-flex1 p))
            (_ (error "Unsupported two byte operator %s %s" op op2)))))
       ((pred (eq fontsloth-otf-cff-endchar))
        (fontsloth:debug* fontsloth-log "End-char %s" op)
        (fontsloth-otf-cff--char-string-endchar ctx depth p)
        (unless (stream-empty-p char-string)
          (error "Data after endchar")))
       ((or (pred (eq fontsloth-otf-cff-hint-mask))
            (pred (eq fontsloth-otf-cff-counter-mask)))
        (fontsloth:debug* fontsloth-log "Hint/counter-mask %s" op)
        (let ((s-len (fontsloth-otf-cff-arguments-stack-len s))
              (stem-len (fontsloth-otf-cff-parse-context-stems-len ctx)))
          ;; currently ignore hint ops
          (fontsloth-otf-cff-arguments-stack-clear s)
          (when (and (cl-oddp s-len) (not width-parsed))
            (cl-decf s-len)
            (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t))
          (setf (fontsloth-otf-cff-parse-context-stems-len ctx)
                (+ stem-len (ash s-len -1)))
          (let* ((stem-len
                  (fontsloth-otf-cff-parse-context-stems-len ctx))
                 (s-advance (ash (+ 7 stem-len) -3)))
            (fontsloth:debug* fontsloth-log "Hint/counter-mask s-advance %s"
                              s-advance)
            (cl-loop repeat s-advance do (stream-pop char-string)))))
       ((pred (eq fontsloth-otf-cff-move-to))
        (fontsloth:debug* fontsloth-log "Move-to %s" op)
        (let ((i 0)
              (s-len (fontsloth-otf-cff-arguments-stack-len s)))
          (when (and (= 3 s-len) (not width-parsed))
            (cl-incf i)
            (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t))
          (fontsloth-otf-cff--parse-move-to p i)))
       ((pred (eq fontsloth-otf-cff-horizontal-move-to))
        (fontsloth:debug* fontsloth-log "Horizontal-move-to %s" op)
        (let ((i 0)
              (s-len (fontsloth-otf-cff-arguments-stack-len s)))
          (when (and (= 2 s-len) (not width-parsed))
            (cl-incf i)
            (setf (fontsloth-otf-cff-parse-context-width-parsed ctx) t))
          (fontsloth-otf-cff--parse-horizontal-move-to p i)))
       ((pred (eq fontsloth-otf-cff-curve-line))
        (fontsloth:debug* fontsloth-log "Curve-line %s" op)
        (fontsloth-otf-cff--parse-curve-line p))
       ((pred (eq fontsloth-otf-cff-line-curve))
        (fontsloth:debug* fontsloth-log "Line-curve %s" op)
        (fontsloth-otf-cff--parse-line-curve p))
       ((pred (eq fontsloth-otf-cff-vv-curve-to))
        (fontsloth:debug* fontsloth-log "Vv-curve-to %s" op)
        (fontsloth-otf-cff--parse-vv-curve-to p))
       ((pred (eq fontsloth-otf-cff-hh-curve-to))
        (fontsloth:debug* fontsloth-log "Hh-curve-to %s" op)
        (fontsloth-otf-cff--parse-hh-curve-to p))
       ((pred (eq fontsloth-otf-cff-short-int))
        (fontsloth:debug* fontsloth-log "Short-int %s" op)
        (pcase-let* ((`(,v ,st) (fontsloth-otf-cff--stream-uint char-string 2))
                     (n (fontsloth-otf-cff--as-sint 16 v)))
          (setq char-string st)
          (fontsloth:debug* fontsloth-log "Pushing short int %s" n)
          (fontsloth-otf-cff-arguments-stack-push
           (fontsloth-otf-cff-char-string-parser-stack p) (* 1.0 n))))
       ((pred (eq fontsloth-otf-cff-call-global-subr))
        (fontsloth:debug* fontsloth-log "Global-subr %s" op)
        (when (fontsloth-otf-cff-arguments-stack-empty-p s)
          (error "Invalid arguments stack length"))
        (when (= depth fontsloth-otf-cff-stack-limit)
          (error "Nesting limit reached"))
        (let* ((global-subrs (alist-get 'global-subrs metadata))
               (subr-bias (fontsloth-otf-cff--calc-subtroutine-bias
                           (map-nested-elt global-subrs '(index count))))
               (index (fontsloth-otf-cff--conv-subr-index
                       (fontsloth-otf-cff-arguments-stack-pop s) subr-bias))
               (char-string (stream (aref (alist-get 'data global-subrs) index))))
          (fontsloth-otf-cff--parse-char-string ctx char-string (1+ depth) p))
        (when (and (fontsloth-otf-cff-parse-context-has-endchar
                    ctx)
                   (not (fontsloth-otf-cff-parse-context-has-seac
                         ctx)))
          (unless (stream-empty-p char-string)
            (error "Data after endchar"))
          (cl-return-from loop)))
       ((pred (eq fontsloth-otf-cff-vh-curve-to))
        (fontsloth:debug* fontsloth-log "Vh-curve-to %s" op)
        (fontsloth-otf-cff--parse-vh-curve-to p))
       ((pred (eq fontsloth-otf-cff-hv-curve-to))
        (fontsloth:debug* fontsloth-log "Hv-curve-to %s" op)
        (fontsloth-otf-cff--parse-hv-curve-to p))
       ((and (pred (<= 32)) (pred (>= 246)))
        (fontsloth:debug* fontsloth-log "Int1 %s" op)
        (fontsloth-otf-cff--parse-int1 p op))
       ((and (pred (<= 247)) (pred (>= 250)))
        (fontsloth:debug* fontsloth-log "Int2 %s" op)
        (setq char-string (fontsloth-otf-cff--parse-int2 p op char-string)))
       ((and (pred (<= 251)) (pred (>= 254)))
        (fontsloth:debug* fontsloth-log "Int3 %s" op)
        (setq char-string (fontsloth-otf-cff--parse-int3 p op char-string)))
       ((pred (eq fontsloth-otf-cff-fixed-16-16))
        (fontsloth:debug* fontsloth-log "Fixed %s" op)
        (setq char-string (fontsloth-otf-cff--parse-fixed p char-string)))))))

(defun fontsloth-otf-cff-parse-char-string (data metadata glyph-id outliner)
  "Interpret GLYPH-ID's postscript DATA with METADATA into OUTLINER."
  (let* ((ctx (fontsloth-otf-cff-parse-context-create
               :metadata metadata
               :local-subrs (map-nested-elt metadata '(local-subrs data) nil)
               :glyph-id glyph-id))
         (builder (fontsloth-otf-cff-builder-create :outliner outliner))
         (stack (fontsloth-otf-cff-arguments-stack-create
                 :data nil
                 :len 0
                 :max-len fontsloth-otf-cff-max-args-stack-len))
         (parser (fontsloth-otf-cff-char-string-parser-create
                  :stack stack
                  :builder builder)))
    (fontsloth-otf-cff--parse-char-string ctx data 0 parser)
    (unless (fontsloth-otf-cff-parse-context-has-endchar ctx)
      (error "Missing endchar for glyph-id %s" glyph-id))
    (let ((bbox (fontsloth-otf-cff-builder-bbox builder)))
      (when (equal bbox (fontsloth-bbox-create))
        (fontsloth:verbose* fontsloth-log "Zero bbox for glyph-id %s" glyph-id)))))

(defun fontsloth-otf-cff-outline (cff glyph-id outliner)
  "Outline GLYPH-ID given CFF table into OUTLINER."
  (fontsloth:verbose* fontsloth-log "Cff: outlining glyph %s" glyph-id)
  (when-let* ((char-strings (map-nested-elt cff '(char-strings data)))
              (char-string (aref char-strings glyph-id))
              (stream (stream char-string)))
    (fontsloth:debug* fontsloth-log "Outlining char-string %s" char-string)
    (fontsloth-otf-cff-parse-char-string stream cff glyph-id outliner)))

(provide 'fontsloth-otf-cff)
;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:
;;; fontsloth-otf-cff.el ends here
