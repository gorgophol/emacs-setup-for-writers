;;; highlight-sentence-length.el --- colorize sentences by their length to aid in writing

;; Copyright (C) 2019  Arne Babenhauserheide <babenhauserheide@dev113>

;; Author: Arne Babenhauserheide <babenhauserheide@dev113>
;; Keywords: convenience, faces
;; Created: 23 Sep 2019

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; you might want to customize sentence-end-base to
;; "[.?!…‽:][]"'”’)}]*" - then it ends sentences with :

;;; Code:

(provide 'highlight-sentence-length)
;;; highlight-sentence-length.el ends here

(require 'thingatpt)

(defcustom hsl-colors
  '(
    (1   "#f00") ;feuerrot
    (2   "#f06") ;purpurrot
    (3   "#842") ;dunkelorange
    (4   "#800") ;dunkelrot
    (5   "#c88") ;rosa
    (6   "#c90") ;golden
    (7   "#bb4") ;hellgelb
    (8   "#4b3") ;grasgrün
    (9   "#175") ;dunkelgrün
    (10  "#031") ;dunkelgrün
    (11  "#055") ;dunkelopalisierend
    (12  "#067") ;blaugrün
    (13  "#008") ;dunkelblau
    (15  "#148") ;ultramarin
    (20  "#4ab") ;hellopalisierend
    (30  "#8bd") ;hellblau
    (40  "#89a") ;silberblau
    (50  "#999") ;hellgrau
    (60  "#a8a") ;silberviolett
    (80  "#a6c") ;hellviolett
    (100 "#90d") ;dunkelviolett
    (120 "#808") ;violett
    (150 "#227") ;lila
    (200 "#321") ;braunschwarz
    (250 "#111") ;samtschwarz
    ) "Colors to show the sentence length."
      :group 'highlight-sentence-length
      :type 'list)

(defun colorize-current-sentence ()
  "Count the number of words in the current sentence."
  (interactive)
  (save-mark-and-excursion
   (with-silent-modifications
     (let ((sentence-end-double-space nil))
       (when (not (looking-at (sentence-end)))
         (forward-sentence))
       (let ((end (+ 1 (point))))
         (backward-sentence)
         (put-text-property (point) end
                            'face
                            `(:foreground
                              ,(color-for-sentence-length
                                (count-words-region (point) end)))))))))

(defun choose-matching-color (colors number)
  "Get the color from COLORS with at least the number NUMBER."
  (if (or (<= number (car (car colors))) (= 1 (length colors)))
      (car (cdr (car colors)))
    (choose-matching-color (cdr colors) number)))

(defun color-for-sentence-length (number-of-words)
  "Get the color for the given NUMBER-OF-WORDS."
  (choose-matching-color hsl-colors number-of-words))

(defun highlight-sentence-length (&optional begin end length)
  "Colorize a buffer or the region between BEGIN and END up to LENGTH by its chars."
  (interactive)
  (let (
        (begin (if (not begin)
                   1
                 begin))
        (end (if (not end)
                 (point-max)
               end)))
    (save-excursion
      (goto-char (point-min))
      (with-silent-modifications
        (let ((sentence-end-double-space nil))
          (while (string-match "[^ \n\r	]+" (buffer-substring (point) (point-max)))
            (let ((start (point)))
              (forward-sentence)
              (message (number-to-string (count-words-region start (+ 1 (point)))))
              (put-text-property start (point)
                                 'face
                                 `(:foreground
                                   ,(color-for-sentence-length
                                     (count-words-region start (+ 1 (point))))))
              (colorize-current-sentence))))))))


(defun colorize-current-sentence-after-change (begin end length-before)
  (save-excursion
    (ignore-errors
      (goto-char (max (point-min) (min begin (point-max)))))
    (colorize-current-sentence)))

(define-minor-mode highlight-sentence-length-mode
  "Highlight the length of sentences with colors based on the number of words."
  :lighter " hsl")

(add-hook 'highlight-sentence-length-mode-hook (lambda () (add-hook 'after-change-functions 'colorize-current-sentence-after-change t t)))

(provide 'highlight-sentence-length)

;;; highlight-sentence-length.el ends here
