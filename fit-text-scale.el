;;; fit-text-scale.el --- Fit text by scaling -*- lexical-binding: t ; eval: (view-mode 1) -*-

;; THIS FILE HAS BEEN GENERATED.

;; Author: <marcowahlsoft@gmail.com>
;; Keywords: convenience

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*prologue][prologue:2]]

;; Copyright (C) 2017-2019 Marco Wahl
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ~fit-text-scale~ is an automation to set the scale so that the text
;; uses the maximal space to fit in the window.
;; 
;; Scale is the scale of the font.
;; 
;; There are three functions:
;; - Choose the maximal text scale to still see the full line.
;; - Choose the maximal text scale to still see the full lines.
;; - Choose the maximal text scale to still see all lines of a buffer.

;; Use
;; 
;; - ~M-x fts-max-font-size-fit-line~
;;   - Choose about maximal text scale so that the *current* line still
;;     fits in current window.
;; - ~M-x fts-max-font-size-fit-lines~
;;   - Choose about maximal text scale so that longest visible at cursor
;;     and below line still fits in current window.
;; - ~M-x fts-max-font-size-fit-buffer~
;;   - Choose about maximal text scale so that the buffer content still
;;     fits in current window.

;;; Code:
;; prologue:2 ends here

;; truncated lines environment
;; :PROPERTIES:
;; :ID:       1418004a-5c5f-4c19-9738-78b7efbef3dc
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*truncated lines environment][truncated lines environment:1]]

(defmacro fts-with-truncated-lines (&rest body)
  (let ((truncate-lines-before (gensym)))
    `(let ((,truncate-lines-before truncate-lines))
      (unless ,truncate-lines-before
        (toggle-truncate-lines))
      (unwind-protect
          (progn
            ,@body)
        (unless ,truncate-lines-before
          (toggle-truncate-lines))))))
;; truncated lines environment:1 ends here

;; text scale wrapper
;; :PROPERTIES:
;; :ID:       17ed5806-2afd-4771-8495-89558378e2d5
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:1]]

;; text scale wrapper
;; text scale wrapper:1 ends here

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:2]]
(defvar fts-hesitation 0)
;; text scale wrapper:2 ends here

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:3]]
(defun fts--increase ()
  (text-scale-increase 1)
  (sit-for fts-hesitation))
;; text scale wrapper:3 ends here

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:4]]
(defun fts--decrease ()
  (text-scale-decrease 1)
  (sit-for fts-hesitation))
;; text scale wrapper:4 ends here

;; measurement
;; :PROPERTIES:
;; :ID:       6f4c44ee-0f77-40d5-9ba2-d1d384fcc9ca
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*measurement][measurement:1]]

;; measurement

(require 'face-remap) ; text-scale- functions

(defun fts--line-width-in-pixel ()
  "Calculate line width containing point in pixel."
  (save-excursion
    (let* ((start (save-excursion (beginning-of-visual-line) (point)))
           (end (save-excursion (end-of-visual-line) (point))))
      (beginning-of-visual-line)
      (if (and (posn-at-point start) (posn-at-point end))
          (- (car (posn-x-y (posn-at-point end)))
             (car (posn-x-y (posn-at-point start))))
        (1+ (fts--window-width-in-pixel))))))

(defun fts--window-width-in-pixel ()
  "Return window width in pixel."
  (let* ((window-inside-pixel-edges (window-inside-pixel-edges)))
    (- (nth 2 window-inside-pixel-edges)
       (nth 0 window-inside-pixel-edges))))

(defun fts--buffer-height-fits-in-window-p ()
  (save-excursion
    (goto-char (point-min))
    (sit-for 0)
    (posn-at-point (point-max))))
;; measurement:1 ends here

;; find longest line
;; :PROPERTIES:
;; :ID:       1b3fd6e6-bf2b-4897-8f18-b732f6753cf8
;; :END:

;; the longest line length is essential to fit a part horizontally into a
;; given window.


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*find longest line][find longest line:1]]

;; find longest line

(defvar fts-consider-max-number-lines 42)

;;;###autoload
(defun fts-goto-visible-line-of-max-length ()
  "Set point into longest visible line.
Take at most `fts-consider-max-number-lines' lines into account."
  (interactive)
  (fts-with-truncated-lines
   (let* ((max-line-number
           (min (save-excursion (move-to-window-line -1))
                fts-consider-max-number-lines))
          (n 0)
          (index-of-max-line-length 0)
          (max-length (save-excursion
                        (move-to-window-line n)
                        (fts--line-width-in-pixel))))
     (while (< n max-line-number)
       (incf n)
       (move-to-window-line n)
       (let ((hl-line-mode t)) (hl-line-highlight))
       (sit-for 0) ; get visual progress indicator.
       (let ((length-candidate  (save-excursion
                        (move-to-window-line n)
                        (fts--line-width-in-pixel))))
         (when (< max-length length-candidate)
           (setq max-length length-candidate)
           (setq index-of-max-line-length n)))
       (let ((hl-line-mode t)) (hl-line-highlight)))
     (move-to-window-line index-of-max-line-length)
     (let ((hl-line-mode nil)) (hl-line-highlight)))))

(defun fts-goto-visible-line-of-max-length-down ()
  "Set point into longest visible line looking downwards.
Take at most `fts-consider-max-number-lines' lines into account."
  (interactive)
  (fts-with-truncated-lines
   (let* ((point-in-bottom-window-line
           (save-excursion (move-to-window-line -1) (point)))
          (n 0)
          (max-length (fts--line-width-in-pixel))
          (target (point)))
     (while (and (< n fts-consider-max-number-lines)
                 (<= (point) point-in-bottom-window-line)
                 (not (eobp)))
       (forward-line)
       (incf n)
       (let ((length-candidate (fts--line-width-in-pixel)))
         (when (< max-length length-candidate)
           (setq max-length length-candidate)
           (setq target (point)))))
     (goto-char target))))
;; find longest line:1 ends here

;; fit in window
;; :PROPERTIES:
;; :ID:       9df260fe-b9dc-4444-8fab-56ea1cb9ebd5
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*fit in window][fit in window:1]]

;; fit in window
;;;###autoload
(defun fts-max-font-size-fit-buffer ()
  "Use the maximal text scale to fit the buffer in the window.
When at minimal text scale stay there and inform."
  (interactive)
  (save-excursion
    (while (and (fts--buffer-height-fits-in-window-p)
                (< (or text-scale-mode-amount 0)
                   (text-scale-max-amount)))
      (fts--increase))
    (while (and
            (not (fts--buffer-height-fits-in-window-p))
            (< (1+ (text-scale-min-amount))
               (or text-scale-mode-amount 0)))
      (fts--decrease))
    (when (= (floor (text-scale-max-amount))
             (or text-scale-mode-amount 0))
      (message "At maximal text scale."))
    (when (= (floor (text-scale-min-amount))
             (or text-scale-mode-amount 0))
      (message "At minimal text scale."))))

;;;###autoload
(defun fts-max-font-size-fit-line ()
  "Use the maximal text scale to fit the line in the window.
Pracmatic tip: if this function gives a text scale not as big as
it could be then a further call might.

DO try to get rid of the factor trick thing below.  this might be
when `text-rescale-line-width-in-pixel' is fixed."
  (interactive)
  (text-scale-mode)
  (fts-with-truncated-lines
   (let
       ((factor 1.05)
        (min-width 23)
        (fts-max-amount 15)
        (fts-min-amount -12))
     (save-excursion
       (while (and (< text-scale-mode-amount fts-max-amount)
                   (<= (* factor (max min-width (fts--line-width-in-pixel)))
                       (fts--window-width-in-pixel)))
         (fts--increase))
       (while (and (< fts-min-amount text-scale-mode-amount)
                   (< (fts--window-width-in-pixel)
                      (* factor (max min-width (fts--line-width-in-pixel)))))
         (fts--decrease))))))

;;;###autoload
(defun fts-max-font-size-fit-lines ()
  "Use the maximal text scale to fit the line and lines below in the window.
If this function gives a text scale not as big as it could be
then the next call might."
  (interactive)
  (save-excursion
    (fts-goto-visible-line-of-max-length-down)
    (fts-max-font-size-fit-line)))
;; fit in window:1 ends here

;; epilogue
;; :PROPERTIES:
;; :ID:       1ee365eb-e9ce-4ac3-ac14-1b2361d55ed8
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*epilogue][epilogue:1]]

(provide 'fit-text-scale)


;;; fit-text-scale.el ends here
;; epilogue:1 ends here
