;;; fit-text-scale.el --- Fit text by scaling -*- lexical-binding: t ; eval: (view-mode 1) -*-

;; THIS FILE HAS BEEN GENERATED.

;; Author: <marcowahlsoft@gmail.com>
;; Keywords: convenience

;; [[id:dc521e3c-123a-429f-9ad2-8451c1a11035][prologue:2]]

;; Copyright (C) 2017, 2018 Marco Wahl
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

;; ~fit-text-scale~ provides functions to maximize the fontsize to fit
;; the text into a window.
;; 
;; Up to now there are three functions:
;; - Choose the maximal text scale to still see the full line.
;; - Choose the maximal text scale to still see the full lines.
;; - Choose the maximal text scale to still see all lines of a buffer.

;; Use
;; 
;; - ~M-x fts-max-font-size-see-lines~
;;   - Choose about maximal text scale so that longest visible line still
;;     fits in current window.
;; - ~M-x fts-max-font-size-see-line~
;;   - Choose about maximal text scale so that the *current* line still
;;     fits in current window.
;; - ~M-x fts-max-font-size-see-buffer~
;;   - Choose about maximal text scale so that the buffer content still
;;     fits in current window.

;;; Code:
;; prologue:2 ends here

;; visual line number
;; :PROPERTIES:
;; :ID:       e5ae819e-c71e-4389-bdf5-b7497be1c566
;; :END:

;; Consider just the window lines.


;; [[id:e5ae819e-c71e-4389-bdf5-b7497be1c566][visual line number:1]]
(defun fts-window-line-number-with-point ()
  "Return number of window line.
Top line is 0."
  ;; Note: Think about speed up by exponential seach.
  (let ((pt (save-excursion (forward-line 0) (point)))
        (n 0))
    (save-excursion
      (while (< (progn
                   (move-to-window-line n)
                   (point))
                 pt)
        (setq n (1+ n))))
    n))
;; visual line number:1 ends here

;; truncated lines environment
;; :PROPERTIES:
;; :ID:       1418004a-5c5f-4c19-9738-78b7efbef3dc
;; :END:


;; [[id:1418004a-5c5f-4c19-9738-78b7efbef3dc][truncated lines environment:1]]

(defmacro fts-with-truncated-lines (&rest body)
  `(let ((truncate-lines-before truncate-lines))
     (unless truncate-lines-before
       (toggle-truncate-lines 1))
     (unwind-protect
         (progn
           ,@body)
       (unless truncate-lines-before
         (toggle-truncate-lines 1)))))
;; truncated lines environment:1 ends here

;; text scale wrapper
;; :PROPERTIES:
;; :ID:       17ed5806-2afd-4771-8495-89558378e2d5
;; :END:


;; [[id:17ed5806-2afd-4771-8495-89558378e2d5][text scale wrapper:1]]

;; text scale wrapper
;; text scale wrapper:1 ends here

;; [[id:17ed5806-2afd-4771-8495-89558378e2d5][text scale wrapper:2]]
(defun fts--increase ()
  (text-scale-increase 1)
  (sit-for 0.2))
;; text scale wrapper:2 ends here

;; [[id:17ed5806-2afd-4771-8495-89558378e2d5][text scale wrapper:3]]
(defun fts--decrease ()
  (text-scale-decrease 1)
  (sit-for 0.2))
;; text scale wrapper:3 ends here

;; measurement
;; :PROPERTIES:
;; :ID:       6f4c44ee-0f77-40d5-9ba2-d1d384fcc9ca
;; :END:


;; [[id:6f4c44ee-0f77-40d5-9ba2-d1d384fcc9ca][measurement:1]]

;; measurement

(require 'face-remap) ; text-scale- functions

(defun fts--line-width-in-pixel ()
  "Calculate line width containing point in pixel.

DO get this function right!
"
  (fts-with-truncated-lines
   (save-excursion
     (let* ((start (save-excursion (beginning-of-visual-line) (point)))
            (end (save-excursion (end-of-visual-line) (point))))
       (beginning-of-visual-line)
       (if (and (posn-at-point start) (posn-at-point end))
           (- (car (posn-x-y (posn-at-point end)))
              (car (posn-x-y (posn-at-point start))))
         (1+ (fts--window-width-in-pixel)))))))

(defun fts--window-width-in-pixel ()
  "Return window width in pixel."
  (let* ((window-inside-pixel-edges (window-inside-pixel-edges)))
    (- (nth 2 window-inside-pixel-edges)
       (nth 0 window-inside-pixel-edges))))

(defun fts--buffer-height-fits-in-window-p ()
  (save-excursion
    (let* ((end (point-max))
           (start (point-min)))
      (goto-char start)
      (posn-at-point end))))
;; measurement:1 ends here

;; find longest line
;; :PROPERTIES:
;; :ID:       1b3fd6e6-bf2b-4897-8f18-b732f6753cf8
;; :END:


;; [[id:1b3fd6e6-bf2b-4897-8f18-b732f6753cf8][find longest line:1]]

;; find longest line

(defvar fts-consider-max-number-lines 42)

;;;###autoload
(defun fts-goto-visible-line-of-max-length ()
  "Set point into longest line.
Take at most `fts-consider-max-number-lines' lines into account."
  (interactive)
  (fts-with-truncated-lines
   (let* ((max-line-number
           (min (save-excursion (move-to-window-line -1)
                                (fts-window-line-number-with-point))
                fts-consider-max-number-lines))
          (n 0)
          (index-of-max-line-length 0)
          (max-length (save-excursion
                        (move-to-window-line n)
                        (fts--line-width-in-pixel))))
     (while (< n max-line-number)
       (incf n)
       (move-to-window-line n)
       (let ((length-candidate  (save-excursion
                        (move-to-window-line n)
                        (fts--line-width-in-pixel))))
         (when (< max-length length-candidate)
           (setq max-length length-candidate)
           (setq index-of-max-line-length n))))
     (move-to-window-line index-of-max-line-length))))
;; find longest line:1 ends here

;; fit in window
;; :PROPERTIES:
;; :ID:       9df260fe-b9dc-4444-8fab-56ea1cb9ebd5
;; :END:


;; [[id:9df260fe-b9dc-4444-8fab-56ea1cb9ebd5][fit in window:1]]

;; fit in window
;;;###autoload
(defun fts-max-font-size-see-buffer ()
  "Use the maximal text scale to fit the buffer in the window.
When at minimal text scale stay there and inform."
  (interactive)
  (save-excursion
    (while (fts--buffer-height-fits-in-window-p)
      (fts--increase))
    (while (and
            (not (fts--buffer-height-fits-in-window-p))
            (< (1+ (text-scale-min-amount))
               (if text-scale-mode text-scale-mode-amount 0)))
      (fts--decrease))
    (when (= (floor (text-scale-min-amount))
             (if text-scale-mode text-scale-mode-amount 0))
      (message "At minimal text scale."))))

;;;###autoload
(defun fts-max-font-size-see-line ()
  "Use the maximal text scale to fit the line in the window.
If this function gives a text scale not as big as it could be
then the next call might.

DO try to get rid of the factor trick thing below.  this might be
when `text-rescale-line-width-in-pixel' is fixed."
  (interactive)
  (text-scale-mode)
  (fts-with-truncated-lines
   (let
       ((factor 1.05)
        (min-width 23)
        (fts-max-amount 20)
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
(defun fts-max-font-size-see-lines ()
  "Use the maximal text scale to fit the lines on the screen in the window.
If this function gives a text scale not as big as it could be
then the next call might."
  (interactive)
  (save-excursion
    (fts-goto-visible-line-of-max-length)
    (fts-max-font-size-see-line)))
;; fit in window:1 ends here

;; epilogue
;; :PROPERTIES:
;; :ID:       1ee365eb-e9ce-4ac3-ac14-1b2361d55ed8
;; :END:


;; [[id:1ee365eb-e9ce-4ac3-ac14-1b2361d55ed8][epilogue:1]]

(provide 'fit-text-scale)


;;; fit-text-scale.el ends here
;; epilogue:1 ends here
