;;; fit-text-scale.el --- Fit text by scaling -*- lexical-binding: t -*-

;; Truncated lines environment
;; :PROPERTIES:
;; :ID:       1418004a-5c5f-4c19-9738-78b7efbef3dc
;; :END:


;; [[id:1418004a-5c5f-4c19-9738-78b7efbef3dc][Truncated lines environment:1]]

(defmacro fit-text-scale-with-truncated-lines (&rest body)
  `(let ((truncate-lines-before truncate-lines))
     (unless truncate-lines
       (toggle-truncate-lines 1))
     (unwind-protect
         (progn
           ,@body)
       (toggle-truncate-lines (if truncate-lines-before 1 0)))))
;; Truncated lines environment:1 ends here

;; Text scale wrapper
;; :PROPERTIES:
;; :ID:       17ed5806-2afd-4771-8495-89558378e2d5
;; :END:


;; [[id:17ed5806-2afd-4771-8495-89558378e2d5][Text scale wrapper:1]]
(defun fit-text-scale--increase ()
  (text-scale-increase 1)
  (sit-for .1))
;; Text scale wrapper:1 ends here

;; [[id:17ed5806-2afd-4771-8495-89558378e2d5][Text scale wrapper:2]]
(defun fit-text-scale--decrease ()
  (text-scale-decrease 1)
  (sit-for .1))
;; Text scale wrapper:2 ends here

;; Measurement
;; :PROPERTIES:
;; :ID:       6f4c44ee-0f77-40d5-9ba2-d1d384fcc9ca
;; :END:


;; [[id:6f4c44ee-0f77-40d5-9ba2-d1d384fcc9ca][Measurement:1]]

(require 'face-remap) ; text-scale- functions

(defun fit-text-scale--line-width-in-pixel ()
  "Calculate line width in pixel.

DO get this function right!
"
  (fit-text-scale-with-truncated-lines
   (save-excursion
     (let* ((start (save-excursion (beginning-of-visual-line) (point)))
            (end (save-excursion (end-of-visual-line) (point))))
       (beginning-of-visual-line)
       (if (and (posn-at-point start) (posn-at-point end))
           (- (car (posn-x-y (posn-at-point end)))
              (car (posn-x-y (posn-at-point start))))
         (1+ (fit-text-scale--window-width-in-pixel)))))))

(defun fit-text-scale--window-width-in-pixel ()
  "Return window width in pixel."
  (let* ((window-inside-pixel-edges (window-inside-pixel-edges)))
    (- (nth 2 window-inside-pixel-edges)
       (nth 0 window-inside-pixel-edges))))

(defun fit-text-scale--buffer-height-fits-in-window-p ()
  (save-excursion
    (let* ((end (point-max))
           (start (point-min)))
      (goto-char start)
      (posn-at-point end))))
;; Measurement:1 ends here

;; Find longest line
;; :PROPERTIES:
;; :ID:       1b3fd6e6-bf2b-4897-8f18-b732f6753cf8
;; :END:


;; [[id:1b3fd6e6-bf2b-4897-8f18-b732f6753cf8][Find longest line:1]]
(defun fit-text-scale-goto-visible-line-of-max-length ()
  "Set point into longest line.
Take at most 84 lines into account."
  (interactive)
  (fit-text-scale-with-truncated-lines
   (let* ((max-line-number (min (save-excursion (move-to-window-line -1)
                                                (mw-visual-line-number-with-point))
                                84))
          (n 0)
          (index-of-max-line-length 0)
          (max-length (- (progn (move-to-window-line n)
                                (end-of-line)
                                (point))
                         (progn (move-to-window-line n)
                                (point)))))
     (while (< n max-line-number)
       (incf n)
       (move-to-window-line n)
       (let ((length-candidate
              (- (progn (move-to-window-line n)
                        (end-of-line)
                        (point))
                 (progn (move-to-window-line n)
                        (point)))))
         (when (< max-length length-candidate)
           (setq max-length length-candidate)
           (setq index-of-max-line-length n))))
     (move-to-window-line index-of-max-line-length))))
;; Find longest line:1 ends here

;; Fit in window
;; :PROPERTIES:
;; :ID:       9df260fe-b9dc-4444-8fab-56ea1cb9ebd5
;; :END:


;; [[id:9df260fe-b9dc-4444-8fab-56ea1cb9ebd5][Fit in window:1]]
(defun fit-text-scale-max-font-size-whole-buffer ()
  "Use the maximal text scale to fit the buffer in the window.
When at minimal text scale stay there and inform."
  (interactive)
  (save-excursion
    (while (fit-text-scale--buffer-height-fits-in-window-p)
      (fit-text-scale--increase))
    (while (and
            (not (fit-text-scale--buffer-height-fits-in-window-p))
            (< (1+ (text-scale-min-amount))
               (if text-scale-mode text-scale-mode-amount 0)))
      (fit-text-scale--decrease))
    (when (= (floor (text-scale-min-amount))
             (if text-scale-mode text-scale-mode-amount 0))
      (message "At minimal text scale."))))

(defun fit-text-scale-see-whole-line-max-font-size ()
  "Use the maximal text scale to fit the line in the window.
If this function gives a text scale not as big as it could be
then the next call might.

DO try to get rid of the factor trick thing below.  this might be
when `text-rescale-line-width-in-pixel' is fixed.
"
  (interactive)
  (fit-text-scale-with-truncated-lines
   (let ((factor 1.05))
     (save-excursion
       (while (<= (* factor (fit-text-scale--line-width-in-pixel))
                  (fit-text-scale--window-width-in-pixel))
         (fit-text-scale--increase))
       (while (< (fit-text-scale--window-width-in-pixel)
                 (* factor (fit-text-scale--line-width-in-pixel)))
         (fit-text-scale--decrease))))))

(defun fit-text-scale-see-whole-lines-max-font-size ()
  "Use the maximal text scale to fit the lines on the screen in the window.
If this function gives a text scale not as big as it could be
then the next call might."
  (interactive)
  (save-excursion
    (fit-text-scale-goto-visible-line-of-max-length)
    (fit-text-scale-see-whole-line-max-font-size)))
;; Fit in window:1 ends here

;; Epilogue
;; :PROPERTIES:
;; :ID:       1ee365eb-e9ce-4ac3-ac14-1b2361d55ed8
;; :END:


;; [[id:1ee365eb-e9ce-4ac3-ac14-1b2361d55ed8][Epilogue:1]]

(provide 'fit-text-scale)
;;; fit-text-scale.el ends here
;; Epilogue:1 ends here