;;; fit-text-scale.el --- Fit text by scaling -*- lexical-binding: t ; eval: (view-mode 1) -*-

;; THIS FILE HAS BEEN GENERATED.


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*prologue][prologue:2]]

;; Copyright (C) 2017-2020 Marco Wahl
;; 
;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl <marcowahlsoft@gmail.com>
;; Created: 2017
;; Version: 1.0.0
;; Package-Requires: ((emacs "25"))
;; Keywords: convenience
;; URL: https://gitlab.com/marcowahl/fit-text-scale
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

;; 
;; You are too lazy to do the C-x C-+ + + +... - - - - ... + + - dance
;; all the time to see the FULL line in maximal font size?
;; 
;; You want a keystroke to see the whole buffer at once by changing the
;; font size?

;; 
;; ~fit-text-scale~ is an automation to set the scale so that the text
;; uses the maximal space to fit in the window.
;; 
;; Scale is the scale of the font.
;; 
;; There are three functions:
;; - Choose the maximal text scale to still see the full line.
;; - Choose the maximal text scale to still see the full lines.
;; - Choose the maximal text scale to still see all lines of a buffer.

;; The following code in an init file binds the
;; functionality to keys.
;; 
;; #+begin_src emacs-lisp
;; (global-set-key
;;  (kbd "C-x C-&")
;;  (lambda (&optional arg)
;;    (interactive "P")
;;    (apply
;;     (if arg
;;         #'fts-max-font-size-fit-line
;;       #'fts-max-font-size-fit-lines)
;;     nil)))
;; 
;; (global-set-key
;;  (kbd "C-x C-*")
;;    #'fts-max-font-size-fit-buffer)
;; #+end_src
;; 
;; With these settings there is
;; 
;; - ~C-x C-&~
;;   - Choose maximal text scale so that the longest line
;;     below still fits in current window.
;; - ~C-u C-x C-&~
;;   - Choose maximal text scale so that the *current* line still
;;     fits in the window.
;; - ~C-x C-*
;;   - Choose maximal text scale so that the vertical buffer content
;;     still fits into current window.
;; - ~C-x C-0~ (Already given.  This is good old ~text-scale-adjust~.)
;;   - Switch back to the default size when control about the sizes has
;;     been lost.
;; - ~C-x C-+~ + - and ~C-x C--~ - + for fine tuning.  (Also given.)
;; - ~C-g C-g C-g~... (hit the keyboard hard!) if something, hrm, hangs.

;; There are some parameters to fine tune the functionality.  Check it out with
;; 
;;     M-x customize-group fit-text-scale
;; 

;; To install fit-text-scale add the lines (with YOUR installation path)
;; to your init file.
;; 
;; #+begin_src emacs-lisp
;; (push "/PATH/TO/fit-text-scale" load-path)
;; (require 'fit-text-scale)
;; #+end_src
;; 
;; Consider to add also the keybindings above.

;;; Code:
;; prologue:2 ends here

;; customizables


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*customizables][customizables:1]]

;; customizables
;; customizables:1 ends here

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*customizables][customizables:2]]
(defcustom fts-hesitation 0.01
  "Duration to wait til next text scale change.
Smallest sane value is 0 which should result in the fastest
animation.  Only effective when `fts-graphic-sugar' is on."
  :type 'number
  :group 'fit-text-scale)

(defcustom fts-graphic-sugar t
  "Animate the zoom.  `fts-hesitation' controls the animation speed."
  :type 'boolean
  :group 'fit-text-scale)

(define-obsolete-variable-alias 'fts-graphic-suger 'fts-graphic-sugar "2020-02-13")

(defcustom fts-max-amount 23
  "Maximum achievable text scale with this program."
  :type 'number
  :group 'fit-text-scale)

(defcustom fts-min-amount -12
  "Minimum achievable text scale with this program."
  :type 'number
  :group 'fit-text-scale)

(defcustom fts-consider-max-number-lines 42
"Maximum number of lines to consider before choosing
the longest in function `fts-max-font-size-fit-lines'."
  :type 'integer
  :group 'fit-text-scale )
;; customizables:2 ends here

;; text scale wrapper
;; :PROPERTIES:
;; :ID:       17ed5806-2afd-4771-8495-89558378e2d5
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:1]]

;; text scale wrapper
;; text scale wrapper:1 ends here

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:2]]
(require 'face-remap)  ; text-scale- functions
;; text scale wrapper:2 ends here

;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*text scale wrapper][text scale wrapper:3]]
(defun fts--increase ()
  (text-scale-increase 1)
  (when fts-graphic-sugar
    (sit-for fts-hesitation)))

(defun fts--decrease ()
  (text-scale-decrease 1)
  (when fts-graphic-sugar
    (sit-for fts-hesitation)))
;; text scale wrapper:3 ends here

;; measurement
;; :PROPERTIES:
;; :ID:       6f4c44ee-0f77-40d5-9ba2-d1d384fcc9ca
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*measurement][measurement:1]]

;; measurement

(defun fts--line-length ()
  "Calculate line width containing point in chars."
  (- (save-excursion (end-of-visible-line) (point))
     (save-excursion (beginning-of-line) (point))))

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

;; Finding the longest line is essential to fit a part horizontally into
;; a given window.


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*find longest line][find longest line:1]]

;; find longest line

;;;###autoload
(defun fts-goto-visible-line-of-max-length-down ()
  "Set point into longest visible line looking downwards.
Take at most `fts-consider-max-number-lines' lines into account."
  (interactive)
  (let* ((point-in-bottom-window-line
          (save-excursion (move-to-window-line -1) (point)))
         (n 0)
         (max-length (fts--line-length))
         (target (point)))
    (while (and (< n fts-consider-max-number-lines)
                (<= (point) point-in-bottom-window-line)
                (not (eobp)))
      (let ((length-candidate (fts--line-length)))
        (when (< max-length length-candidate)
          (setq max-length length-candidate)
          (setq target (point))))
      (forward-line)
      (incf n))
    (goto-char target)))
;; find longest line:1 ends here

;; fit in window horizontally


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*fit in window horizontally][fit in window horizontally:1]]

;;;###autoload
(defun fts-max-font-size-fit-line ()
  "Use the maximal text scale to fit the line in the window."
  (interactive)
  (text-scale-mode)
  (beginning-of-line)
  (let ((eol (progn (save-excursion (end-of-visible-line)
                                    (point)))))
    (assert (<= (progn (save-excursion (end-of-visual-line) (point)))
                eol)
            "programming logic error.  this is a bad sign.  please report the issue.")
    (while (and (< text-scale-mode-amount fts-max-amount)
                (= (progn (save-excursion (end-of-visual-line) (point))) eol))
      (fts--increase))
    (while  (and (< fts-min-amount text-scale-mode-amount)
                 (< (progn (save-excursion (end-of-visual-line) (point))) eol))
      (fts--decrease))))

;;;###autoload
(defun fts-max-font-size-fit-lines ()
  "Use the maximal text scale to fit the line and lines below in the window.
If this function gives a text scale not as big as it could be
then the next call might."
  (interactive)
  (save-excursion
    (fts-goto-visible-line-of-max-length-down)
    (fts-max-font-size-fit-line)))
;; fit in window horizontally:1 ends here

;; fit in window vertically


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*fit in window vertically][fit in window vertically:1]]

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
;; fit in window vertically:1 ends here

;; epilogue
;; :PROPERTIES:
;; :ID:       1ee365eb-e9ce-4ac3-ac14-1b2361d55ed8
;; :END:


;; [[file:~/p/elisp/mw/fit-text-scale/fit-text-scale.org::*epilogue][epilogue:1]]

(provide 'fit-text-scale)


;;; fit-text-scale.el ends here
;; epilogue:1 ends here
