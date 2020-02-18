;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I am in empty buffer \"\\(.+\\)\"$"
  (lambda (something)
    (set-buffer (get-buffer-create something))
    (erase-buffer)))

(And "^buffer \"\\(.+\\)\" is current and empty$"
  (lambda (something)
    (switch-to-buffer something)
    (delete-other-windows)))

(And "^I insert$"
     (lambda (text)
       (insert text)))

(When "^I fit scale vertically$"
  (lambda ()
    (fit-text-scale-max-font-size-fit-buffer)))

(Then "^I should see all lines$"
      (lambda ()
        (goto-char 1)
        (move-to-window-line -1)
        (end-of-line)
        (require 'cl-lib)
        (cl-assert (or (= (point-max) (point)))
                   nil
                   "Point is not at the end of the buffer.")))

(Then "^I should see almost all lines$"
      (lambda ()
        (goto-char 1)
        (move-to-window-line -1)
        (end-of-line)
        (require 'cl-lib)
        (cl-assert (or (< (line-number-at-pos (point-max) 'absolute)
                          (+ 2 (line-number-at-pos (point) 'absolute)))))))
