(require 'f)

(defvar fit-text-scale-support-path
  (f-dirname load-file-name))

(defvar fit-text-scale-features-path
  (f-parent fit-text-scale-support-path))

(defvar fit-text-scale-root-path
  (f-parent fit-text-scale-features-path))

(add-to-list 'load-path fit-text-scale-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'fit-text-scale)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
