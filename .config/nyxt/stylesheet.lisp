(in-package #:nyxt-user)

;; This only works on the versions of Nyxt after 2.2.4.
(define-configuration browser
  ((theme (make-instance
           'theme:theme
           :dark-p t
	   :on-accent-color "white"
	   :on-background-color "white"
           :background-color "#282a36"
           :accent-color "#445075"
           :primary-color "#444744"
	   :on-secondary-color "white"
           :secondary-color "#6272a4"))))

;; Custom Dark-mode for webpages
(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "#282a36 !important"
                :background-image "none !important"
                :color "#f8f8f2")
               (a
                :background-color "#282a36 !important"
                :background-image "none !important"
                :color "#6272a4 !important"))))))
