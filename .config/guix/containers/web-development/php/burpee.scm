(define-module (php burpee)
  #:use-module (php magento2-base-container)
  #:use-module (guix)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (packages php)
  #:use-module (guix packages))

(set! %app-front-name '("development.local"))
(set! %active-php-package php81)

(operating-system
 (inherit magento2-container)
 (host-name "burpee-magento2")
    (packages
    (append
     (list
      (specification->package "php@8.1")
      (specification->package "mysql")
      (specification->package "redis"))
     %base-packages)))
