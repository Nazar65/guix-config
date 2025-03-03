(define-module (php sharkgaming)
  #:use-module (php magento2-base-container)
  #:use-module (guix)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (packages php)
  #:use-module (guix packages))

(set! %active-php-package php82)
(set! %app-front-name '("development.local"))

(operating-system
 (inherit magento2-container)
 (host-name "sharkgaming-magento2")
    (packages
    (append
     (list
      (specification->package "php@8.2")
      (specification->package "mysql")
      (specification->package "redis"))
     %base-packages)))
