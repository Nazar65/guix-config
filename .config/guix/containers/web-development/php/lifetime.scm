(define-module (php lifetime)
  #:use-module (php magento2-base-container)
  #:use-module (guix)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (packages opensearch)
  #:use-module (gnu services pm)
  #:use-module (guix packages)
  #:use-module (gnu services base)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (packages php)
  #:use-module (guix packages))

(set! %app-front-name '("development.local"))
(set! %active-php-package php82)

(operating-system
  (inherit magento2-container)
  (host-name "lifetime-shop-magento2")
  (services  (cons*
              (service %opensearch-activation-service-type)
              (service %opensearch-service-type '(""))
              %magento2-base-services))
  (packages
   (append
    (list
     (specification->package "opensearch")
     (specification->package "php@8.2")
     (specification->package "mysql")
     (specification->package "redis"))
    %base-packages)))
