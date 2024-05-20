(define-module (php magento2-base-container)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (packages php)
  #:use-module (gnu services pm)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (guix packages))

(define-public %app-root-folder "/srv/http/pub")
(define-public %app-front-name '("development.local"))
(define-public %nginx-config-file-path "/home/nazar/guix-system/.config/nginx/nginx.conf")
(define-public %active-php-package php81)
(define-public %local-php-ini "/home/nazar/guix-system/.config/php/php.ini")
(define-public %php-socket-path
  (string-append "/var/run/php"
		 (version-major
		  (package-version %active-php-package))
		 "-fpm.sock"))
(define (nginx-raw-content config-path)
  (list (string-append "include " config-path ";")))


(define-public magento2-container
  (operating-system
   (host-name "magento2-container")
   (timezone "Europe/Kiev")
   (file-systems (cons (file-system
			(device (file-system-label "does-not-matter"))
			(mount-point "/")
			(type "ext4"))
                       %base-file-systems))
   (bootloader (bootloader-configuration
		(bootloader grub-bootloader)
		(targets '("/dev/sdX"))))
   (services
    (cons* (service redis-service-type)
	   (service nginx-service-type
		    (nginx-configuration
		     (server-blocks
                      (list (nginx-server-configuration
			     (listen '("8081"))
			     (server-name %app-front-name)
			     (root %app-root-folder)
			     (index '("index.php"))
			     (raw-content (nginx-raw-content %nginx-config-file-path))
			     (locations
                              (list (nginx-php-location))))))))

	   (service php-fpm-service-type
		    (php-fpm-configuration
		     (php %active-php-package)
		     (socket %php-socket-path)
		     (user "nazar")
		     (group "users")
		     (socket-user "nazar")
		     (display-errors "#t")
		     (php-ini-file %local-php-ini)))

	   (service mysql-service-type
		    (mysql-configuration
		     (socket "/run/mysqld/mysqld.sock")
		     (auto-upgrade? "#f")))
           %base-services))))
