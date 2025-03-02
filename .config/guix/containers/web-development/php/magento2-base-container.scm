(define-module (php magento2-base-container)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (packages php)
  #:use-module (gnu packages admin)
  #:use-module (packages opensearch)
  #:use-module (gnu services pm)
  #:use-module (gnu services base)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages))

(define-public %app-root-folder "/srv/http/pub")
(define-public %webserver-root-folder "/srv/http")
(define-public %app-front-name '("development.local"))
(define-public %nginx-config-file-path "/home/nazar/guix-system/.config/nginx/nginx.conf")
(define-public %active-php-package php82)
(define-public %local-php-ini "/home/nazar/guix-system/.config/php/php.ini")
(define-public %php-socket-path
  (string-append "/var/run/php"
		 (version-major
		  (package-version %active-php-package))
		 "-fpm.sock"))
(define %opensearch-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/log/opensearch/")
      (mkdir-p "/var/lib/opensearch/config")
      (mkdir-p "/var/lib/opensearch/data")
      (copy-recursively #$(file-append opensearch "/config") "/var/lib/opensearch/config")
      (let ((user (getpwnam "opensearch")))
        (chown "/var/lib/opensearch"
               (passwd:uid user) (passwd:gid user))
        (chown "/var/lib/opensearch/config"
               (passwd:uid user) (passwd:gid user))
        (chown "/var/lib/opensearch/data"
               (passwd:uid user) (passwd:gid user))
        (chown "/var/log/opensearch"
               (passwd:uid user) (passwd:gid user)))))

(define %opensearch-accounts
  (list (user-group (name "opensearch") (system? #t))
        (user-account
         (name "opensearch")
         (group "opensearch")
         (system? #t)
         (comment "opensearch daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define-public %opensearch-activation-service-type
  (service-type (name 'opensearch-activation-service-type)
                (extensions
                 (list (service-extension account-service-type
                                          (const %opensearch-accounts))
                       (service-extension activation-service-type
                                          (const %opensearch-activation))))
                (default-value 'opensearch-activation-service-type)
                (description
                 "Run opensearch service")))
(define-public %opensearch-service-type
  (shepherd-service-type
   'opensearch-server
   (lambda (config)
     (shepherd-service
      (documentation "Start opensearch service")
      (auto-start? #f)
      (provision '(opensearch-server))
      (start #~(make-forkexec-constructor
                (list #$(file-append opensearch "/bin/opensearch"))
                #:environment-variables
                (cons* (string-append "OPENSEARCH_PATH_CONF=/var/lib/opensearch/config")
                       (string-append "DISABLE_SECURITY_PLUGIN=true")
                       (default-environment-variables))
                #:log-file  "/var/log/opensearch.log"
                #:user "opensearch" #:group "opensearch"))
      (stop #~(make-kill-destructor))))
   (description "Starts opensearch server")))

(define (nginx-raw-content config-path)
  (list (string-append "include " config-path ";")))


(define %mariadb-state-directory
  "/var/lib/mysql")

(define set-permissions-gexp
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        ;; Set ownership of mariadb state directory.
        (let ((user (getpw "mysql")))
          (for-each (lambda (file)
                      (chown file (passwd:uid user) (passwd:gid user)))
                    (find-files #$%mariadb-state-directory #:directories? #t)))
        ;; Set ownership of magento root directory.
        (let ((user (getpw "nazar")))
          (for-each (lambda (file)
                      (chown file (passwd:uid user) (passwd:gid user)))
                    (find-files #$%webserver-root-folder #:directories? #t))))))

(define-public %magento2-base-services
  (cons*
   (service redis-service-type)
   (simple-service 'set-permissions
                   activation-service-type
                   set-permissions-gexp)
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
	     (auto-upgrade? "#f")
             (extra-content
              "secure-file-priv='/tmp/'")))
   %base-services))

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
   (users
    (cons*
     (user-account
      (name "nazar")
      (comment "Nazar")
      (group "users"))
     %base-user-accounts))
   (services %magento2-base-services)))
