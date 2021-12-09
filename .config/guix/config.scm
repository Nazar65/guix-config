(use-modules
 (gnu)
 (gnu services)
 (gnu services web)
 (gnu services mcron)
 (gnu services databases)
 (gnu services shepherd))
(use-service-modules base desktop networking ssh xorg)

(define %wwwuser "nazar")
(define %wwwgroup "httpd")
(define %local-php-ini
  (plain-file "php.ini"
	      "memory_limit = 10G\n
               max_execution_time = 1800\n
               display_errors = on "))

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Uzhgorod")
  (keyboard-layout
   (keyboard-layout "us,ua,ru"))
  (host-name "alienware")
  (hosts-file
   (plain-file "hosts"
	       "127.0.0.1 localhost\n127.0.0.1 magentoi4.vg"))
  (users
   (cons*
    (user-account
     (name "nazar")
     (comment "Nazar")
     (group "users")
     (home-directory "/home/nazar")
     (supplementary-groups
      '("wheel" "netdev" "audio" "video" "httpd" "php-fpm")))
    %base-user-accounts))
  (packages
   (append
    (list
     (specification->package "emacs")
     (specification->package "emacs-exwm")
     (specification->package "xrandr")
     (specification->package "ripgrep")
     (specification->package "icecat")
     (specification->package "git")
     (specification->package "stow")
     (specification->package "httpd")
     (specification->package "mysql")
     (specification->package "php")
     (specification->package "ssh")
     (specification->package
      "emacs-desktop-environment")
     (specification->package "nss-certs"))
    %base-packages))
  (services
   (append
    (list
     (service tor-service-type)
     (service httpd-service-type
	      (httpd-configuration
	       (config
		(httpd-config-file
		 (user %wwwuser)
		 (modules
		  (cons*
		   (httpd-module
		    (name "rewrite_module")
		    (file "modules/mod_rewrite.so"))
		   (httpd-module
		    (name "proxy_module")
		    (file "modules/mod_proxy.so"))
		   (httpd-module
		    (name "proxy_fcgi_module")
		    (file "modules/mod_proxy_fcgi.so"))
		   (httpd-module
		    (name "proxy_http_module")
		    (file "modules/mod_proxy_http.so"))
		   %default-httpd-modules))
		 (extra-config
		  (list "\
<FilesMatch \\.php$>
    SetHandler \"proxy:unix:/var/run/php-fpm.sock|fcgi://localhost/\"
</FilesMatch>"))))))
     (service php-fpm-service-type
	      (php-fpm-configuration
	       (socket "/var/run/php-fpm.sock")
	       (user %wwwuser)
	       (group %wwwgroup)
	       (socket-user %wwwuser)
	       (socket-group %wwwgroup)
	       (display-errors "#t")
	       (php-ini-file %local-php-ini)))
     (simple-service 'magentoi4 httpd-service-type
		     (list
		      (httpd-virtualhost
		       "magentoi4.vg"
		       (list
			(string-join
			 '("ServerName magentoi4.vg"
			   "ServerAlias magentoi4.vg"
			   "DocumentRoot /home/nazar/Projects/i4/smith/magento"
			   "<Directory /home/nazar/Projects/i4/smith/magento>"
			   "Options -Indexes +FollowSymLinks +MultiViews"
			   "AllowOverride All"
			   "Require all granted"
			   "</Directory>")
			 "\n")))))
     (service mysql-service-type
	      (mysql-configuration
	       (auto-upgrade? "#t")))
     (set-xorg-configuration
      (xorg-configuration
       (keyboard-layout keyboard-layout))))
    %desktop-services))
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (target "/boot/efi")
    (keyboard-layout keyboard-layout)))
  (swap-devices
   (list
    (uuid "b79e5261-456f-4f3d-ad10-7a2c1487ef71")))
  (file-systems
   (cons*
    (file-system
      (mount-point "/")
      (device
       (uuid "9023101c-f316-460c-baae-14c5a7f54868"
             'ext4))
      (type "ext4"))
    (file-system
      (mount-point "/boot/efi")
      (device
       (uuid "62A0-D994" 'fat32))
      (type "vfat"))
    (file-system
      (mount-point "/home")
      (device
       (uuid "ada31119-4fdb-4008-9373-e9255a69cacd"
             'ext4))
      (type "ext4"))
    %base-file-systems)))
