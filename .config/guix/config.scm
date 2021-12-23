
(use-modules
 (gnu)
 (gnu services)
 (gnu services pm)
 (gnu services web)
 (gnu services mcron)
 (guix)
 (guix utils)
 (guix packages)
 (gnu packages php)
 (packages php72)
 (gnu services sound)
 (gnu packages autotools)
 (gnu services databases)
 (gnu services shepherd))
(use-service-modules base dbus desktop networking  xorg)

(define %wwwuser "nazar")
(define %wwwgroup "httpd")
(define %local-php-ini
  (plain-file "php.ini"
	      "memory_limit = 10G\n
               max_execution_time = 1800\n
               display_errors = on "))
(define %php-socket-path
  (string-append "/var/run/php" (version-major (package-version php)) "-fpm.sock"))

(define %my-desktop-services
  (modify-services
   %desktop-services
   (elogind-service-type config =>
			 (elogind-configuration
			  (inherit config)
			  (handle-power-key 'suspend)
			  (handle-lid-switch-external-power 'suspend)))))

(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Uzhgorod")
 (keyboard-layout
  (keyboard-layout "us,ua,ru"))
 (host-name "alienware")
 (hosts-file
  (plain-file "hosts"
	      (string-join
	       '("127.0.0.1 localhost"
		 "127.0.0.1 magentoi4.vg"
		 "127.0.0.1 second.magentoi4.vg"
		 ) "\n")))
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
    (specification->package "libnotify")
    (specification->package "xrandr")
    (specification->package "ripgrep")
    (specification->package "icecat")
    (specification->package "git")
    (specification->package "ispell")
    (specification->package "stow")
    (specification->package "node")
    (specification->package "w3m")
    (specification->package "httpd")
    (specification->package "mysql")
    (specification->package "scrot")
    (specification->package "file")
    (specification->package "rsync")
    (specification->package "notification-daemon")
    (specification->package "ungoogled-chromium")
    (specification->package "php72")
    (specification->package "openssh")
    (specification->package "alsa-utils")
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
    SetHandler \"proxy:unix:"%php-socket-path"|fcgi://localhost/\"
</FilesMatch>"))))))
    (service php-fpm-service-type
	     (php-fpm-configuration
	      (php php72)
	      (socket %php-socket-path)
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
			  "    Options -Indexes +FollowSymLinks +MultiViews"
			  "    AllowOverride All"
			  "    Require all granted"
			  "</Directory>")
			"\n")))
		     (httpd-virtualhost
		      "second.magentoi4.vg"
		      (list
		       (string-join
			'("ServerName second.magentoi4.vg"
			  "ServerAlias second.magentoi4.vg"
			  "DocumentRoot /home/nazar/Projects/i4/smith/magento"
			  "<Directory /home/nazar/Projects/i4/smith/magento>"
			  "    Options -Indexes +FollowSymLinks +MultiViews"
			  "    AllowOverride All"
			  "    Require all granted"
			  "</Directory>"
			  "SetEnv MAGE_RUN_CODE \"ES\""
                          "SetEnv MAGE_RUN_TYPE \"website\"")
			"\n")))))

    (service mysql-service-type
	     (mysql-configuration
	      (auto-upgrade? "#f")))

    (service tlp-service-type
	     (tlp-configuration
	      (tlp-default-mode "BAT")
              (energy-perf-policy-on-ac "performance")
              (energy-perf-policy-on-bat "normal")
              (wifi-pwr-on-ac? #f)
              (wifi-pwr-on-bat? #f)
              (usb-autosuspend? #t)
	      (cpu-scaling-governor-on-ac (list "performance"))
	      (sched-powersave-on-bat? #t)))

    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout keyboard-layout))))
   %my-desktop-services))
 (kernel-arguments '("snd_hda_intel.dmic_detect=0"))
 (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "4552043b-213f-4c56-90a9-a3dd3654b473")))
  (file-systems
    (cons* (file-system
             (mount-point "/home")
             (device
               (uuid "139bbb7b-0cec-4a81-97ca-074dc63d3d0c"
                     'ext4))
             (type "ext4"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "1B80-16D9" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "3716efbc-6c8f-4b85-b4d4-4060e73eb78a"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
