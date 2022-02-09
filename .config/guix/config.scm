(use-modules
 (gnu)
 (guix)
 (guix utils)
 (guix packages)
 (packages composer)
 (packages php74)
 (gnu packages audio)
 (gnu packages pulseaudio)
 (gnu packages autotools)
 (gnu packages admin)
 (gnu packages mail)
 (gnu services)
 (gnu services pm)
 (gnu services web)
 (gnu services mcron)
 (gnu system setuid)
 (gnu services sound)
 (gnu services databases)
 (gnu services mail)
 (gnu services shepherd))
(use-service-modules base dbus desktop networking  xorg)

(define %wwwuser "nazar")
(define %wwwgroup "httpd")
(define %local-php-ini "/home/nazar/.config/php/php.ini")
(define %php-socket-path
  (string-append "/var/run/php"
		 (version-major
		  (package-version php74))
		 "-fpm.sock"))

(define %my-desktop-services
  (modify-services
      %desktop-services
    (elogind-service-type
     config =>
     (elogind-configuration
      (inherit config)
      (handle-power-key 'suspend)
      (handle-lid-switch-external-power 'suspend)))))

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Uzhgorod")
  (keyboard-layout
   (keyboard-layout "us,ua,ru" #:options '("grp:alt_shift_toggle")))
  (host-name "alienware")
  (hosts-file
   (plain-file
    "hosts"
    (string-join
     '("127.0.0.1 localhost"
       "127.0.0.1 alienware.ai")
     "\n")))
  (groups (cons (user-group (name "openvpn")) %base-groups))
  (users
   (cons*
    (user-account
     (name "nazar")
     (comment "Nazar")
     (group "users")
     (home-directory "/home/nazar")
     (supplementary-groups
      '("wheel" "netdev" "audio" "video" "httpd" "php-fpm")))
    (user-account
     (name "openvpn")
     (group "openvpn")
     (system? #t)
     (comment "openvpn user")
     (home-directory "/var/empty")
     (shell (file-append shadow "/sbin/nologin")))
    %base-user-accounts))
  (setuid-programs
   (append
    (list (setuid-program
           (program (file-append opensmtpd "/sbin/smtpctl"))
	   (setuid? #f)
	   (setgid? #t)
	   (user "root")
	   (group "smtpq"))
	  (setuid-program
           (program (file-append opensmtpd "/sbin/sendmail"))
	   (setuid? #f)
	   (setgid? #t)
	   (user "root")
	   (group "smtpq")))
    %setuid-programs))
  (packages
   (append
    (list
     (specification->package "emacs")
     (specification->package "emacs-exwm")
     (specification->package "libnotify")
     (specification->package "brightnessctl")
     (specification->package "xrandr")
     (specification->package "ripgrep")
     (specification->package "icecat")
     (specification->package "playerctl")
     (specification->package "git")
     (specification->package "ispell")
     (specification->package "stow")
     (specification->package "node")
     (specification->package "sendmail")
     (specification->package "w3m")
     (specification->package "curl")
     (specification->package "git")
     (specification->package "httpd")
     (specification->package "mysql")
     (specification->package "scrot")
     (specification->package "file")
     (specification->package "ly")
     (specification->package "opensmtpd")
     (specification->package "setxkbmap")
     (specification->package "pulseaudio-equalizer")
     (specification->package "rsync")
     (specification->package "autoconf")
     (specification->package "notification-daemon")
     (specification->package "ungoogled-chromium")
     (specification->package "php74")
     (specification->package "elasticsearch")
     (specification->package "phpfixer")
     (specification->package "xdebug3")
     (specification->package "composer")
     (specification->package "openssh")
     (specification->package "redis")
     (specification->package "alsa-utils")
     (specification->package "emacs-desktop-environment")
     (specification->package "nss-certs"))
    %base-packages))
  (services
   (append
    (list
     (service tor-service-type)
     (service redis-service-type)
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
	       (php php74)
	       (socket %php-socket-path)
	       (user %wwwuser)
	       (group %wwwgroup)
	       (socket-user %wwwuser)
	       (socket-group %wwwgroup)
	       (display-errors "#t")
	       (php-ini-file %local-php-ini)))
     (simple-service
      'magentoi4 httpd-service-type
      (list
       (httpd-virtualhost
	"alienware.ai"
	(list
	 (string-join
	  '("ServerName alienware.ai"
	    "ServerAlias alienware.ai"
	    "DocumentRoot /home/nazar/srv/pub"
	    "<Directory /home/nazar/srv>"
	    "    Options -Indexes +FollowSymLinks +MultiViews"
	    "    AllowOverride All"
	    "    Require all granted"
	    "</Directory>")
	  "\n")))))

     (service mysql-service-type
	      (mysql-configuration
	       (auto-upgrade? "#f")))

     (service opensmtpd-service-type
              (opensmtpd-configuration
               (config-file (local-file "my-smtpd.conf"))))

     (service ladspa-service-type
              (ladspa-configuration (plugins (list swh-plugins))))

     (service tlp-service-type
	      (tlp-configuration
	       (tlp-default-mode "BAT")
	       (energy-perf-policy-on-ac "performance")
	       (energy-perf-policy-on-bat "normal")
	       (wifi-pwr-on-ac? #f)
	       (wifi-pwr-on-bat? #f)
	       (usb-autosuspend? #t)
	       (cpu-scaling-governor-on-ac
		(list "performance"))
	       (sched-powersave-on-bat? #t)))
     (set-xorg-configuration
      (xorg-configuration
       (extra-config (list (string-join
	  '("Section \"InputClass\""
            "Identifier \"touchpad\""
            "Driver \"libinput\""
            "MatchIsTouchpad \"on\""
            "Option \"DisableWhileTyping\" \"on\""
            "Option \"Tapping\" \"1\""
            "Option \"NaturalScrolling\" \"1\""
            "Option \"Emulate3Buttons\" \"yes\""
            "EndSection") "\n")))
       (keyboard-layout keyboard-layout))))
    %my-desktop-services))
  (kernel-arguments
   '("snd_hda_intel.dmic_detect=0"))
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (target "/boot/efi")
    (keyboard-layout keyboard-layout)))
  (swap-devices
   (list
    (uuid "4552043b-213f-4c56-90a9-a3dd3654b473")))
  (file-systems
   (cons*
    (file-system
      (mount-point "/home")
      (device
       (uuid "139bbb7b-0cec-4a81-97ca-074dc63d3d0c"
             'ext4))
      (type "ext4"))
    (file-system
      (mount-point "/boot/efi")
      (device
       (uuid "1B80-16D9" 'fat32))
      (type "vfat"))
    (file-system
      (mount-point "/")
      (device
       (uuid "3716efbc-6c8f-4b85-b4d4-4060e73eb78a"
             'ext4))
      (type "ext4"))
    %base-file-systems)))
