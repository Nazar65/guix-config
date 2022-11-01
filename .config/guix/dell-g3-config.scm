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
 (gnu packages usb-modeswitch)
 (gnu services databases)
 (gnu services pm)
 (gnu services web)
 (gnu services mcron)
 (gnu system setuid)
 (gnu services sound)
 (gnu services docker)
 (gnu services mail)
 (gnu services shepherd))
(use-service-modules base dbus desktop databases networking xorg)

(define %wwwuser "nazar")
(define %wwwgroup "httpd")
(define %local-php-ini "/home/nazar/.config/php/php.ini")
(define %php-socket-path
  (string-append "/var/run/php"
		 (version-major
		  (package-version php74))
		 "-fpm.sock"))

(define (simple-https-website domain directory documentRoot)
  (list (httpd-virtualhost
         "*:443"
         (list "
                ServerName " domain "
                ServerName " domain "
                DocumentRoot " documentRoot "
                <Directory " directory "/>
                    Options -Indexes +FollowSymLinks +MultiViews
                    AllowOverride All
                    Require all granted
                </Directory>
                SSLEngine on
                SSLCertificateFile /home/nazar/.dotfiles/.config/guix/certs/" domain "/mysitename.crt
                SSLCertificateKeyFile /home/nazar/.dotfiles/.config/guix/certs/" domain "/mysitename.key\n"))))


(define huawei-usb-modem-udev-rule
  (file->udev-rule "90-huawei-usb-modem-rule.rules"
             (local-file "udev/60-usb_modeswitch.rules")))

(define disable-unsued-devices-rule
  (file->udev-rule "90-disable-invidia-devices.rules"
             (local-file "udev/70-disable-nvidia-devices.rules")))

(define %my-desktop-services
  (modify-services
      %desktop-services
    (udev-service-type
     config => (udev-configuration
                (inherit config)
		(rules (append
			(udev-configuration-rules config)
			(list huawei-usb-modem-udev-rule
			      disable-unsued-devices-rule)))))
    (pulseaudio-service-type
     config =>
     (pulseaudio-configuration
      (inherit config)))
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
      '("wheel" "netdev" "audio" "video")))
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
     (specification->package "pavucontrol")
     (specification->package "password-store")
     (specification->package "gst-plugins-ugly")
     (specification->package "gst-plugins-good")
     (specification->package "gst-plugins-base")
     (specification->package "gst-plugins-bad")
     (specification->package "gst-libav")
     (specification->package "gnupg")
     (specification->package "pinentry")
     (specification->package "ripgrep")
     (specification->package "icecat")
     (specification->package "nyxt-3")
     (specification->package "playerctl")
     (specification->package "git")
     (specification->package "mu")
     (specification->package "docker")
     (specification->package "docker-cli")
     (specification->package "imagemagick")
     (specification->package "usb-modeswitch")
     (specification->package "gifsicle")
     (specification->package "offlineimap")
     (specification->package "isync")
     (specification->package "msmtp")
     (specification->package "the-silver-searcher")
     (specification->package "ispell")
     (specification->package "stow")
     (specification->package "node")
     (specification->package "sendmail")
     (specification->package "w3m")
     (specification->package "curl")
     (specification->package "git")
     (specification->package "network-manager")
     (specification->package "httpd")
     (specification->package "scrot")
     (specification->package "file")
     (specification->package "ly")
     (specification->package "bluez")
     (specification->package "speedtest-cli")
     (specification->package "font-gnu-freefont")
     (specification->package "font-gnu-unifont")
     (specification->package "tdlib")
     (specification->package "make")
     (specification->package "gcc")
     (specification->package "opensmtpd")
     (specification->package "setxkbmap")
     (specification->package "pulseaudio-equalizer")
     (specification->package "rsync")
     (specification->package "autoconf")
     (specification->package "notification-daemon")
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
     (service docker-service-type)
     (service redis-service-type)
     (service httpd-service-type
	      (httpd-configuration
	       (config
		(httpd-config-file
		 (user %wwwuser)
		 (listen '("443"))
		 (modules
		  (cons*
		   (httpd-module
		    (name "rewrite_module")
		    (file "modules/mod_rewrite.so"))
		   (httpd-module
		    (name "ssl_module")
		    (file "modules/mod_ssl.so"))
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
	       (user %wwwuser)
	       (group %wwwgroup)
	       (socket %php-socket-path)
	       (socket-user %wwwuser)
	       (socket-group %wwwgroup)
	       (display-errors "#t")
	       (php-ini-file %local-php-ini)))

     (simple-service 'alienware.ai httpd-service-type
                     (simple-https-website "alienware.ai"
                                           "/home/nazar/srv"
					   "/home/nazar/srv/pub"))
     (service mysql-service-type
	      (mysql-configuration
               (socket "/run/mysqld/mysqld.sock")
               (extra-content (string-join
                               '("datadir=/warehouse/databases") "\n"))
	       (auto-upgrade? "#f")))

     (service opensmtpd-service-type
              (opensmtpd-configuration
               (config-file (local-file "my-smtpd.conf"))))

     (service ladspa-service-type
              (ladspa-configuration (plugins (list swh-plugins))))

     (bluetooth-service #:auto-enable? #t)
     (service tlp-service-type
	      (tlp-configuration
	       (cpu-boost-on-ac? #t)
	       (tlp-default-mode "BAT")
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
   '("snd-intel-dspcfg.dsp_driver=1"
     "modprobe.blacklist=nouveau"
     "modprobe.blacklist=snd_hda_codec_hdmi"))
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
      (mount-point "/warehouse")
      (device
       (uuid "fd919fec-008f-42b6-a480-2395bc14709d"
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
