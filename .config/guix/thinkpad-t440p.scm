;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (guix thinkpad-t440p)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages mail)
  #:use-module (gnu system setuid)
  #:use-module (gnu services pm)
  #:use-module (gnu services web)
  #:use-module (gnu services sound)
  #:use-module (gnu services databases)
  #:use-module (gnu services mail)
  #:use-module (gnu services linux)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages))

(use-service-modules base dbus desktop networking xorg)

(define huawei-usb-modem-udev-rule
  (file->udev-rule "90-huawei-usb-modem-rule.rules"
		   (local-file "udev/60-usb_modeswitch.rules")))

(define %my-desktop-services
  (modify-services
      %desktop-services
    (udev-service-type
     config => (udev-configuration
                (inherit config)
	        (rules (append
		        (udev-configuration-rules config)
		        (list huawei-usb-modem-udev-rule)))))
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
   (keyboard-layout "us,ua" #:options '("grp:alt_shift_toggle" "ctrl:nocaps")))
  (host-name "t440p")

  (essential-services
   (modify-services
       (operating-system-default-essential-services this-operating-system)
     (hosts-service-type config => (list
                                    (host "127.0.0.1" "localhost")
                                    (host "::1"       "localhost")
                                    (host "127.0.0.1" "development.local")))))

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
     (specification->package "emacs-desktop-environment")
     (specification->package "pinentry-emacs")
     (specification->package "perl")
     (specification->package "libnotify")
     (specification->package "brightnessctl")
     (specification->package "xrandr")
     (specification->package "pavucontrol")
     (specification->package "password-store")
     (specification->package "gnupg")
     (specification->package "ripgrep")
     (specification->package "pantalaimon")
     (specification->package "openvpn")
     (specification->package "font-awesome")
     (specification->package "polybar")
     (specification->package "clojure")
     (specification->package "clojure-tools")
     (specification->package "openjdk")
     (specification->package "mu")
     (specification->package "feh")
     (specification->package "imagemagick")
     (specification->package "gifsicle")
     (specification->package "offlineimap3")
     (specification->package "isync")
     (specification->package "msmtp")
     (specification->package "ispell")
     (specification->package "stow")
     (specification->package "sendmail")
     (specification->package "w3m")
     (specification->package "curl")
     (specification->package "git")
     (specification->package "network-manager")
     (specification->package "scrot")
     (specification->package "file")
     (specification->package "tdlib")
     (specification->package "opensmtpd")
     (specification->package "setxkbmap")
     (specification->package "pulseaudio")
     (specification->package "rsync")
     (specification->package "binutils")
     (specification->package "openssh")
     (specification->package "usb-modeswitch")
     (specification->package "alsa-utils")
     (specification->package "nss-certs")
     (specification->package "cifs-utils")
     (specification->package "nfs-utils")
     (specification->package "xinput"))
    %base-packages))

  (services
   (append
    (list
     (service tor-service-type)
     (service zram-device-service-type
	      (zram-device-configuration
	       (size "8G")
	       (compression-algorithm 'zstd)))

     (service opensmtpd-service-type
              (opensmtpd-configuration
               (config-file (local-file "my-smtpd.conf"))))

     (service ladspa-service-type
              (ladspa-configuration (plugins (list swh-plugins))))

     (service bluetooth-service-type
              (bluetooth-configuration
               (auto-enable? #t)))

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
                              "Identifier \"Synaptics TM3053-003\""
                              "Driver \"libinput\""
                              "MatchIsTouchpad \"on\""
			      "Option \"Ignore\" \"on\""
                              "EndSection" "\n"
			      "Section \"InputClass\""
			      "Identifier \"TPPS/2 IBM TrackPoint\""
			      "Option \"AccelSpeed\" \"1\""
			      "EndSection") "\n"
			      )))
       (keyboard-layout keyboard-layout))))
    %my-desktop-services))

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "c385eefa-1cb4-40e3-8262-ca744233163f")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "16e1d446-be55-49de-aaf4-7611ec5be37e"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
