;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (guix modules base-operating-system)
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

(define-public base-operating-system

  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Uzhgorod")
    (host-name "goat")
    (keyboard-layout
     (keyboard-layout "us,ua" #:options '("grp:alt_shift_toggle" "ctrl:nocaps")))

    (users
     (cons*
      (user-account
       (name "nazar")
       (comment "Nazar")
       (group "users")
       (home-directory "/home/nazar")
       (supplementary-groups
	'("wheel" "netdev" "audio" "video" "netdev")))
      %base-user-accounts))

    (groups (cons (user-group (name "openvpn")) %base-groups))

    ;; Install bare-minimum system packages
    (packages (append (list
		       (specification->package "feh")
		       (specification->package "nss-certs")
		       (specification->package "pulseaudio")
		       (specification->package "git")
                       (specification->package "network-manager")
		       (specification->package "emacs")
		       (specification->package "emacs-exwm")
		       (specification->package "emacs-desktop-environment")
		       (specification->package "pinentry-emacs")
		       (specification->package "libnotify")
		       (specification->package "perl")
		       (specification->package "brightnessctl")
		       (specification->package "xrandr")
		       (specification->package "password-store")
		       (specification->package "gnupg")
		       (specification->package "ripgrep")
		       (specification->package "font-awesome")
		       (specification->package "polybar")
		       (specification->package "mu"))
                    %base-packages))

    (services
     (append
      (list
       (service tor-service-type)
       (service zram-device-service-type
		(zram-device-configuration
		 (size "8G")
		 (compression-algorithm 'zstd)))       
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
		 (sched-powersave-on-bat? #t))))))

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")
                 (keyboard-layout keyboard-layout)))

    (file-systems (cons*
                   (file-system
                     (mount-point "/")
                     (device "none")
                     (type "ext4")
                     (check? #f))
                   %base-file-systems))))
