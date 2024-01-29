;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (base)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages admin)
  #:use-module (gnu services pm)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module (gnu services desktop)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu services databases)
  #:use-module (gnu services linux)
  #:use-module (guix packages))

(define-public %keyboard-layout
  (keyboard-layout "us,ua"
		   #:model "thinkpad"
		   #:options '("grp:alt_shift_toggle" "ctrl:nocaps")))

(define-public %base-desktop-services
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
	      (sched-powersave-on-bat? #t))))
   %desktop-services))

(define-public %bare-minimum-packages
  (append (list
	   (specification->package "feh")
	   (specification->package "nss-certs")
	   (specification->package "pulseaudio")
	   (specification->package "git")
           (specification->package "network-manager")
	   (specification->package "emacs")
	   (specification->package "emacs-exwm")
	   (specification->package "emacs-desktop-environment")
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

(define-public base-operating-system
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Uzhgorod")
    (host-name "goat")
    (keyboard-layout %keyboard-layout)

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

    (packages %bare-minimum-packages)
    (services %base-desktop-services)

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")
                 (keyboard-layout keyboard-layout)))

    (file-systems (cons*
                   (file-system
		     (device "none")
                     (mount-point "/")
                     (type "ext4")
                     (check? #f))
                   %base-file-systems))))
