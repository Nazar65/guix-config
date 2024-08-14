;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (server-base)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu system)
  #:use-module (gnu services pm)
  #:use-module (gnu services networking)
  #:use-module (gnu services linux)
  #:use-module (gnu services docker)
  #:use-module (gnu services ssh)
  #:use-module (guix packages))

(define-public %server-bare-minimum-packages
  (append (list
	   (specification->package "nss-certs")
	   (specification->package "git")
           (specification->package "docker-compose")
           (specification->package "network-manager")
	   (specification->package "emacs")
	   (specification->package "perl")
	   (specification->package "gnupg")
	   (specification->package "ripgrep"))
          %base-packages))

(define-public %server-keyboard-layout
  (keyboard-layout "us,ua"
		   #:model "thinkpad"
		   #:options '("grp:alt_shift_toggle" "ctrl:nocaps")))

(define-public %server-modified-services
  (modify-services
   %base-services
   (guix-service-type
    config =>
    (guix-configuration
     (inherit config)
     (authorized-keys
      (append (list (local-file "/etc/guix/signing-key.pub"))
              %default-authorized-guix-keys))))))

(define-public %server-base-services
  (append
   (list
    (service tor-service-type)
    (service docker-service-type)
    (service zram-device-service-type
	     (zram-device-configuration
	      (size "8G")
	      (compression-algorithm 'zstd))))
   %server-modified-services))


(define-public server-operating-system
  (operating-system
    (locale "en_US.utf8")
    (timezone "Europe/Uzhgorod")
    (host-name "goat")
    (keyboard-layout %server-keyboard-layout)
    (sudoers-file
    (plain-file "sudoers"
                (string-append (plain-file-content %sudoers-specification)
                               (format #f "~a ALL = NOPASSWD: ALL~%"
                                       "goat"))))
    (users
     (cons*
      (user-account
       (name "goat")
       (comment "Goat")
       (group "users")
       (password (crypt "InitialPassword!" "123123q"))
       (home-directory "/home/goat")
       (supplementary-groups
	'("wheel" "netdev" "audio" "video")))
      %base-user-accounts))

    (packages %server-bare-minimum-packages)
    (services %server-base-services)

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

