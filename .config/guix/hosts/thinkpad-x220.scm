;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (hosts thinkpad-x220)
  #:use-module (base)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (gnu services sound)
  #:use-module (gnu services mail)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages mail)
  #:use-module (guix packages))

(use-service-modules base dbus desktop networking xorg)

(define %my-desktop-services
  (modify-services
      %base-desktop-services
    (elogind-service-type
     config =>
     (elogind-configuration
      (inherit config)
      (handle-power-key 'suspend)
      (handle-lid-switch-external-power 'suspend)))))

(operating-system
 (inherit base-operating-system)
  (keyboard-layout
   (keyboard-layout "us,ua,ru" #:options '("grp:alt_shift_toggle" "ctrl:nocaps")))
  (host-name "x220")

  (essential-services
   (modify-services
       (operating-system-default-essential-services this-operating-system)
     (hosts-service-type config => (list
                                    (host "127.0.0.1" "localhost")
                                    (host "::1"       "localhost")
                                    (host "127.0.0.1" "development.local")))))

  (packages
   (append
    (list
     (specification->package "offlineimap3")
     (specification->package "clojure")
     (specification->package "xl2tpd")
     (specification->package "clojure-tools")
     (specification->package "openjdk")
     (specification->package "imagemagick")
     (specification->package "gifsicle")
     (specification->package "isync")
     (specification->package "msmtp")
     (specification->package "ispell")
     (specification->package "stow")
     (specification->package "w3m")
     (specification->package "curl")
     (specification->package "scrot")
     (specification->package "file")
     (specification->package "tdlib")
     (specification->package "opensmtpd")
     (specification->package "setxkbmap")
     (specification->package "rsync")
     (specification->package "binutils")
     (specification->package "openssh")
     (specification->package "direnv")
     (specification->package "usb-modeswitch")
     (specification->package "alsa-utils")
     (specification->package "xinput"))
    %bare-minimum-packages))

  (services
   (append
    (list    
     (set-xorg-configuration
      (xorg-configuration
       (extra-config (list (string-join
	                    '("Section \"InputClass\""
                                "Identifier \"SynPS/2 Synaptics TouchPad\""
                                "Driver \"libinput\""
                                "MatchIsTouchpad \"on\""
			        "Option \"Ignore\" \"on\""
                              "EndSection" "\n"
			       "Section \"InputClass\""
				   "Identifier \"TPPS/2 IBM TrackPoint\""
				   "Option \"AccelSpeed\" \"1\""
			       "EndSection") "\n"
			      )))
       (keyboard-layout %keyboard-layout))))
    %base-desktop-services))
  
  (kernel-arguments
   '("intel_idle.max_cstate=4"))
  
(swap-devices (list (swap-space
                        (target (uuid
                                 "7b7de6f2-7b46-4f7e-8077-bbe6555a3246")))))

 (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "3fe9f4ee-5e8d-4d69-a0b4-e8076ce8d48f"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
