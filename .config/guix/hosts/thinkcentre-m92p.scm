;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (hosts thinkcentre-m92p)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (server-base)
  #:use-module (gnu services syncthing)
  #:use-module (gnu system)
  #:use-module (gnu services ssh)
  #:use-module (gnu services dbus)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (guix packages))

(define %frigate-packages
  (append (list
	   (specification->package "syncthing")
	   (specification->package "rsync"))
          %server-bare-minimum-packages))

(operating-system
 (inherit server-operating-system)
 (packages %frigate-packages) 
 (services
  (append
   (list
    (service elogind-service-type)
    (service containerd-service-type)
    (service network-manager-service-type)
    (service wpa-supplicant-service-type)
    (service dbus-root-service-type)
    (service oci-container-service-type
             (list
              (oci-container-configuration
               (image "ghcr.io/blakeblackshear/frigate:dev-05bc383")
               (provision "frigate")
               (network "host")
               (ports
                '(("8971" . "8971")
                  ("8554" . "8554")))
               (volumes
                '("/home/goat/frigate/config:/config"
                  "/home/goat/frigate/storage:/media/frigate")))))
    (service openssh-service-type
             (openssh-configuration
              (x11-forwarding? #t)
              (password-authentication? #f)
              (public-key-authentication? #t)
              (authorized-keys
               `(("goat" ,(local-file "/home/goat/.ssh/thinkcentre-server.pub"))))))
    (service syncthing-service-type
             (syncthing-configuration (user "goat"))))
   %server-base-services))


 (swap-devices (list (swap-space
                      (target (uuid
                               "04ba80bc-d72b-49ec-ba2e-778db698a25a")))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (device (uuid "a215ade6-018c-49c2-b653-467b15656836" 'ext4))
		       (mount-point "/")
		       (type "ext4")
		       (check? #f))
		      %base-file-systems)))
