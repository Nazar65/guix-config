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
  #:use-module (gasket-dkms)
  #:use-module (gnu services syncthing)
  #:use-module (gnu system)
  #:use-module (gnu machine)
  #:use-module (gnu services linux)
  #:use-module (gnu packages linux)
  #:use-module (gnu machine ssh)
  #:use-module (gnu services ssh)
  #:use-module (gnu services dbus)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (guix packages))


(define %frigate-packages
  (append (list
	   (specification->package "syncthing")
           (specification->package "gasket-dkms")
	   (specification->package "rsync"))
          %server-bare-minimum-packages))

(define %google-coral-tpu-udev-rule
  (file->udev-rule "90-google-coral-tpu-udev.rules"
		   (local-file "./udev/90-google-coral-tpu-udev.rules")))

(define %my-server-services
  (modify-services
      %server-base-services
    (udev-service-type
     config => (udev-configuration
                (inherit config)
	        (rules (append
		        (udev-configuration-rules config)
		        (list %google-coral-tpu-udev-rule)))))))

(define %default-extra-linux-options ((@@ (gnu packages linux) default-extra-linux-options) linux-libre-version))

(define linux-libre-apex-driver
  ((@@ (gnu packages linux) make-linux-libre*)
   (@@ (gnu packages linux) linux-libre-version)
   (@@ (gnu packages linux) linux-libre-gnu-revision)
   (@@ (gnu packages linux) linux-libre-source)
   '("x86_64-linux")
   #:extra-version "gasket-apex-driver"
   #:configuration-file (@@ (gnu packages linux) kernel-config)
   #:extra-options (append
                    `(("CONFIG_STAGING_APEX_DRIVER" . #t))
                    %default-extra-linux-options)))
 
(define %thinkcentre-server
  (operating-system
   (inherit server-operating-system)
   (packages %frigate-packages)
   (kernel linux-libre-apex-driver)
   (kernel-loadable-modules (list gasket-dkms))

   (users
    (cons*
     (user-account
      (name "goat")
      (comment "goat")
      (group "users")
      (password (crypt "InitialPassword!" "123123q"))
      (home-directory "/home/goat")
      (supplementary-groups
       '("wheel" "netdev" "apex")))
     %base-user-accounts))

   (groups (cons (user-group (name "apex")) %base-groups))

   (services
    (append
     (list
      (service elogind-service-type)
      (service containerd-service-type)
      (service network-manager-service-type)
      (service wpa-supplicant-service-type)
      (service dbus-root-service-type)
      (simple-service 'add-extra-modules kernel-module-loader-service-type
                      '("apex"
                        "gasket"))

      (service oci-container-service-type
               (list
                (oci-container-configuration
                 (image "ghcr.io/blakeblackshear/frigate:0.14.1")
                 (provision "frigate")
                 (network "host")
                 (ports
                  '(("8971" . "8971")
                    ("8554" . "8554")))
                 (volumes
                  '("/home/goat/frigate/config:/config"
                    "/etc/localtime:/etc/localtime:ro"
                    "/home/goat/frigate/storage:/media/frigate"))
                 (extra-arguments
                  (list
                   (string-append "--privileged")
                   (string-append "--device=/dev/apex_0")
                   (string-append "--shm-size=102m"))))))
      (service openssh-service-type
               (openssh-configuration
                (x11-forwarding? #t)
                (password-authentication? #f)
                (public-key-authentication? #t)
                (authorized-keys
                 `(("goat" ,(local-file "/home/nazar/.ssh/thinkcentre-server.pub"))))))
      (service syncthing-service-type
               (syncthing-configuration (user "goat"))))
     %my-server-services))


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
		        %base-file-systems))))

(list (machine
       (operating-system %thinkcentre-server)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "192.168.88.12")
                       (system "x86_64-linux")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIArhPPFuBiDSziOPh4LKs0Lxe76x6p00iJNNAJsI7iJQ nazar@t440p")
                       (build-locally? #f)
                       (identity "/home/nazar/.ssh/thinkcentre-server")
                       (user "goat")))))
