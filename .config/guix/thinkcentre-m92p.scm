;; Indicate which modules to import to access the variables
;; used in this configuration.

(use-modules (gnu) (gnu system) (gnu services syncthing))
(use-service-modules cups desktop networking ssh xorg)

(define %my-base-services
  (modify-services
   %base-services
   (guix-service-type
    config =>
    (guix-configuration
     (inherit config)
     (authorized-keys
      (append (list (local-file "/etc/guix/signing-key.pub"))
              %default-authorized-guix-keys))))))

(define thinkcentre-server
  (operating-system
   (locale "en_US.utf8")
   (timezone "Europe/Kiev")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "thinkcentre")

   (users (cons* (user-account
                  (name "nazar")
                  (comment "Nazar")
                  (group "users")
                  (home-directory "/home/nazar")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                 %base-user-accounts))

   (sudoers-file
    (plain-file "sudoers"
                (string-append (plain-file-content %sudoers-specification)
                               (format #f "~a ALL = NOPASSWD: ALL~%"
                                       "nazar"))))
   (packages
    (append
     (list
      (specification->package "nss-certs")
      (specification->package "syncthing"))
     %base-packages))

   (services
    (append (list (service network-manager-service-type)
                  (service wpa-supplicant-service-type)
                  (service ntp-service-type)
                  (service openssh-service-type
                           (openssh-configuration
                            (x11-forwarding? #t)
                            (permit-root-login 'prohibit-password)
                            (authorized-keys
                             `(("nazar" ,(local-file "/home/nazar/openssh-keys/thinkcentre-server.pub"))))))
                  (service syncthing-service-type
                           (syncthing-configuration (user "nazar"))))

            %my-base-services))
   (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
   (swap-devices (list (swap-space
                        (target (uuid
                                 "109860d5-5542-4051-a5c3-6053d32df3a9")))))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "51de8c31-bdee-49e5-a638-44295222f022"
                                  'ext4))
                         (type "ext4"))
                        (file-system
                         (mount-point "/home")
                         (device (uuid
                                  "28578b89-367b-4d6e-a38a-4ee2fa8f88bc"
                                  'ext4))
                         (type "ext4")) %base-file-systems))))

(list (machine
       (operating-system thinkcentre-server)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       (host-name "192.168.88.12")
                       (system "x86_64-linux")
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC4Ulgiz4PYU/eA8lloeBo/2ccRaNINsV/3hpsNOOV97 root@(none)")
                       (build-locally? #f)
                       (identity "/home/nazar/.ssh/thinkcentre_server")
                       (user "nazar")))))
