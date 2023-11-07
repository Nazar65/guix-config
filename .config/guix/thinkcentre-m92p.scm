;; Indicate which modules to import to access the variables
;; used in this configuration.

(use-modules (gnu)
              (ice-9 match)
             (ice-9 popen)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-26)
	     (gnu system)
	     (gnu services syncthing)
	     (gnu services samba)
	     (gnu services web)
	     (gnu services cgit))


(use-package-modules version-control)
(use-service-modules cups desktop cgit networking ssh xorg)

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

(define cgit-nginx-configuration
  (nginx-server-configuration
   (root cgit)
   (server-name '("192.168.88.12"))
   (listen '("80"))
   (locations
    (list
     (nginx-location-configuration ;; So CSS & co. are found
      (uri "~ ^/share/cgit/cgit.css")
      (body `(("root /srv/git/css;"))))
     (nginx-location-configuration
      (uri "@cgit")
      (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
	      "fastcgi_param PATH_INFO $uri;"
	      "fastcgi_param QUERY_STRING $args;"
	      "fastcgi_param HTTP_HOST $server_name;"
	      "fastcgi_pass 127.0.0.1:9000;")))))
   (try-files (list "$uri" "@cgit"))))

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
                 (user-account
                  (name "git")
                  (comment "Git server user")
                  (group "users")
                  (home-directory "/home/git")
                  (supplementary-groups '("wheel" "netdev")))
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
      (specification->package "syncthing")
      (specification->package "rsync")
      (specification->package "git")
      (specification->package "cgit")
      (specification->package "nginx")
      (specification->package "samba"))
     %base-packages))

   (services
    (append (list (service network-manager-service-type)
                  (service wpa-supplicant-service-type)
                  (service ntp-service-type)
		  (service cgit-service-type
			   (cgit-configuration
                            (root-desc "CGIT Browser")
                            (root-title "Klovanych's Git Hosting Sapce")
			    (repository-directory "/srv/git/repositories")
			    (enable-git-config? #t)
                            (side-by-side-diffs? #t)
			    (enable-index-links? #t)
			    (enable-html-serving? #t)
			    (enable-commit-graph? #t)
			    (enable-log-filecount? #t)
			    (enable-log-linecount? #t)
			    (readme ":README.md")
			    (remove-suffix? #t)
			    (section-from-path 1)
                            (source-filter
                             (program-file
                              "cgit-syntax-highlighting"
                              #~(apply execl (string-append
                                              #$cgit "/lib/cgit/filters/syntax-highlighting.py")
                                       (command-line))))
                            (about-filter
                             (program-file
                              "cgit-about-formatting"
                              #~(apply execl (string-append
                                              #$cgit "/lib/cgit/filters/about-formatting.sh")
                                       (command-line))))
			   (nginx
			    (list
			     cgit-nginx-configuration))))
                  (service openssh-service-type
                           (openssh-configuration
                            (x11-forwarding? #t)
                            (password-authentication? #f)
                            (public-key-authentication? #t)
                            (authorized-keys
                             `(("nazar" ,(local-file "/home/nazar/.ssh/thinkcentre-server.pub"))
                               ("git" ,(local-file "/home/nazar/.ssh/git-server.pub"))))))
                  (service syncthing-service-type
                           (syncthing-configuration (user "nazar")))
                  (service samba-service-type
                           (samba-configuration
                            (enable-smbd? #t)
                            (config-file (plain-file "smb.conf" "\
[global]
   map to guest = Bad User
   logging = syslog@1
   workgroup = WORKGROUP
   server string = Samba Server
   server role = standalone server
   security=user

[accounting]
    max connections = 4

[shared_storage]
   path = /home/nazar/shared_storage
   browsable = yes
   writable = yes
   read only = no
   valid users = nazar
   force user = nazar
   force group = users\n")))))

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
                       (identity "/home/nazar/.ssh/thinkcentre-server")
                       (user "nazar")))))
