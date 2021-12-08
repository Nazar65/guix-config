;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
	     (gnu services)
	     (gnu services web)
	     (gnu services shepherd))
(use-service-modules desktop networking ssh xorg)

(operating-system
 (locale "en_US.utf8")
 (timezone "Europe/Uzhgorod")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "alienware")
 (users (cons* (user-account
                (name "nazar")
                (comment "Nazar")
                (group "users")
                (home-directory "/home/nazar")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "emacs")
         (specification->package "emacs-exwm")
	 (specification->package "xrandr")
	 (specification->package "ripgrep")
	 (specification->package "icecat")
	 (specification->package "git")
	 (specification->package "stow")
	 (specification->package "httpd")
	 (specification->package "mysql")
	 (specification->package "php")
         (specification->package
          "emacs-desktop-environment")
         (specification->package "nss-certs"))
   %base-packages))
 (services
  (append
   (list (service tor-service-type)
	 (service httpd-service-type
		  (httpd-configuration
		   (config
		    (httpd-config-file
		     (server-name "www.example.com")
		     (document-root "/srv/http/www.example.com")))))
	 (simple-service 'www.example.com-server httpd-service-type
			 (list
			  (httpd-virtualhost
			   "*:80"
			   (list (string-join '("ServerName magentoi4.vg"
						"DocumentRoot /home/nazar/Projects/i4/smith/magento"
						"<Directory '/home/nazar/Projects/i4/smith/magento'>"
						"Options -Indexes +FolowSymLinks +MultiViews"
						"AllowOverride All"
						"Require all granted"
						"</Directory>")
					      "\n")))))
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (uuid "b79e5261-456f-4f3d-ad10-7a2c1487ef71")))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "9023101c-f316-460c-baae-14c5a7f54868"
                 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "62A0-D994" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/home")
          (device
           (uuid "ada31119-4fdb-4008-9373-e9255a69cacd"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
