; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
 (gnu home)
 (guix store)
 (gnu packages chromium)
 (gnu packages gnuzilla)
 (gnu packages lxqt)
 (gnu packages compton)
 (gnu packages gnupg)
 (gnu packages fonts)
 (gnu packages pulseaudio)
 (gnu packages gnome)
 (gnu packages rust-apps)
 (gnu packages vpn)
 (gnu packages wm)
 (gnu packages golang)
 (gnu packages databases)
 (gnu packages xorg)
 (gnu packages compression)
 (gnu packages matrix)
 (gnu packages web-browsers)
 (gnu packages gstreamer)
 (gnu packages speech)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu services dbus)
 (gnu services xorg)
 (gnu home services shepherd)
 (gnu home services)
 (gnu home services desktop)
 (gnu home services gnupg)
 (gnu home services shells)
 (gnu home services desktop)
 (gnu packages shellutils)
 (gnu packages shells))

(home-environment
 (packages
  (list emacs-lsp-booster
	libnotify
	direnv
        sqls
        go
        unzip
	dunst
	pinentry-emacs
	font-google-noto-emoji
	font-awesome
	ungoogled-chromium
	pavucontrol
	compton
	openvpn
	icecat))

 (services
  (list
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)))

   (service home-dbus-service-type)
   (service home-bash-service-type
            (home-bash-configuration
	     (aliases '(
			("magento-cloud" . "/home/nazar/.magento-cloud/bin/magento-cloud")
                        ("php" . "php -d memory_limit=-1 $*")
			("build-burpee-container" . "guix system container ~/guix-system/.config/guix/containers/web-development/php/burpee.scm --network --share=$HOME/Projects/burpee=/srv/http --share=$HOME/guix-system --share=$HOME/.guix-profile -L ~/guix-system/.config/guix/containers/web-development/ --share=$HOME/db_dumps --share=$HOME/containers/mariadb-state=/var/lib/mysql")
                        ("build-sharkgaming-container" . "guix system container ~/guix-system/.config/guix/containers/web-development/php/sharkgaming.scm --network --share=$HOME/Projects/sharkgaming=/srv/http --share=$HOME/guix-system -L ~/guix-system/.config/guix/containers/web-development/ --share=$HOME/db_dumps --share=$HOME/containers/shrakgaming-mariadb-state=/var/lib/mysql")
                        ("build-lifetime-container" . "guix system container ~/guix-system/.config/guix/containers/web-development/php/lifetime.scm --network --share=$HOME/Projects/lifetime=/srv/http --share=$HOME/guix-system -L ~/guix-system/.config/guix/containers/web-development/ --share=/var/opensearch/config --share=$HOME/containers/lifetime-mariadb-state=/var/lib/mysql")
			("reconfigure-nazars-home" . "guix home reconfigure ~/guix-system/.config/guix/home-environments/nazars-home.scm")
			("reconfigure-x220" . "sudo guix system reconfigure -L ~/guix-system/.config/guix/hosts/modules/ ~/guix-system/.config/guix/hosts/thinkpad-x220.scm")
			("reconfigure-t440p" . "sudo guix system reconfigure -L ~/guix-system/.config/guix/hosts/modules/ ~/guix-system/.config/guix/hosts/thinkpad-t440p.scm")
			("package-burpee" . "guix package --manifest=/home/nazar/guix-system/.config/guix/manifests/burpee.scm --profile=/home/nazar/.guix-extra-profiles/burpee/")
                        ("package-lifetime" . "guix package --manifest=/home/nazar/guix-system/.config/guix/manifests/lifetime.scm --profile=/home/nazar/.guix-extra-profiles/lifetime/")
                        ("package-shark" . "guix package --manifest=/home/nazar/guix-system/.config/guix/manifests/sharkgaming.scm --profile=/home/nazar/.guix-extra-profiles/sharkgaming/")
                        ("depoy-thinkcentre-server" . "guix deploy ~/guix-system/.config/guix/hosts/thinkcentre-m92p.scm -L ~/guix-system/.config/guix/hosts/modules/ $*")
			))
	     (bashrc (list (plain-file "bashrc" "\
                       eval \"$(direnv hook bash)\"\n
                     ")))
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
                  export HISTFILE=$XDG_CACHE_HOME/.bash_history")))))
   )))
