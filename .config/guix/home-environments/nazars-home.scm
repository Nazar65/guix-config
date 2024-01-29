;; This "home-environment" file can be passed to 'guix home reconfigure'
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
 (gnu packages gnupg)
 (gnu packages fonts)
 (gnu packages pulseaudio)
 (gnu packages gnome)
 (gnu packages vpn)
 (gnu packages wm)
 (gnu packages xorg)
 (gnu packages matrix)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu home services shepherd)
 (gnu home services)
 (gnu home services desktop)
 (gnu home services gnupg)
 (gnu home services shells)
 (gnu packages shellutils)
 (gnu packages shells))

(home-environment
 (packages
  (list zsh-completions
	zsh-syntax-highlighting
	zsh
	libnotify
	dunst
	pinentry-emacs
	font-google-noto-emoji
	font-awesome
	ungoogled-chromium
	pavucontrol
	xcompmgr
	openvpn
	pantalaimon
	icecat))
 
 (services
  (list
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)))
   (service home-dbus-service-type)
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services
              (list
	       (shepherd-service
		(documentation "Run xcompmgr compositor™")
		(provision '(xcompmgr))
		(start #~(make-forkexec-constructor
                          (list #$(file-append xcompmgr "/bin/xcompmgr")
                                "-c"
				"-t-6"
				"-l-6"
				"-o.1")))
		(stop #~(make-kill-destructor))
		;; Needs gpg key to unlock.
		(auto-start? #t)
		(respawn? #f))
	       (shepherd-service
		(documentation "Run pantalaimon™")
		(provision '(pantalaimon))
		(start #~(make-forkexec-constructor
                          (list #$(file-append pantalaimon "/bin/pantalaimon"))))		
		(stop #~(make-kill-destructor))
		;; Needs gpg key to unlock.
		(auto-start? #t)
		(respawn? #f))
	       (shepherd-service
		(documentation "Run dunst-daemon™")
		(provision '(dunst-daemon))
		(start #~(make-forkexec-constructor
                          (list #$(file-append dunst "/bin/dunst"))
			  #:user "nazar"
			  #:log-file "/home/nazar/.config/shepherd/dunst-daemon.log"))
		(stop #~(make-kill-destructor))
		(auto-start? #t)
		(respawn? #f))))))
   (service home-bash-service-type
            (home-bash-configuration
	     (aliases '(
			("reconfigure-nazars-home" . "guix home reconfigure ~/guix-system/.config/guix/home-environments/nazars-home.scm")
			("reconfigure-x220" . "sudo guix system reconfigure -L ~/guix-system/.config/guix/hosts/modules/ ~/guix-system/.config/guix/hosts/thinkpad-x220.scm")
			("reconfigure-t440p" . "sudo guix system reconfigure -L ~/guix-system/.config/guix/hosts/modules/ ~/guix-system/.config/guix/hosts/thinkpad-t440p.scm")
			("package-burpee" . "guix package --manifest=/home/nazar/guix-system/.config/guix/manifests/burpee.scm --profile=/home/nazar/.guix-extra-profiles/burpee/")
			("package-reinders" . "guix package --manifest=/home/nazar/guix-system/.config/guix/manifests/reinders.scm --profile=/home/nazar/.guix-extra-profiles/reinders/")
			("ssh-burppe-production" . "ssh 6.ent-j5axkcqyhe6rg-production-vohbr3y@ssh.us-5.magento.cloud")
			("ssh-burppe-staging" . "ssh 6.ent-j5axkcqyhe6rg-staging-5em2ouy@ssh.us-5.magento.cloud")
			("ssh-reinders-production" . "ssh 1.ent-t45edsgrjclvi-production-vohbr3y@ssh.us-5.magento.cloud")
			("ssh-reinders-staging" . "ssh 1.ent-t45edsgrjclvi-staging-5em2ouy@ssh.us-5.magento.cloud")))
	     (bashrc (list (plain-file "bashrc" "\
                       eval \"$(direnv hook bash)\"\n
                     ")))
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
                  export HISTFILE=$XDG_CACHE_HOME/.bash_history")))))


   )))
