;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
 (gnu home)
 (gnu packages gnome)
 (gnu packages xorg)
 (gnu packages matrix)
 (gnu packages)
 (gnu services)
 (guix gexp)
 (gnu home services shepherd)
 (gnu home services)
 (gnu home services shells)
 (gnu packages shellutils)
 (gnu packages shells))

(home-environment
 (packages
  (specifications->packages
   (list "zsh-completions"
	 "zsh-syntax-highlighting"
	 "zsh"
	 "direnv"
	 "ungoogled-chromium"
	 "notification-daemon"
	 "xcompmgr"
	 "pantalaimon"
	 "icecat")))
 (services
  (list
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services
              (list
	       (shepherd-service
		(documentation "Run notification-daemon™")
		(provision '(notification-daemon))
		(start #~(make-forkexec-constructor
                          (list #$(file-append notification-daemon "/libexec/notification-daemon")
                                "-r")))
		 (stop #~(make-kill-destructor))
		;; Needs gpg key to unlock.
		(auto-start? #t)
		(respawn? #f))
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
		(respawn? #f))))))

   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc (list
		     (local-file "../../.zshrc" "zshrc")
                     ;; This loads liquidprompt
                     (mixed-text-file "liquidprompt"
                                      "[[ $- = *i* ]] && source " liquidprompt "/share/liquidprompt/liquidprompt")
		     (mixed-text-file "powerline-theme"
                                           "source " liquidprompt "/share/liquidprompt/themes/powerline/powerline.theme")))))
   )))
