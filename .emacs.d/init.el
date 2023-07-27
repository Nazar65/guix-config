;;; package ---
;;; Commentary:
;;; Code:
(defvar bootstrap-version)

(setq straight-check-for-modifications nil)
(setq vc-follow-symlinks t)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package system-packages
  :custom
  system-packages-noconfirm t)

(use-package use-package-core
  :straight (:type built-in)
  :custom
  (use-package-verbose 0)
  (use-package-compute-statistics 0)
  (use-package-always-defer t)
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

(use-package use-package-ensure-system-package)

(use-package em-tramp
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  :straight (:type built-in))

(use-package desktop-environment
  :straight (:type git :host github :repo "DamienCassou/desktop-environment")
  :after exwm
  :init
  (desktop-environment-mode))

(use-package auto-complete
  :straight (:type git :host github :repo "auto-complete/auto-complete")
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB"))

(use-package password-store
  :straight t)

(use-package ztree
  :straight (:type git :host github :repo "fourier/ztree"))

(use-package bluetooth
  :straight (:type git :host github :repo "emacs-straight/bluetooth"))

(use-package restclient
  :straight (:type git :host github :repo "pashky/restclient.el"))

(use-package company-restclient
  :straight (:type git :host github :repo "iquiw/company-restclient")
  :config (add-to-list 'company-backends 'company-restclient))

(use-package emacs
  :straight (:type built-in)
  :bind (("M-f"     . 'forward-to-word)
         ("M-b"     . 'backward-to-word)
         ("C-!"     . 'kill-this-buffer)
         ("C-<f5>"  . display-line-numbers-mode)
         ("C-c d"   . 'local/duplicate-start-of-line-or-region)
         ([remap dabbrev-expand] . 'hippie-expand))
  :mode (("\\.[Ee][Ll]\\'" . emacs-lisp-mode))
  :hook ((before-save . delete-trailing-whitespaces)
         (emacs-startup . (lambda ()
                            (let ((startup-time (float-time (time-subtract after-init-time before-init-time))))
                              (message "Emacs ready in %.2f seconds with %d garbage collections." startup-time gcs-done)))))
  :init
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0)
  (display-time-mode 1)
  (display-battery-mode 1)
  :custom
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq file-name-handler-alist nil)
  (setq indent-tabs-mode nil)
  (nsm-settings-file "~/.emacs.d/network-security.data")
  (history-delete-duplicates t)
  (display-time-default-load-average nil)
  (history-length 600)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (setq display-time-mail-string "")
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (setq undo-limit 800000)
  (setq undo-strong-limit 12000000)
  (setq undo-outer-limit 120000000)
  (setq password-cache t)
  (setq password-cache-expiry 3600)
  (setq nxml-child-indent 4 nxml-attribute-indent 4)
  (setq desktop-path '("~/.emacs.d/" "~" "."))
  (global-unset-key "\C-z")
  (global-set-key "\C-z" 'advertised-undo))

(use-package tramp
  :straight (:type built-in)
  :config
  (setq emacs-persistence-directory (expand-file-name "var/" user-emacs-directory))
  (let ((dir (expand-file-name "backup" emacs-persistence-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setq backup-directory-alist `(("." . ,dir))))
  (let ((backup-dir (concat emacs-persistence-directory "tramp-backup/")))
    (setq tramp-persistency-file-name (concat emacs-persistence-directory "tramp")
	  tramp-backup-directory-alist `(("." . ,backup-dir))
          tramp-auto-save-directory (concat emacs-persistence-directory "tramp-auto-save/"))
    (dolist (d (list tramp-auto-save-directory backup-dir))
      (unless (file-exists-p d)
	(make-directory d t)))))

(use-package nxml
  :straight (:type built-in)
  :config
  (add-hook 'hack-local-variables-hook
            (lambda ()
	      (save-excursion
		(goto-char (point-min))
		(when (search-forward-regexp "^<\\?xml" 6 0)
		  (nxml-mode))))))

(use-package files
  :straight (:type built-in)
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (auto-save-default nil)
  (backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t))))

(use-package proceed
  :straight (:type built-in)
  :no-require t
  :hook (add-hook 'proced-mode-hook 'proced-settings))

(defun proced-settings ()
  (global-set-key (kbd "C-x p") #'proced)
  (setq-default proced-filter 'all)
  (proced-toggle-auto-update))

(use-package custom
  :straight (:type built-in)
  :no-require t
  :config
  (setq custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package geiser-guile
  :mode (("\\.[Ss][Cc][Mm]\\'" . guix-devel-mode))
  :straight t)

(use-package exwm
  :init
  (require 'exwm)
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  (exwm-enable)
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  (add-hook 'exwm-randr-screen-change-hook #'efs/exwm-change-screen-hook)
  (exwm-systemtray-enable)
  (exwm-randr-enable)
  :config
  (defun efs/configure-window-by-class ()
    (interactive)
    (pcase (buffer-name)
      ("*eshell*" (exwm-workspace-move 1))))

  (defun remove-whitespaces (string)
    (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

  (defun bluetooth/switch-profile (profile)
    (let ((card
	   (remove-whitespaces
	    (replace-regexp-in-string
	     "Name:"
	     ""
	     (shell-command-to-string "pactl list | grep bluez_card")))))
      (start-process "set-bluetooth-profile" nil "pactl" "set-card-profile" card profile)))

  (defun efs/exwm-init-hook ()
    (exwm-workspace-switch-create 1)
    (start-process-shell-command "" nil  "shepherd")
    (eshell))

  (setq exwm-layout-show-all-buffers t)
  (setq exwm-input-global-keys
        `(([s-print] . desktop-environment-screenshot-part)
          ([s-escape] . desktop-environment-lock-screen)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
	  ([?\s-.] . (lambda ()
		       (interactive)
		       (bluetooth/switch-profile "a2dp_sink")))
	  ([?\s-,] . (lambda ()
		       (interactive)
		       (bluetooth/switch-profile "handsfree_head_unit")))
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (defun efs/exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
	  (xrandr-output-disconnected-regexp "\n\\([^ ]+\\) disconnected primary")
          default-output)

      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
	(if (re-search-forward xrandr-output-disconnected-regexp nil 'noerror)
	    (call-process "xrandr" nil nil nil "--output" (match-string 1) "--off")))

      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (re-search-forward xrandr-output-regexp nil 'noerror)
        (setq default-output (match-string 1))
        (forward-line)
        (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
	    (call-process "xrandr" nil nil nil "--output" default-output "--auto" "--current")
	    (setq exwm-randr-workspace-output-plist (list 0 default-output))
	  (call-process
           "xrandr" nil nil nil
           "--output" (match-string 1) "--primary" "--auto" "--above" default-output "--rotate" "normal"
	   "--output" default-output "--auto" "--rotate" "normal")
          (setq exwm-randr-workspace-output-plist (list 1 (match-string 1) 0 default-output)))))))

(use-package helm-exwm
  :after helm
  :commands (helm-exwm)
  :config
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
				    helm-source-recentf)))
(use-package async
  :after bytecomp
  :hook ((after-init . async-bytecomp-package-mode)
	 (dired-mode . dired-async-mode)))

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package helm
  :straight (:type git :host github :repo "emacs-helm/helm")
  :init (helm-mode)
  :bind (("C-x f" . 'helm-find-files)
	 ("C-x p" . 'helm-browse-project)
	 ("C-s" . 'helm-occur)
	 ("C-x b" . 'helm-mini)
	 ("C-x <mouse-movement>" . 'helm-mini)
	 ("C-x C-b" . 'helm-mini))
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (setq helm-split-window-in-side-p t
	helm-M-x-fuzzy-match t
	helm-locate-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-apropos-fuzzy-match t
	helm-etag-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-autoresize-min-height 20))

(use-package erc
  :hook
  (erc-mode . abbrev-mode)
  (erc-mode . erc-spelling-mode)
  :config
  (global-set-key (kbd "s-<f12>") #'connect-libera-irc)
  (defun connect-libera-irc ()
    (interactive)
    (erc-tls :server "irc.libera.chat" :port 6697 :nick "klovanych"))
  (setq
   erc-nick "klovanych"
   erc-user-full-name "Nazar Klovanych"
   erc-prompt-for-password nil
   erc-log-channels-directory "~/Messages/ERC"
   erc-autojoin-channels-alist
   '(("#emacs"
      "#guix"
      "#libreboot"
      "#org-mode"))
   erc-modules
   '(autoaway autojoin button completion fill sound
              list match menu move-to-prompt netsplit networks noncommands ring stamp track
              smiley notify notifications)))

(use-package flyspell
  :straight (:type built-in)
  :init (flyspell-mode))

(use-package helm-ag
  :straight (:type git :host github :repo "emacsorphanage/helm-ag")
  :config
  (setq helm-ag-insert-at-point 'word)
  (define-key projectile-mode-map (kbd "C-c p s s") 'helm-do-ag)
  (setq helm-ag-base-command "ag --nocolor --nogroup")
  (setq helm-ag-success-exit-status '(0 2))
  :after helm)

(use-package helm-projectile
  :straight t
  :after helm
  :hook (projectile-mode . helm-projectile-on)
  :commands helm-projectile)

;; Projectile mode and extensions
(use-package projectile
  :straight t
  :init (projectile-mode)
  :after helm
  :config
  (defvar projectile-project-folder '("~/Projects/atwix/"))
  (setq projectile-enable-caching nil
	projectile-project-search-path projectile-project-folder
	projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".pyc" ".jar" "*.class")
	projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".mypy_cache")
	projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store" "GTAGS")
	projectile-mode-line-prefix " - ")
  (global-set-key (kbd "C-c p") 'projectile-command-map))

;; Dired extensions and utils
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (add-hook 'dired-sidebar-mode-hook
            (lambda () (linum-mode -1)))
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-width 38))

;; Icons for dired sidebar
(use-package vscode-icon
  :straight t
  :after dired-sidebar)

;; Global customizations
;; ===============================================

(use-package doom-themes
  :straight t
  :init(load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config))

(use-package org
  :straight (:type built-in))

(use-package logview
  :straight (:type git :host github :repo "doublep/logview"))

(use-package mu4e
  :straight (:type built-in)
  :init (load "~/.emacs.d/mu4e-config.el")
  :config
  (require 'mu4e)
  (mu4e--init-handlers)
  (run-with-timer 0 300 #'mu4e-update-mail-and-index t)
  (mu4e-modeline-mode -1)
  (add-hook 'mu4e-main-mode-hook (lambda()  (mu4e-modeline-mode -1))))

(use-package mu4e-alert
  :straight (:type git :host github :repo "iqbalansari/mu4e-alert")
  :init (mu4e-alert-enable-mode-line-display)
  :config
  (setq mu4e-alert-email-notification-types '(count))
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications))

(use-package nerd-icons
  :straight (:type git :host github :repo "rainstormstudio/nerd-icons.el"))

(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-mode)
  :config
  (with-eval-after-load "doom-modeline"
    (doom-modeline-def-modeline 'main
      '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
      '(objed-state lsp vcs major-mode persp-name grip gnus github debug repl minor-modes input-method indent-info checker process mu4e misc-info battery time "  ")))
  (setq doom-modeline-mu4e t)
  (setq doom-modeline-height 25)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-vcs-max-length 32))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;; PHP settings
;; ===============================================

(use-package php-doc-block
  :after php-mode
  :straight (:type git
                   :host github
                   :repo "moskalyovd/emacs-php-doc-block")
  :config
  (define-key php-mode-map (kbd "<C-tab>") 'php-doc-block))

(use-package php-cs-fixer
  :straight (:type git :repo "Nazar65/emacs-php-cs-fixer")
  :after php-mode
  :config
  (setq php-cs-fixer-rules-config-file "/home/nazar/.dotfiles/.config/php/.php-cs-fixer.dist.php"
        php-cs-fixer-cache-file-path "/home/nazar/.dotfiles/.config/php/.php-cs-fixer.cache"))

(use-package phpunit
  :straight (:type git :host github :repo "nlamirault/phpunit.el")
  :config
  (setq phpunit-root-directory "./")
  (setq phpunit-configuration-file "./dev/tests/integration/phpunit.xml.dist")
  (define-key php-mode-map (kbd "C-t t") 'phpunit-current-test)
  (define-key php-mode-map (kbd "C-t c") 'phpunit-current-class)
  (define-key php-mode-map (kbd "C-t p") 'phpunit-current-project))

;; PHP debugger
(use-package dap-mode
  :straight t
  :after lsp-mode
  :init (dap-mode -1)
  :config
  (require 'dap-php)
  (define-key dap-mode-map (kbd "<f5>") 'dap-debug)
  (define-key dap-mode-map (kbd "s-o") 'dap-next)
  (define-key dap-mode-map (kbd "s-i") 'dap-step-in)
  (define-key dap-mode-map (kbd "s-g") 'dap-continue)
  (define-key dap-mode-map (kbd "<f10>") 'dap-disconnect)
  (define-key dap-mode-map (kbd "<f9>") 'dap-breakpoint-add)
  (define-key dap-mode-map (kbd "<f11>") 'dap-breakpoint-delete))

(use-package csv-mode
  :straight t
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))

;; Php mode
(use-package php-mode
  :straight (:type git :host github :repo "emacs-php/php-mode")
  :config
  (define-key php-mode-map (kbd "<f12>") 'dap-mode)
  (add-hook 'php-mode-hook 'display-line-numbers-mode)
  (add-hook 'php-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'php-cs-fixer-before-save)
              (add-hook 'before-save-hook 'delete-trailing-whitespace))))

;; Flycheck to check syntax
(use-package flycheck
  :straight t
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'php-mode-hook 'flycheck-mode))

(use-package web-mode
  :custom
  (web-mode-indent-style 4)
  (css-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-engines-alist '(("django" . "\\.html\\'")))
  :custom-face
  (web-mode-block-string-face ((t (:inherit font-lock-string-face))))
  (web-mode-html-attr-value-face ((t (:inherit font-lock-string-face :foreground nil))))
  (web-mode-current-element-highlight-face ((t (:inherit highlight))))
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.xml\\'"        . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode))
  :hook
  (web-mode . display-line-numbers-mode)
  (web-mode . web-mode-toggle-current-element-highlight))

(use-package json-mode
  :mode ("\\.json\\'" . (lambda ()
			  (json-mode)
                          (flycheck-mode)
			  (display-line-numbers-mode))))

(use-package json-navigator
  :commands json-navigator-navigate-region)

(use-package eglot
  :straight (:host github :repo "joaotavora/eglot")
  :hook ((php-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (css-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(php-mode . ("/home/nazar/lsp-servers/intelephense/node_modules/.bin/intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs '(json-mode . ("/home/nazar/lsp-servers/json-css-html/node_modules/.bin/vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(js2-mode . ("/home/nazar/lsp-servers/js-typescript/node_modules/.bin/typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-mode . ("/home/nazar/lsp-servers/json-css-html/node_modules/.bin/vscode-css-language-server" "--stdio")))
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t))

(use-package eldoc-box
  :straight (:host github :repo "casouri/eldoc-box")
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :after eglot)


(use-package sudo-edit
  :bind*
  (("C-x e" . sudo-edit-find-file))
  :commands sudo-edit)

(use-package go-translate
  :straight (:host github :repo "lorniu/go-translate")
  :bind*
  (("C-x t w" . gts-do-translate))
  :config
  (setq gts-translate-list '(("en" "uk") ("uk" "en")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker :single t)
         :engines (list (gts-google-rpc-engine :parser (gts-google-rpc-parser) :url "https://translate.google.com"))
         :render (gts-buffer-render))))

(use-package gif-screencast
  :straight (:host gitlab :repo "ambrevar/emacs-gif-screencast")
  :bind
  ( :map gif-screencast-mode-map
    ("<f8>". gif-screencast-toggle-pause)
    ("<f9>". gif-screencast-stop)))

(use-package emojify
  :commands emojify-mode)

(use-package slack
  :straight (:host github :repo "isamert/emacs-slack")
  :commands (slack-start)
  :config
  (slack-register-team
   :name "atwix"
   :modeline-enabled t
   :default t
   :token (auth-source-pick-first-password
           :host "atwix.slack.com"
           :user "token" :type 'netrc :max 1)
   :cookie (auth-source-pick-first-password
            :host "atwix.slack.com"
            :user "cookie" :type 'netrc :max 1)
   :subscribed-channels '((general)))
  (setq slack-log-level 'error)
  (setq slack-buffer-function 'switch-to-buffer)
  (setq slack-buffer-function #'switch-to-buffer-other-window)
  ;; ^ Open slack windows on the right side of the screen
  (setq slack-modeline-formatter #'slack-icon-modeline-formatter)
  (setq slack-alert-icon "/home/nazar/.emacs.d/static/slack/icon.png")
  (setq slack-enable-global-mode-string t)
  (setq slack-modeline-count-only-subscribed-channel nil)
  (setq slack-buffer-emojify t)
  (setq slack-render-image-p t)
  (setq slack-prefer-current-team t)
  (setq tracking-max-mode-line-entries 0)
  (define-key ctl-x-map "j" #'slack-select-rooms)
  (define-key ctl-x-map "l" #'slack-select-unread-rooms)
  (define-key slack-mode-map "@"
    (defun endless/slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention)
      (insert " ")))
  (define-key slack-mode-map (kbd "C-c C-e")
    #'slack-message-edit)
  (defun emacs-mode-line-logo-image ()
    (find-image
     (list (list :type 'svg
		 :file "/home/nazar/.emacs.d/static/slack/Slack_icon_2019.svg"
                 :scale 1 :ascent 'center
		 :mask 'heuristic
                 :height 15))))
  (defun slack-icon-modeline-formatter (alist)
    (mapconcat #'(lambda (e)
                   (let* ((summary (cdr e))
                          (thread (cdr (cl-assoc 'thread summary)))
                          (channel (cdr (cl-assoc 'channel summary)))
                          (thread-has-unreads (car thread))
                          (channel-has-unreads (car channel))
                          (has-unreads (or thread-has-unreads
                                           channel-has-unreads))
                          (thread-mention-count (cdr thread))
                          (channel-mention-count (cdr channel))
			  (count-messages (+ channel-mention-count thread-mention-count)))
                     (format " %s %s "
			     (propertize "◀"
					 'display (emacs-mode-line-logo-image))
                             (if (or channel-has-unreads (< 0 count-messages))
				 (propertize (number-to-string count-messages)
                                             'face 'slack-modeline-channel-has-unreads-face)
                               0))))
               alist ""))
  (advice-add 'slack-message-notify-alert :before
	      (lambda(message room team)
		(if (slack-message-notify-p message room team)
		    (async-start
		     (lambda ()
		       (play-sound-file "~/.dotfiles/Sounds/Slack-Notification-Tone.au")
		       'ignore))))))

(use-package alert
  :straight (:type git :repo "jwiegley/alert")
  :commands (alert)
  :config
  (setq alert-default-style 'libnotify))

(use-package forge
  :after magit
  :straight (:type git :host github :repo "magit/forge"))

(use-package magit
  :straight (:type git :host github :repo "magit/magit")
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Styles section css
;; ===============================================
(use-package css-mode
  :straight t
  :config
  (setq auto-mode-alist
	(cons '("\\.css\\'" . css-mode) auto-mode-alist)))

;; Javascript
;; ==============================================
(use-package js2-mode
  :straight (:type git :repo "mooz/js2-mode")
  :config
  (add-hook 'before-save-hook #'js2-before-save-hook)
  (add-hook 'js2-mode-hook 'lsp)
  (setq auto-mode-alist
	(cons '("\\.js\\'" . js2-mode) auto-mode-alist)))

(defun js2-before-save-hook ()
  (when (eq major-mode 'js2-mode)
    (web-beautify-js)
    (message "File is Beautified")))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
