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

(use-package password-store
  :straight t)

(use-package ztree
  :straight (:type git :host github :repo "fourier/ztree"))

(use-package bluetooth
  :straight (:type git :host github :repo "emacs-straight/bluetooth"))

(use-package restclient
  :straight (:type git :host github :repo "pashky/restclient.el"))

(use-package lemon
  :straight (:type git :host codeberg :repo "emacs-weirdware/lemon")
  :config (lemon-mode 1))

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
  (defun eos/crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'eos/crm-indicator)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t)
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

(use-package custom
  :straight (:type built-in)
  :no-require t
  :config
  (setq custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

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

(use-package async
  :after bytecomp
  :hook ((after-init . async-bytecomp-package-mode)
	 (dired-mode . dired-async-mode)))

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*.el"))
  :init (vertico-mode))

(use-package marginalia
  :after vertico
  :init(marginalia-mode)
  :functions(marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'center))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after vertico
  :bind (("C-x b"    . consult-buffer)
         ("C-x p"    . consult-project-buffer)
         ("C-x K"     . kill-current-buffer)
         ("C-x C-b"  . ibuffer)
         ("C-c s r"  . 'consult-ripgrep)
         ("C-c s g"  . 'consult-grep)
         ("C-c s i"  . 'consult-git-grep)
         ("C-c s f"  . 'consult-find))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (defvar php-source
    (list :name     "PHP Buffer"
          :category 'buffer
          :narrow   ?c
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              (insert "#+title: " name "\n\n")
              (php-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (eq (buffer-local-value 'major-mode x) 'php-mode))
                     (buffer-list))))))

  (add-to-list 'consult-buffer-sources 'php-source 'append))

(use-package embark
  :bind (("C-c a" . embark-act))
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package erc
  :hook
  (erc-mode . abbrev-mode)
  (erc-mode . erc-spelling-mode)
  :bind (("s-<f12>" . #'connect-libera-irc))
  :config
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
  (run-with-timer 0 900 #'mu4e-update-mail-and-index t)
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

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :bind (("C-n"     . corfu-next)
        ("C-p"     . corfu-previous)
        ("C-q"     . corfu-quick-insert)
        ("M-p"     . corfu-popupinfo-scroll-down)
        ("M-n"     . corfu-popupinfo-scroll-up))
  :init
  (global-corfu-mode)
  :custom
  (corfu-popupinfo-direction '(right left vertical))
  :config
  (setq corfu-auto  t
        corfu-auto-prefix 1
        corfu-quit-no-match nil
        corfu-preview-current nil
        corfu-popupinfo-delay 0.3
        corfu-popupinfo-max-width 70
        corfu-popupinfo-max-height 20))

(use-package svg-lib
  :after kind-icon)

(use-package kind-icon
  :straight (:type git :host github :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Clojure Programming support
;; ===============================================

(use-package clojure-mode
  :straight (:type git :hosts github :repo "clojure-emacs/clojure-mode")
  :commands (clojure-mode clojurescript-mode)
  :hook (outline-minor-mode . clojure-mode)
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode))
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package cider
    :ensure t
    :commands (cider-mode cider-repl-mode))

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
  :straight (:type git :repo "http://git-space.klovanych.org/emacs-php-cs-fixer/")
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

(use-package csv-mode
  :straight t
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))

;; Php mode
(use-package php-mode
  :straight (:type git :host github :repo "emacs-php/php-mode")
  :init (setq php-mode-coding-style 'psr2)
  :config
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
         (css-mode . eglot-ensure)
         (clojure-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(php-mode . ("/home/nazar/lsp-servers/intelephense/node_modules/.bin/intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs '(json-mode . ("/home/nazar/lsp-servers/json-css-html/node_modules/.bin/vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(js2-mode . ("/home/nazar/lsp-servers/js-typescript/node_modules/.bin/typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-mode . ("/home/nazar/lsp-servers/json-css-html/node_modules/.bin/vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(clojure-mode . ("/home/nazar/lsp-servers/clojure/clojure-lsp")))
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

(use-package gif-screencast
  :straight (:host gitlab :repo "ambrevar/emacs-gif-screencast")
  :bind
  ( :map gif-screencast-mode-map
    ("<f8>". gif-screencast-toggle-pause)
    ("<f9>". gif-screencast-stop)))

(use-package emojify
  :commands emojify-mode)

(use-package slack
  :straight (:host github :repo "Konubinix/emacs-slack")
  :config
  (url-cookie-store
   "d"
   (auth-source-pick-first-password
            :host "atwix.slack.com"
            :user "cookie" :type 'netrc :max 1)
   nil ".slack.com" "/" t)
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

(use-package diff-hl
  :straight (:host github :repo "dgutov/diff-hl")
  :config
  (add-hook 'js2-mode 'diff-hl-mode)
  (add-hook 'emacs-lisp-mode 'diff-hl-mode)
  (add-hook 'php-mode-hook 'diff-hl-mode))

;; Styles section css
;; ===============================================
(use-package css-mode
  :straight t
  :mode (("\\.css\\'" . css-mode)
         ("\\.scss\\'" . scss-mode)))

;; Javascript
;; ==============================================
(use-package js2-mode
  :straight (:type git :repo "mooz/js2-mode")
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (add-hook 'before-save-hook #'js2-before-save-hook))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
