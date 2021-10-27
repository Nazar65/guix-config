;;; package ---
;;; Commentary:
;;; Code:
(defvar bootstrap-version)
(defvar projectile-project-folder '("~/Projects/i4/"))

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

(use-package use-package-core
  :straight (:type built-in)
  :custom
  (use-package-verbose nil)
  (use-package-compute-statistics nil)
  (use-package-always-defer t)
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

(use-package use-package-ensure-system-package)


(use-package emacs
  :straight (:type built-in)
  :bind (("C-h"     . 'backward-delete-char-untabify)
         ("M-f"     . 'forward-to-word)
         ("M-b"     . 'backward-to-word)
         ("C-!"     . 'kill-this-buffer)
         ("C-<f5>"  . display-line-numbers-mode)
         ("C-c d"   . 'local/duplicate-start-of-line-or-region)
         ("C-c I"   . 'local/open-init-el)
         ("C-c <tab>"   . 'local/indent-buffer)
         ([remap dabbrev-expand] . 'hippie-expand))
  :bind-keymap(([F1]   .  help-map))
  :hook((before-save . delete-trailing-whitespaces)
        (emacs-startup . (lambda ()
                           (let ((startup-time (float-time (time-subtract after-init-time before-init-time))))
                             (message "Emacs ready in %.2f seconds with %d garbage collections." startup-time gcs-done)))))
  :custom
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq file-name-handler-alist nil)
  (load-prefer-newer t)
  (package-user-dir "~/.emacs.d/elpa")
  (nsm-settings-file "~/.emacs.d/network-security.data")
  (package--init-file-ensured t)
  (history-delete-duplicates t)
  (history-length 300)
  (put 'dired-find-alternate-file 'disabled nil)
  (display-time-default-load-average nil)
  :config
  (setq indent-tabs-mode nil)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (setq undo-limit 800000)
  (setq undo-strong-limit 12000000)
  (setq undo-outer-limit 120000000)
  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 3600)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  ;;Move backups file to another folder
  (setq emacs-persistence-directory
	(expand-file-name "var" user-emacs-directory))
  (let ((dir (expand-file-name "backup" emacs-persistence-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setq backup-directory-alist `(("." . ,dir)))
    )

  (let ((backup-dir (concat emacs-persistence-directory "tramp-backup/")))
    (setq tramp-persistency-file-name (concat emacs-persistence-directory "tramp")
	  tramp-backup-directory-alist `(("." . ,backup-dir))
          tramp-auto-save-directory (concat emacs-persistence-directory "tramp-auto-save/"))
    (dolist (d (list tramp-auto-save-directory backup-dir))
      (unless (file-exists-p d)
	(make-directory d t)))
    )

  (add-hook 'hack-local-variables-hook
	    (lambda ()
	      (save-excursion
		(goto-char (point-min))
		(when (search-forward-regexp "^<\\?xml" 6 0)
		  (nxml-mode)))))

  ;; set nxml line identation
  (setq nxml-child-indent 4 nxml-attribute-indent 4)


  (display-time-mode 1)
  (display-battery-mode 1)
  (global-unset-key "\C-z")
  (global-set-key (kbd "C-x p") #'proced)
  (global-set-key "\C-z" 'advertised-undo)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (menu-bar-mode nil)
  )

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
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  (require 'exwm-config)
  (exwm-systemtray-enable)
  (exwm-config-example)
  (exwm-randr-enable)
  (exwm-enable)
  :config
  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)
  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  ;; emacs window maneger setup
  (defun efs/configure-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ("Slack" (exwm-workspace-move-window 0))
      ("Skype" (exwm-workspace-move-window 0))
      ("eshell" (exwm-workspace-move-window 1))
      ("firefox" (exwm-workspace-move-window 2))
      ))

  (defun efs/exwm-init-hook ()
    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 1)

    ;; Open eshell by default
    (eshell)

    )
  (defun efs/exwm-update-title ()
    (pcase exwm-class-name
      ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title))))
    )

  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-1-0 --primary --auto --pos 1920x0 --rotate normal
                                   --output eDP-1 --auto --pos 0x0 --rotate normal"))
            (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1-0"))
	    ))

(use-package async
  :after bytecomp
  :hook ((after-init . async-bytecomp-package-mode)
	 (dired-mode . dired-async-mode)))

;; Volume controls
(use-package volume
  :config
  (global-set-key (kbd "<XF86AudioRaiseVolume>") 'volume-raise-10)
  (global-set-key (kbd "<XF86AudioLowerVolume>") 'volume-lower-10)
  (global-set-key (kbd "<XF86AudioMute>") 'volume-set-to-0%)
  )


(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package system-packages
  :custom
  system-packages-noconfirm t)

(use-package diminish)

(use-package helm
  :init (helm-mode)
  :bind (("C-x b" . 'helm-mini)
	 ("C-x f" . 'helm-find-files))
  :config
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)

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

	helm-autoresize-min-height 20
	)
  (require 'helm-config))

;; Helm rg depends on ripgrep
(use-package helm-rg
  :after helm-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p s s") 'helm-rg)
  )


(use-package helm-projectile
  :after helm
  :diminish
  :hook (projectile-mode . helm-projectile-on)
  :commands helm-projectile
  )

;; Projectile mode and extensions
(use-package projectile
  :init (projectile-mode)
  :after helm
  :config
  (setq projectile-enable-caching nil
	projectile-project-search-path projectile-project-folder
	projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".pyc" ".jar" "*.class")
	projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".mypy_cache")
	projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store" "GTAGS")
	projectile-mode-line-prefix " - "
	)

  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  :after dired-sidebar
  )

;; Global customizations
;; ===============================================

;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme)

;; smart-mode-line
(use-package smart-mode-line
  :init(smart-mode-line-enable)
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

(use-package doom-themes
  :init(load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config))

(use-package company
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;; PHP settings
;; ===============================================
(use-package php-cs-fixer
  :after php-mode
  :load-path ("~/.emacs.d/src/php-cs-fix/")
  )

(use-package lsp-mode
  :config
  (setq lsp-completion-provider :capf)
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-tcp-server
     (lambda (port)
       `("php72",
         (expand-file-name "~/.config/composer/vendor/felixfbecker/language-server/bin/php-language-server.php"),
         (format "--tcp=localhost:%s" port)"--memory-limit=9095M")))
    :major-modes '(php-mode)
    :server-id 'php-ls))
  (setq lsp-prefer-flymake nil)
  (setq lsp-idle-delay 0.500)
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-log-io nil)
  :hook (php-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :diminish
  :hook (lsp-mode . lsp-ui-mode)
  :requires lsp-mode flycheck
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 5))

(use-package phpunit
  :after php-mode
  :config
  ;;phpunit settings
  (setq phpunit-root-directory "./")
  (setq phpunit-configuration-file "./dev/tests/unit/phpunit.xml.dist")
  (define-key php-mode-map (kbd "C-t t") 'phpunit-current-test)
  (define-key php-mode-map (kbd "C-t c") 'phpunit-current-class)
  (define-key php-mode-map (kbd "C-t p") 'phpunit-current-project))

;; PHP debugger
(use-package dap-mode
  :diminish
  :after lsp-mode
  :init (dap-mode -1)
  :config
  (require 'dap-php)
  (define-key php-mode-map (kbd "<f12>") 'dap-mode)
  (define-key dap-mode-map (kbd "<f5>") 'dap-debug)
  (define-key dap-mode-map (kbd "s-o") 'dap-next)
  (define-key dap-mode-map (kbd "s-i") 'dap-step-in)
  (define-key dap-mode-map (kbd "s-g") 'dap-continue)
  (define-key dap-mode-map (kbd "<f10>") 'dap-disconnect)
  (define-key dap-mode-map (kbd "<f9>") 'dap-breakpoint-add)
  (define-key dap-mode-map (kbd "<f11>") 'dap-breakpoint-delete)
  )

;; For csv files
(use-package csv-mode
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode)
  )

;; Php mode
(use-package php-mode
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'php-cs-fixer-before-save)
              (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (setq-default show-trailing-whitespace t)
  (setq php-cs-fixer-rules-level-part-options (quote ("@PSR2")))
  (setq whitespace-style '(face lines-trail))
  (setq whitespace-line-column 120)
  (setq php-cs-fixer-rules-fixer-part-options
        (quote("no_multiline_whitespace_before_semicolons" "no_unused_imports" "declare_strict_types" "no_whitespace_before_comma_in_array" "array_indentation" "no_spaces_inside_parenthesis" "multiline_whitespace_before_semicolons" "no_extra_blank_lines" "no_spaces_around_offset" "trim_array_spaces" "whitespace_after_comma_in_array" "binary_operator_spaces"))))

;; Flycheck to check syntax
(use-package flycheck
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq auto-mode-alist
	(cons '("\\.el\\'" . flycheck-mode) auto-mode-alist))
  )

;; Magit settings
;; ===============================================

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )

;; Styles section css
;; ===============================================
(use-package css-mode
  :config
  (setq auto-mode-alist
	(cons '("\\.css\\'" . css-mode) auto-mode-alist)))

;; Sort css attributes
(use-package com-css-sort
  :after css-mode
  :config
  (setq com-css-sort-sort-type 'alphabetic-sort)
  ;; Sort attributes inside block.
  (define-key css-mode-map (kbd "C-c C-s") #'com-css-sort-attributes-block)
  ;; Sort attributes through the whole document.
  (define-key css-mode-map (kbd "C-c C-d") #'com-css-sort-attributes-document))

;; Javascript setting
;; ==============================================

;; General javascript mode
(use-package js2-mode
  :config
  (add-hook 'php-mode-hook 'whitespace-mode))

;; Autocomplete mode for javascript
(use-package ac-js2
  :after js2-mode
  :config
  (add-to-list 'company-backends 'ac-js2-company)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'company-backends 'ac-js2-company)
  )
;; Run jscs sniffer to fix edited file
(use-package jscs
  :config
  (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
  (setq flycheck-eslintrc "~/.eslintrc"))

(provide 'init)
;;; init.el ends here
