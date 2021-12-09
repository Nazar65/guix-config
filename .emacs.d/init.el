
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
  :after emacs
  :straight (:type built-in))

(use-package guix
  :straight t)

(use-package geiser-guile
  :mode (("\\.[Ss][Cc][Mm]\\'" . guix-devel-mode))
  :straight t)

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
  (nsm-settings-file "~/.emacs.d/network-security.data")
  (history-delete-duplicates t)
  (display-time-default-load-average nil)
  (history-length 600)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (setq indent-tabs-mode nil)
  (setq display-time-mail-string "")
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (setq undo-limit 800000)
  (setq undo-strong-limit 12000000)
  (setq undo-outer-limit 120000000)
  (setq password-cache t)
  (setq password-cache-expiry 3600)
  (setq nxml-child-indent 4 nxml-attribute-indent 4)
  (global-unset-key "\C-z")
  (global-set-key (kbd "C-x p") #'proced)
  (global-set-key "\C-z" 'advertised-undo))

(use-package tramp
  :straight (:type built-in)
  :config
  (setq emacs-persistence-directory (expand-file-name "var" user-emacs-directory))
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
  :hook (add-hook 'proced-mode-hook 'proced-settings)
  :config
  (defun proced-settings ()
    (proced-toggle-auto-update)))

(use-package custom
  :straight (:type built-in)
  :no-require t
  :config
  (setq custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package exwm
  :after emacs
  :init
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  (add-hook 'exwm-randr-screen-change-hook #'efs/exwm-change-screen-hook)
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (exwm-randr-enable)
  (exwm-enable)
  :config
  (add-hook 'exwm-update-class-hook
            (lambda ()
		(exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
		(exwm-workspace-rename-buffer exwm-title)))
  (defun efs/configure-window-by-class ()
    (interactive)
    (pcase (buffer-name)
      ("*eshell*" (exwm-workspace-move 1))
      ("DuckDuckGo — Privacy, simplified." (exwm-workspace-move 2 0))))
  (defun efs/exwm-init-hook ()
    (exwm-workspace-switch-create 1)
    (eshell))
  
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
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

  (defun efs/exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
          default-output)
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (re-search-forward xrandr-output-regexp nil 'noerror)
        (setq default-output (match-string 1))
        (forward-line)
        (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
	    (call-process "xrandr" nil nil nil "--output" default-output "--auto")
          (call-process
           "xrandr" nil nil nil
           "--output" (match-string 1) "--primary"  "--auto" "--pos" "1920x0" "--rotate" "normal"
	   "--output" default-output "--auto" "--pos" "0x0" "--rotate" "normal")
          (setq exwm-randr-workspace-output-plist (list 1 (match-string 1) 0 default-output)))))))

(use-package async
  :after bytecomp
  :hook ((after-init . async-bytecomp-package-mode)
	 (dired-mode . dired-async-mode)))

(use-package volume
  :config
  (global-set-key (kbd "<XF86AudioRaiseVolume>") 'volume-raise-10)
  (global-set-key (kbd "<XF86AudioLowerVolume>") 'volume-lower-10)
  (global-set-key (kbd "<XF86AudioMute>") 'volume-set-to-0%))

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package helm
  :after emacs
  :straight t
  :init (helm-mode)
  :bind (("C-x b" . 'helm-mini)
	 ("C-x f" . 'helm-find-files))
  :config
  (require 'helm-config)
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
	helm-autoresize-min-height 20))

(use-package flyspell
  :straight (:type built-in)
  :init (flyspell-mode))

(use-package helm-rg
  :straight t
  :after helm-projectile
  :init
 (global-set-key (kbd "C-c p s s") 'helm-rg))

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
  (defvar projectile-project-folder '("~/Projects/i4/"))
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

;; An atom-one-dark theme for smart-mode-line
(use-package smart-mode-line-atom-one-dark-theme
  :straight t)

(use-package smart-mode-line
  :straight t
  :init (smart-mode-line-enable)
  :config
  (add-hook 'exwm-init-hook 'smart-mode-line-enable)
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

(use-package doom-themes
  :straight t
  :init(load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;; PHP settings
;; ===============================================
(use-package php-cs-fixer
  :after php-mode
  :load-path ("~/.emacs.d/src/php-cs-fix/"))

(use-package lsp-mode
  :straight (:type git :repo "emacs-lsp/lsp-mode")
  :config
  (setq lsp-prefer-flymake nil
        lsp-idle-delay 0.500
        read-process-output-max (* 1024 1024)
        gc-cons-threshold 100000000
        lsp-enable-file-watchers nil
        lsp-log-io nil
        lsp-ui-doc-show-with-mouse nil
        sp-signature-auto-activate nil
	lsp-ui-doc-position 'at-point
        lsp-completion-provider :capf)
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-tcp-server
     (lambda (port)
       `("php72", (expand-file-name "~/.config/composer/vendor/felixfbecker/language-server/bin/php-language-server.php"),
         (format "--tcp=localhost:%s" port)"--memory-limit=9095M")))
    :major-modes '(php-mode)
    :server-id 'php-ls))
  :hook (php-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :straight (:type git :repo "emacs-lsp/lsp-ui")
  :hook (lsp-mode . lsp-ui-mode)
  :requires lsp-mode flycheck
  :custom
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.500))

(use-package phpunit
  :straight t
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
  :straight t
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
  (define-key dap-mode-map (kbd "<f11>") 'dap-breakpoint-delete))

;; For csv files
(use-package csv-mode
  :straight t
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  :config
  (add-hook 'csv-mode-hook 'csv-align-mode))

;; Php mode
(use-package php-mode
  :straight t
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'php-cs-fixer-before-save)
              (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (setq php-cs-fixer-rules-level-part-options (quote ("@PSR2")))
  (setq php-cs-fixer-rules-fixer-part-options
        (quote("no_multiline_whitespace_before_semicolons"
              "no_unused_imports"
               "declare_strict_types"
               "no_whitespace_before_comma_in_array"
               "array_indentation"
               "no_spaces_inside_parenthesis"
               "multiline_whitespace_before_semicolons"
               "no_extra_blank_lines"
               "no_spaces_around_offset"
               "trim_array_spaces"
               "whitespace_after_comma_in_array"
               "binary_operator_spaces"))))

;; Flycheck to check syntax
(use-package flycheck
  :straight t
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq auto-mode-alist
	(cons '("\\.el\\'" . flycheck-mode) auto-mode-alist)))

(use-package slack
  :straight (:type git :host github :repo "yuya373/emacs-slack")
  :commands (slack-start)
  :config
  (slack-register-team
   :name "i4"
   :default t
   :token (auth-source-pick-first-password
           :host '("i4.slack.com")
           :user "token" :type 'netrc :max 1)
   :cookie (auth-source-pick-first-password
            :host '("i4.slack.com")
            :user "cookie" :type 'netrc :max 1)
   :subscribed-channels '((general)))
  (setq slack-buffer-emojify t)
  (setq slack-render-image-p t)
  (setq slack-prefer-current-team t)
  (setq tracking-max-mode-line-entries 0)
  (define-key ctl-x-map "j" #'slack-select-rooms)
  (define-key slack-mode-map "@"
    (defun endless/slack-message-embed-mention ()
      (interactive)
      (call-interactively #'slack-message-embed-mention)
      (insert " ")))
  (define-key slack-mode-map (kbd "C-c C-d")
    #'slack-message-delete)
  (define-key slack-mode-map (kbd "C-c C-e")
    #'slack-message-edit)
  (define-key slack-mode-map (kbd "C-c C-k")
    #'slack-channel-leave))

(use-package alert
  :straight (:type git :repo "jwiegley/alert")
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(use-package gnus-notify
  :straight (:type built-in)
  :after gnus
  :load-path ("~/.emacs.d/src/gnus-notify"))

(use-package smtpmail :straight t)

(use-package gnus
  :straight (:type built-in)
  :init (require 'gnus-notify)
  :config
  (defadvice gnus-group-get-new-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (30 (message "Gnus timed out."))
    ad-do-it))
  (gnus-demon-add-handler 'gnus-group-get-new-news 1 nil)
  (setq gnus-secondary-select-methods
        '((nnml "local.mail")
          (nntp "news.gnus.org")
          (nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl))
          (nnimap "i4"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl))))
    (setq gnus-parameters
        '(("INBOX"
           (gnus-use-adaptive-scoring nil)
           (gnus-use-scoring nil)
           (visible . t)
           (display . all)
           (modeline-notify . t))
          ("mail.misc"
           (gnus-use-adaptive-scoring nil)
           (gnus-use-scoring nil)
           (visible . t)
           (display . all)
           (modeline-notify . t)))
          group-name-map
          '(("nnml+local.mail:mail.misc" . "Local")
            ("nnimap+gmail:INBOX" . "Gmail")
            ("nnimap+i4:INBOX" . "i4"))
        user-mail-address	"nazarn96@gmail.com"
        user-full-name	"Nazar Klovanych"
        mail-sources '((file :path "/var/spool/mail/nazar"))
        gnus-thread-sort-functions'((not gnus-thread-sort-by-number) gnus-thread-sort-by-score)
        gnus-select-method '(nnnil)
        w3m-fill-column 100
        mm-text-html-renderer 'gnus-w3m
        w3m-toggle-inline-images t
        w3m-default-display-inline-images t
        mm-inline-text-html-with-images t
        send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))

(use-package magit
  :straight (:type git :repo "magit/magit")
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Styles section css
;; ===============================================
(use-package css-mode
  :straight t
  :config
  (setq auto-mode-alist
	(cons '("\\.css\\'" . css-mode) auto-mode-alist)))

;; Sort css attributes
(use-package com-css-sort
  :straight t
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
(use-package js2-mode :straight t)

;; Autocomplete mode for javascript
(use-package ac-js2
  :straight t
  :after js2-mode
  :config
  (add-to-list 'company-backends 'ac-js2-company)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'company-backends 'ac-js2-company))

;; Run jscs sniffer to fix edited file
(use-package jscs
  :straight t
  :config
  (add-hook 'js2-mode-hook #'jscs-fix-run-before-save)
  (setq flycheck-eslintrc "~/.eslintrc"))

(provide 'init)
;;; init.el ends here
