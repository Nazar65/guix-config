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

(use-package wallpaper
  :straight (:type git :host github :repo "farlado/emacs-wallpaper")
  :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
         (after-init . wallpaper-cycle-mode))
  :custom ((wallpaper-cycle-single t)
           (wallpaper-scaling 'scale)
           (wallpaper-cycle-interval 45)
           (wallpaper-cycle-directory "~/Pictures/wallpapers")))

(use-package org-jira
  :straight (:type git :host github :repo "ahungry/org-jira")
  :config
  (setq jiralib-url "https://burpeeit.atlassian.net")
  (setq jiralib-update-issue-fields-exclude-list '(reporter))
  (setq org-jira-custom-jqls
  '(
    (:jql " project = MAG AND (assignee = currentUser() OR assignee = EMPTY ) AND Sprint IN openSprints() AND status IN ('Ready for Dev','New','In Design','Blocked','On Hold') ORDER BY priority DESC"
          :limit 50
          :filename "nazars-current-tasks")
    (:jql " project = MAG AND assignee = currentUser() AND status IN ('To Estimate') ORDER BY priority DESC  "
          :limit 50
          :filename "to-estimate-tickets")
    (:jql " project = MAG AND assignee = currentUser() AND status IN ('Ready for Code Review') AND Sprint IN openSprints() ORDER BY priority DESC "
          :limit 50
          :filename "code-review-tickets")
    (:jql " project = MAG AND assignee = currentUser() AND status IN ('In Dev') AND Sprint IN openSprints() ORDER BY priority DESC "
          :limit 50
          :filename "in-dev-tickets")
    ))

    (defconst jiralib-token
    '("Cookie" . (auth-source-pick-first-password
                  :host "burpeeit.atlassian.com"
                  :user "cookie" :type 'netrc :max 1))))

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

(use-package direnv
  :straight (:type git :host github :repo "wbolster/emacs-direnv"))

(use-package pinentry
  :straight t
  :after emacs
  :config
  (setq epa-pinentry-mode 'loopback)
  :init)

(use-package emacs
  :straight t
  :bind (("M-f"     . 'forward-to-word)
         ("M-b"     . 'backward-to-word)
         ("C-!"     . 'kill-this-buffer)
         ("C-<f5>"  . display-line-numbers-mode)
         ("C-c d"   . 'local/duplicate-start-of-line-or-region)
         ([remap dabbrev-expand] . 'hippie-expand))
  :mode (("\\.[Ee][Ll]\\'" . (lambda ()
                               (emacs-lisp-mode)
			       (display-line-numbers-mode))))
  :init
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0)
  (display-time-mode 0)
  (display-battery-mode 0)
  :custom
  (read-extended-command-predicate
        #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  (byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (file-name-handler-alist nil)
  (indent-tabs-mode nil)
  (history-delete-duplicates t)
  (display-time-default-load-average nil)
  (history-length 600)
  (put 'dired-find-alternate-file 'disabled nil)
  (warning-minimum-level :emergency)
  (display-time-mail-string "")
  (display-time-day-and-date nil)
  (display-time-24hr-format nil)
  (undo-limit 800000)
  (undo-strong-limit 12000000)
  (undo-outer-limit 120000000)
  (password-cache t)
  (password-cache-expiry 3600)
  (nxml-child-indent 4 nxml-attribute-indent 4)
  (desktop-path '("~/.emacs.d/" "~" "."))
  :config
  (global-unset-key "\C-z")
  (global-set-key "\C-z" 'advertised-undo))

(use-package sql
  :straight t
  :custom
  (sql-connection-alist
   '((mysql-local-burpee
      (sql-product 'mysql)
      (sql-server "127.0.0.1")
      (sql-user "burpee")
      (sql-password "burpee")
      (sql-database "burpee")
      (sql-port 3306))))
  :config
  (add-hook 'sql-mode-hook 'lsp)
  (setq lsp-sqls-workspace-config-path nil)
  (setq lsp-sqls-connections
        '(((driver . "mysql") (dataSourceName . "burpee:burpee@tcp(127.0.0.1:3306)/burpee")))))

        
(use-package tramp
  :straight t
  :config
  (connection-local-set-profile-variables
    'guix-system
    '((tramp-remote-path . (tramp-own-remote-path))))
   (connection-local-set-profiles
    `(:application tramp :protocol "sudo" :machine ,(system-name))
    'guix-system)
  (setq tramp-verbose 10)
  (setq emacs-persistence-directory (expand-file-name "var/" user-emacs-directory)))

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

(defvar counsel-network-manager-history nil
  "Network manager history.")

(defun counsel-network-manager (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "nmcli device wifi rescan")
  (let ((networks-list (s-split "\n" (shell-command-to-string "nmcli device wifi list"))))
    (consult--read networks-list
                   :prompt "Select network"
                   :initial initial-input
                   :require-match t
                   :history counsel-network-manager-history
                   :sort nil
                   :lookup (lambda (line &rest _)
                             (let ((network (car (s-split " " (s-trim (s-chop-prefix "*" line)) t))))
                               (message "Connecting to \"%s\".." network)
                               (async-shell-command
                                (format "sudo nmcli device wifi connect %s" (shell-quote-argument network))))))))

(defun dw/go-to-inbox ()
  (interactive)
  (mu4e-headers-search dw/mu4e-inbox-query))

(setq dw/mu4e-inbox-query
      "(maildir:/INBOX) AND flag:unread")

(defun dw/polybar-vpn-status ()
  (let ((vpn-status (replace-regexp-in-string "\n$" "" (shell-command-to-string
                     "nmcli --mode tabular --terse connection show --active | grep vpn | cut -d ':' -f1")))
        (result ""))
    (if (string-match "vpn" vpn-status)
        (setq result (format " %s" vpn-status)))
    (format "VPN %s" result)))

(defun dw/polybar-mail-count (max-count)
  (let ((mail-count (shell-command-to-string
                     (format "mu find --nocolor -n %s \"%s\" | wc -l" max-count dw/mu4e-inbox-query))))
    (if (string-match "no matches" (string-trim mail-count))
        (setq mail-count "0"))
    (format " %s" (string-trim mail-count))))

(use-package app-launcher
  :straight (:type git :host github :repo "SebastienWae/app-launcher"))

(defun dw/slack-messages-count ()
  (if (not (boundp 'slack-teams-by-token))
      (format " n/a")
    (let ((teams (hash-table-values slack-teams-by-token)))
    (when (< 0 (length teams))
      (setq alist
            (mapcar #'(lambda (e)
                        (cons (or (oref e modeline-name)
                                  (slack-team-name e))
                              (slack-team-counts-summary e)))
                    teams))))

  (mapconcat #'(lambda (e)
                 (let* ((team-name (car e))
                        (summary (cdr e))
                        (thread (cdr (cl-assoc 'thread summary)))
                        (channel (cdr (cl-assoc 'channel summary)))
                        (thread-mention-count (cdr thread))
                        (channel-mention-count (cdr channel))
                        (count-messages (+ channel-mention-count thread-mention-count)))
                   (format " %s" (number-to-string count-messages))))
             alist " ")))


(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-compton ()
  (interactive)
  (start-process "compton" "compton" "compton" "--backend" "glx" "--vsync" "opengl-swc"))

(defun efs/start-dunst ()
  (interactive)
  (start-process "dunst" "dunst" "dunst"))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process "polybar" "polybar" "polybar" "bar")))

(use-package exwm
  :straight (:type git :host github :repo "ch11ng/exwm")
  :init
  (require 'exwm)
  (require 'exwm-randr)
  (exwm-enable)
  (exwm-randr-mode)
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)
  (add-hook 'exwm-workspace-switch-hook #'dw/update-polybar-exwm)
  (add-hook 'exwm-randr-screen-change-hook #'efs/exwm-change-screen-hook)
  :config
  (defun efs/configure-window-by-class ()
    (interactive)
    (pcase (buffer-name)
      ("*shell*" (exwm-workspace-move 1))))

  (defun remove-whitespaces (string)
    (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

  (defun dw/update-polybar-exwm ()
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook mu4e 1" ))
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook vpn 1" ))
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook slack 1" )))
  (defun efs/exwm-init-hook ()
    (server-start)
    (exwm-workspace-switch-create 1)
    (efs/start-panel)
    (efs/start-dunst)
    (pixel-scroll-mode)
    (set-frame-parameter (selected-frame) 'alpha '(90 . 85))
    (add-to-list 'default-frame-alist '(alpha . (90 . 85))))
    (epa-file-enable)
    (pinentry-start)
    (direnv-mode)
    (about-emacs)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-input-global-keys
        `(([s-print] . desktop-environment-screenshot-part)
          ([s-escape] . desktop-environment-lock-screen)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ([?\s-&] . app-launcher-run-app)
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
	   "--output" (match-string 1) "--primary"  "--auto" "--above" default-output "--rotate" "normal"
	   "--output" default-output "--auto" "--rotate" "normal")
          (setq exwm-randr-workspace-monitor-plist (list 1 (match-string 1) 0 default-output))
          (efs/start-compton))))))

(use-package async
  :after bytecomp
  :hook ((after-init . async-bytecomp-package-mode)
	 (dired-mode . dired-async-mode)))

(use-package gcmh
  :init
  (gcmh-mode 1))

(use-package geiser-guile
  :mode (("\\.[Ss][Cc][Mm]\\'" . scheme-mode))
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/guix")
    (add-to-list 'geiser-guile-load-path "~/guix-system/.config/guix")))


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
  (orderless-component-separator "[ &]")
  (orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp))
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after vertico
  :bind (("C-x b"    . consult-buffer)
         ("C-x p"    . consult-project-buffer)
         ("C-x K"     . kill-current-buffer)
         ("C-x C-b"  . ibuffer)
         ("C-c s r"  . 'consult-ripgrep)
         ("C-c s l"  . 'consult-line)
         ("C-c s g"  . 'rgrep)
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

(use-package flyspell
  :straight (:type built-in)
  :init (flyspell-mode))

;; Dired extensions and utils
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-width 38))

(use-package image-dired)

;; Icons for dired sidebar
(use-package vscode-icon
  :straight t
  :after dired-sidebar)

;; Global customizations
;; ===============================================


(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  :init
  ;; Load theme
  (setq bespoke-set-theme 'dark)
  (load-theme 'bespoke t))

(use-package bespoke-modeline
  :straight (:type git :host github :repo "mclear-tools/bespoke-modeline") 
  :init
  ;; Set header line
  (setq bespoke-modeline-position 'bottom)
  ;; Set mode-line height
  (setq bespoke-modeline-size 3)
  ;; Show diff lines in mode-line
  (setq bespoke-modeline-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-modeline-cleaner t)
  ;; Use mode-line visual bell
  (setq bespoke-modeline-visual-bell t)
  ;; Set vc symbol
  (setq  bespoke-modeline-vc-symbol " branch: ")
  :init
  (bespoke-modeline-mode))

;; Vertical window divider
(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 3)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

(use-package fontset
  :straight (:type built-in) ;; only include this if you use straight
  :config
  ;; Use symbola for proper unicode
  (when (member "Symbola" (font-family-list))
    (set-fontset-font
     t 'symbol "Symbola" nil)))
        

(use-package eat
 :straight (:type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package simple-http
  :straight (:type git :host github :repo "/skeeto/emacs-web-server"))

(require 'simple-httpd)
(setq httpd-root "/home/nazar/Projects/klovanych.org/blog")
(httpd-start)

(use-package org-modern
  :straight (:type git :host github :repo "minad/org-modern")
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package org
  :straight (:type built-in))

(use-package logview
  :straight (:type git :host github :repo "doublep/logview"))

(use-package mu4e
  :after exwm
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
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (setq mu4e-alert-email-notification-types '(subjects))
  (mu4e-alert-set-default-style 'libnotify))

(use-package nerd-icons
  :straight (:type git :host github :repo "rainstormstudio/nerd-icons.el"))

(use-package lsp-mode
  :straight t
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]generated\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]pub\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]var\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]setup\\'")
  :hook
  (clojure-mode-hook . lsp)
  (clojurec-mode-hook . lsp)
  (clojurescript-mode-hook . lsp)
  (php-mode . lsp)
  (lsp-completion-mode . (lambda ()
                           (setq-local completion-category-defaults
                                       (assoc-delete-all 'lsp-capf completion-category-defaults))))
  (js2-mode . lsp))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
 :ensure t
 :config
 (defun just-one-face (fn &rest args)
   (let ((orderless-match-faces [completions-common-part]))
     (apply fn args)))
 (advice-add 'company-capf--candidates :around #'just-one-face) 
 (setq company-idle-delay 0.3)
 (global-company-mode 1))

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

;; Golang section
;; ===============================================

(use-package godoctor)
(use-package go-mode)
(use-package go-ts-mode
  :straight t
  :mode "\\.go\\'"
  :preface
  (defun vd/go-lsp-start()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (lsp-deferred))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path "~/.guix-home/profile/bin")
  (setq lsp-go-analyses '(
                          (nilness . t)
                          (shadow . t)
                          (unusedwrite . t)
                          (fieldalignment . t)
                          (escape . t)
                                       )
        lsp-go-codelenses '(
                          (test . t)
                          (tidy . t)
                          (upgrade_dependency . t)
                          (vendor . t)
                          (gc_details . t)
                          (run_govulncheck . t)))
  :hook
  (go-ts-mode . (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))
  (go-ts-mode . vd/go-lsp-start))

;; PHP settings
;; ===============================================

(use-package php-cs-fixer
  :after php-mode
  :straight (:type git :repo "Nazar65/emacs-php-cs-fixer")
  :hook ((before-save . php-cs-fixer-hook))
  :custom
  (php-cs-fixer-rules-config-file "/home/nazar/.config/php/.php-cs-fixer.dist.php")
  :config
  (defun php-cs-fixer-hook ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.php\\'" buffer-file-name))
    (add-hook 'before-save-hook 'php-cs-fixer-before-save nil 'make-it-local))))

(use-package php-doc-block
  :straight (:type git
                   :host github
                   :repo "moskalyovd/emacs-php-doc-block")
  :after php-mode
  :config
  (define-key php-mode-map (kbd "<C-tab>") 'php-doc-block))

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
  (add-hook 'php-mode-hook 'display-line-numbers-mode))

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

(use-package tree-sitter
  :hook ('php-mode . 'tree-sitter-mode)
  ('js2-mode . 'tree-sitter-mode)
  ('go-ts-mode . 'tree-sitter-mode))

(use-package tree-sitter-langs)

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
  :straight (:host github :repo "isamert/emacs-slack")
  :config
  (url-cookie-store
   "d"
   (auth-source-pick-first-password
    :host "atwix.slack.com"
    :user "cookie" :type 'netrc :max 1)
   nil ".slack.com" "/" t)
  (slack-register-team
   :name "atwix"
   :modeline-enabled nil
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
  (setq slack-alert-icon "/home/nazar/.emacs.d/static/slack/icon.png")
  (setq slack-enable-global-mode-string t)
  (setq slack-buffer-emojify t)
  (setq slack-render-image-p t)
  (setq slack-prefer-current-team t)
  (define-key ctl-x-map "j" #'slack-select-rooms)
  (define-key ctl-x-map "l" #'slack-select-unread-rooms)
  (define-key slack-mode-map "@"
              (defun endless/slack-message-embed-mention ()
                (interactive)
                (call-interactively #'slack-message-embed-mention)
                (insert " ")))
  (define-key slack-mode-map (kbd "C-c C-e")
              #'slack-message-edit)
  (advice-add 'slack-message-notify-alert :before
	      (lambda(message room team)
		(if (slack-message-notify-p message room team)
		    (async-start
		     (lambda ()
		       (play-sound-file "~/guix-system/Sounds/Slack-Notification-Tone.au")
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
  :straight (:type git :host github :repo "magit/magit" branch: "master" (:defaults "lisp/*"))
  :config
  (require 'git-commit)
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
  :mode (("\\.js$" . js2-mode)))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)

