;;; mu4e-config.el --- Config for mu4e. -*- lexical-binding: t -*-

;;; Code:
;;; Message and smtp settings

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-attachment-dir  "~/Downloads/MailAttachments")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-maildir-shortcuts
    '((:maildir "/INBOX"              :key ?i)
      (:maildir "/[Gmail].Sent Mail"  :key ?s)
      (:maildir "/[Gmail].Trash"      :key ?t)
      (:maildir "/[Gmail].All Mail"   :key ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "n.klovanych@atwix.com"
   user-full-name  "Nazar Klovanych"
   mu4e-compose-signature
    (concat
     "Nazar Klovanych | "
     "Technical Lead at "
     "<#part type=text/html><a href='https://www.atwix.com/'>Atwix</a><#/part> \n"
     "<#part type=text/html><br><br><img src='https://www.atwix.com/assets/atwix_logo.png' width='96' height='42'><#/part>"))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "n.klovanych@atwix.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-user "n.klovanych@atwix.com"
   smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'mu4e-config)

;;; mu4e-config.el ends here
