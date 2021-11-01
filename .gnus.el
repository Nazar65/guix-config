(setq user-mail-address	"nazarn96@gmail.com"
      user-full-name	"Nazar Klovanych")

(setq gnus-secondary-select-methods '(
                                      (nnml "local.mail")
                                      (nntp "news.gnus.org")
                                      (nnimap "gmail"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port "imaps")
                                              (nnimap-stream ssl))
                                      (nnimap "i4"
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port "imaps")
                                              (nnimap-stream ssl))
                                      ))
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")

(gnus-demon-add-handler 'gnus-group-get-new-news 1 nil)
(setq mm-text-html-renderer 'w3m)
(setq w3m-fill-column 100)
(setq gnus-select-method '(nnnil))
(require 'smtpmail)
(require 'gnus-notify)
(setq group-name-map '(("nnml+local.mail:mail.misc" . "Local")
                       ("nnimap+gmail:INBOX" . "Gmail")
                       ("nnimap+i4:INBOX" . "i4")))
(setq mail-sources
      '((file :path "/var/spool/mail/nazar")))

(setq gnus-parameters
      '(("INBOX"
         (gnus-use-adaptive-scoring nil)
         (gnus-use-scoring nil)
         (visible . t)
         (display . all)
         (modeline-notify . t)
         )
        ("mail.misc"
         (gnus-use-adaptive-scoring nil)
         (gnus-use-scoring nil)
         (visible . t)
         (display . all)
         (modeline-notify . t)
         )))
(gnus-demon-init)
