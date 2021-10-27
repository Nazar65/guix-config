(setq user-mail-address	"nazarn96@gmail.com"
      user-full-name	"Nazar Klovanych")

(setq gnus-secondary-select-methods '((nnimap "gmail"
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

(require 'smtpmail)
