(define emacs-daemon
  (make <service>
    #:provides '(emacs-daemon)
    #:docstring "Run `emacs' as daemon"
    #:start (make-forkexec-constructor '("emacs" "--fg-daemon"))
    #:stop (make-kill-destructor)
    #:respawn? #t))
(register-services emacs-daemon)

(start emacs-daemon)
