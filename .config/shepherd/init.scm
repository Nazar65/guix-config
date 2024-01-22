(use-modules (shepherd service))

(define notification-daemon
  (make <service>
    #:provides '(notification-daemon)
    #:docstring "Run notification-daemon"
    #:start (make-forkexec-constructor '((string-append (getenv "HOME") ".guix-profile/libexec/notification-daemon" "-r" "&")))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(define xcompmgr-daemon
  (make <service>
    #:provides '(xcompmgr-daemon)
    #:docstring "Run xcompmgr-daemon"
    #:start (make-forkexec-constructor '("xcompmgr" "-c" "-t-6" "-l-6" "-o.1" "&"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(define pantalaimon-daemon
  (make <service>
    #:provides '(pantalaimon-daemon)
    #:docstring "Run pantalaimon-daemon"
    #:start (make-forkexec-constructor '("pantalaimon-daemon"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services xcompmgr-daemon pantalaimon-daemon)

(for-each start '(xcompmgr-daemon pantalaimon-daemon))
