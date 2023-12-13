(use-modules (shepherd service))

(define notification-daemon
  (make <service>
    #:provides '(notification-daemon)
    #:docstring "Run notification-daemon"
    #:start (make-forkexec-constructor '("/home/nazar/.guix-profile/libexec/notification-daemon" "-r" "&"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(define xcompmgr-daemon
  (make <service>
    #:provides '(xcompmgr-daemon)
    #:docstring "Run xcompmgr-daemon"
    #:start (make-forkexec-constructor '("xcompmgr" "-c" "-t-6" "-l-6" "-o.1" "&"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services notification-daemon xcompmgr-daemon)

(for-each start '(notification-daemon xcompmgr-daemon))
