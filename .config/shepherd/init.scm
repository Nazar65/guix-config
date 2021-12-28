(use-modules (shepherd service))

(define notification-daemon
  (make <service>
    #:provides '(notification-daemon)
    #:docstring "Run notification-daemon"
    #:start (make-forkexec-constructor '("/home/nazar/.guix-profile/libexec/notification-daemon"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services notification-daemon)

(for-each start '(notification-daemon))
