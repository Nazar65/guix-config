(use-modules (shepherd service))

(define notification-daemon
  (make <service>
    #:provides '(notification-daemon)
    #:docstring "Run notification-daemon"
    #:start (make-forkexec-constructor '("/home/nazar/.guix-profile/libexec/notification-daemon"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(define pulseaudio-daemon
  (make <service>
    #:provides '(pulseaudio-daemon)
    #:docstring "Run pulseaudio-daemon"
    #:start (make-forkexec-constructor '("pulseaudio" "-vvv"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services notification-daemon)
(register-services pulseaudio-daemon)

(for-each start '(notification-daemon pulseaudio-daemon pulseaudio-equalizer))
