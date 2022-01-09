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
    #:start (make-forkexec-constructor '("pulseaudio" "-D"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(define pulseaudio-equalizer
  (make <service>
    #:provides '(pulseaudio-equalizer)
    #:docstring "Enable pulseaudio-equalizer"
    #:start (make-forkexec-constructor '("pulseaudio-equalizer" "enable"))
    #:stop (make-kill-destructor)
    #:respawn? #t))

(register-services notification-daemon)
(register-services pulseaudio-daemon)
(register-services pulseaudio-equalizer)

(for-each start '(notification-daemon pulseaudio-daemon pulseaudio-equalizer))
