(in-package :nyxt-user)

;;loading of config files
(nyxt::load-lisp "~/.config/nyxt/themes/standard-dark.lisp")

(defvar autofills
  (list
   (make-autofill :name "example" :fill "here")))

;;configuration for browser
(define-configuration browser
    ((session-restore-prompt :always-restore)
     #+nyxt-2
     (autofills autofills)))

;;default modes for buffer and nosave buffer
(define-configuration (web-buffer nosave-buffer)
  ((default-modes `(reduce-tracking-mode
                    blocker-mode
                    ,@%slot-default%))))

(define-configuration buffer
    ((smooth-scrolling nil)
     (search-always-auto-complete-p nil)
     (override-map (let ((map (make-keymap "my-override-map")))
                     (define-key map
                         "C-b d" 'delete-buffer
                         "C-b c" 'make-buffer
                         "C-b b" 'set-url-from-bookmark
                         "C-p c" 'copy-password
                         "C-n c" 'copy-username
                         "C-d i" 'open-inspector
                         "C-s" 'query-selection-in-search-engine)
                     map))))

#+nyxt-3
(define-configuration nyxt/autofill-mode:autofill-mode
    ((nyxt/autofill-mode:autofills autofills)))

(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
  ((nyxt/reduce-tracking-mode:preferred-user-agent
    ;; Safari on Mac. Taken from
    ;; https://techblog.willshouse.com/2012/01/03/most-common-user-agents
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.3 Safari/605.1.15")))

(define-mode chrome-mimick-mode (nyxt/reduce-tracking-mode:reduce-tracking-mode)
  "A simple mode to set Chrome-like Windows user agent."
  ((nyxt/reduce-tracking-mode:preferred-user-agent
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36")))

(define-mode firefox-mimick-mode (nyxt/reduce-tracking-mode:reduce-tracking-mode)
  "A simple mode to set Firefox-like Linux user agent."
  ((nyxt/reduce-tracking-mode:preferred-user-agent
    "Mozilla/5.0 (X11; Linux x86_64; rv:94.0) Gecko/20100101 Firefox/94.0")))

;;when reloading init.lisp file shows in message bar once finished
(echo "Loaded config.")