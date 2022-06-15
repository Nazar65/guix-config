(in-package :nyxt-user)

;;loading of config files
(nyxt::load-lisp "~/.config/nyxt/themes/standard-dark.lisp")

;;configuration for browser
(define-configuration browser
  ((session-restore-prompt :always-ask)))

;;default modes for buffer and nosave buffer
(define-configuration (web-buffer nosave-buffer)
  ((default-modes `(reduce-tracking-mode
                    blocker-mode
                    ,@%slot-default%))))

(define-configuration buffer
  ((smooth-scrolling nil)
   (override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                         "C-b d" 'delete-buffer
                         "C-c i" 'open-inspector
                         "C-s" 'query-selection-in-search-engine
                         "C-c" 'nyxt/web-mode::copy)
                     map))))

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
