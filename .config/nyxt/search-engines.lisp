(in-package #:nyxt-user)
(in-package #:nyxt)
;; Define buffer search-engines slot to be a list of several
;; nx-search-engines-provided ones.
(define-configuration (buffer web-buffer)
    ((search-engines (list
                      (engines:google :shortcut "google"
                                      :safe-search nil)
		      (engines:invidious :shortcut "video")
                      (engines:duckduckgo :theme :dark
					  :shortcut "duck"
                                          :help-improve-duckduckgo nil
                                          :homepage-privacy-tips nil
                                          :privacy-newsletter nil
                                          :newsletter-reminders nil
                                          :install-reminders nil
                                          :install-duckduckgo nil)
		      (engines:github
		       :object :code)
		      (engines:startpage
		       :shortcut "startpage")
		      (engines:searx :shortcut "searx")))))

(define-command-global query-text-in-search-engine
    (&key (query-in-new-buffer-p t))
  "Search text using the queried search engine."
  (let* ((query
	  (first
	   (prompt :prompt "Search Query"
		   :sources (make-instance 'prompter:raw-source
                                            :name "Query"))))
	 (engine
          (first
           (prompt :prompt "Search engine"
		   :sources
                   (make-instance 'search-engine-source))))
         (target-buffer
          (if query-in-new-buffer-p
              (make-buffer-focus)
              (current-buffer))))
    (when engine
      (buffer-load
       (make-instance 'new-url-query :query query :engine engine) :buffer
       target-buffer))))
