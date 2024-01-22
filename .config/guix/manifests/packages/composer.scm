(define-module (composer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages php)
  #:export (composer))

(define composer
  (package
    (name "composer")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://getcomposer.org/download/"
                                  version
                                  "/composer.phar"))
              (sha256
               (base32
                "19nwlvc2sjdxskxlawvnqafnn2c8fld8861qbijgjvy7iqqgd37a"))))
    (build-system trivial-build-system)
    (propagated-inputs (list php))
    (arguments (list
                #:modules '((guix build utils))
                #:builder
                #~(begin
                    (use-modules (guix build utils))
                    (copy-file #$source "./composer")
                    (let ((out (string-append #$output "/bin")))
                      (mkdir-p out)
                      (chmod "./composer" #o555)
                      (copy-file
                       "./composer"
                       (string-append out "/composer"))))))
    (home-page "https://getcomposer.org/")
    (synopsis "Manage dependencies for PHP projects")
    (description "Composer is a tool for dependency management in PHP. It allows
 you to declare the libraries your project depends on and it will manage
(install/update) them for you.")
    (license license:expat)))

composer
