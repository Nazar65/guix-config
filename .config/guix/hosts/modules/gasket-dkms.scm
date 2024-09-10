(define-module (gasket-dkms)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu system)
  #:use-module (guix build-system linux-module)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public gasket-dkms
  (package
   (name "gasket-dkms")
   (version "1.0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/google/gasket-driver")
                  (commit "5815ee3908a46a415aac616ac7b9aedcb98a504c")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1zv1n6lpgc6ay9i6w5q38nbaj3jpk3x0xxfmnlwzdm94radgwpiv"))))
   (build-system linux-module-build-system)
   (arguments
    (list #:tests? #f
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'chdir
                          (lambda _
                            (chdir "src"))))))   
   (home-page "https://coral.ai/docs/notes/build-coral/#build-the-edge-tpu-python-api")
   (synopsis "Driver for Mini PCIe or M.2 Accelerator cards")
   (description
    "This is required only for PCIe devices such as the Accelerator Module and Mini PCIe or M.2 Accelerator cards.")
   (license license:gpl3+)))
