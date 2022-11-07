(cons* (channel
        (name 'guix-phps)
        (url "https://github.com/Nazar65/guix-phps-channel")
        (introduction
         (make-channel-introduction
          "0aa82aeb8414d83b2910a93ca97f8f9055744eef"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       (channel
        (name 'my-guix-channel)
        (url "https://github.com/Nazar65/my-guix-packages-channel")
        (introduction
         (make-channel-introduction
          "8a605c4ab11ae8cbc8ff157e8ac1cacd8a495562"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       %default-channels)
