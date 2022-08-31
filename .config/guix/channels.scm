(cons* (channel
        (name 'guix-phps)
        (url "https://github.com/Nazar65/guix-phps-channel")
        (introduction
         (make-channel-introduction
          "bce6d8b460198458eb75ee3c277b8595755283de"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       (channel
        (name 'my-guix-channel)
        (url "https://github.com/Nazar65/my-guix-packages-channel")
        (introduction
         (make-channel-introduction
          "1a12779ad12a201b9d03013e1060012ba3cd4417"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       %default-channels)
