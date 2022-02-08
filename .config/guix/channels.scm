(cons* (channel
        (name 'guix-phps)
        (url "https://github.com/Nazar65/guix-phps-channel")
        (introduction
         (make-channel-introduction
          "0f4074aab0ed83f8d0ca9f5d0284db0bb45f7e99"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       (channel
        (name 'my-guix-channel)
        (url "https://github.com/Nazar65/my-guix-packages-channel")
        (introduction
         (make-channel-introduction
          "5e8ba5ce350d28a5fefcbfb7ed87910b0d3be430"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       %default-channels)
