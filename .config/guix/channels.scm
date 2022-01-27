(cons* (channel
        (name 'guix-phps)
        (url "https://github.com/Nazar65/guix-phps-channel")
        (introduction
         (make-channel-introduction
          "84992e5f256c73f091d3e9828a1546a11721f4d1"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       (channel
        (name 'my-guix-channel)
        (url "https://github.com/Nazar65/my-guix-packages-channel")
        (introduction
         (make-channel-introduction
          "df9199652d4def1e4794d724fbe648a83256039b"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       %default-channels)
