(cons* (channel
        (name 'guix-phps)
        (url "https://github.com/Nazar65/guix-phps-channel")
        (introduction
         (make-channel-introduction
          "f07085eaa69a4657cdcba30329b9c02bf99104af"
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
       (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
