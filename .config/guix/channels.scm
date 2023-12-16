(cons* (channel
        (name 'guix-phps)
	(branch "master")
        (url "https://git-space.klovanych.org/guix-phps-channel")
        (introduction
         (make-channel-introduction
          "296e82e7ffea18468c5ac4dfe9db73d8e5e5c5fd"
          (openpgp-fingerprint
           "4441 0743 6E32 EE9F F20A  753F B510 AA5B 74EA F294"))))
       (channel
        (name 'my-guix-channel)
        (url "https://git-space.klovanych.org/guix-packages-channel")
        (introduction
         (make-channel-introduction
          "cd48ca3b1cf0331a7d4cda7e79d4792ee2c1e57f"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
       %default-channels)
