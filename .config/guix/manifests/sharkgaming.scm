(use-modules (packages composer)
	     (packages php)
             (packages phpfixer))

(specifications->manifest '("php@8.2"
			    "composer"
                            "mysql"
                            "phpfixer"
			    "node"))
