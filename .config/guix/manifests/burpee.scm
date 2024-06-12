(use-modules (packages composer)
	     (packages php)
             (packages phpfixer))

(specifications->manifest '("php@8.1"
			    "composer"
                            "mysql"
                            "phpfixer"
			    "node"))
