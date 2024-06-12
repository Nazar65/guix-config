(use-modules (packages composer)
	     (packages php))

(specifications->manifest '("php@8.1"
			    "composer"
                            "node"
                            "phpfixer"
			    "mysql"))
