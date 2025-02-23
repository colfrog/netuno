(asdf:defsystem netuno
    :version "0.0.1"
    :author "Laurent Cimon <laurent@nilio.ca>"
    :maintainer "Laurent Cimon <laurent@nilio.ca>"
    :license "bsd-2-clause"
    :description "A game of uno on raw sockets"
    :components ((:file "package")
		 (:file "uno")
		 (:file "game")
		 (:file "server")))
