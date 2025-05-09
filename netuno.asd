(asdf:defsystem netuno
  :version "0.0.4"
  :author "Laurent Cimon <laurent@nilio.ca>"
  :maintainer "Laurent Cimon <laurent@nilio.ca>"
  :license "bsd-2-clause"
  :description "A game of uno on raw sockets"
  :components ((:file "package")
	       (:file "uno")
	       (:file "server"))
  :depends-on (#:usocket #:bordeaux-threads #:cl-ppcre #:flexi-streams)
  :build-operation "program-op"
  :build-pathname "netuno"
  :entry-point "netuno:start-and-wait")
