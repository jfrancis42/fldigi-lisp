;;;; fldigi.asd

(asdf:defsystem #:fldigi
  :description "A library for talking to FLDigi"
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:s-xml-rpc
	       #:babel
	       #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "fldigi")))

