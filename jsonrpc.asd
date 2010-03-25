(in-package :asdf)

(defsystem jsonrpc
  :name "jsonrpc"
  :version "0.0.1"
  :author "Takeru Ohta"
  :description "A Common lisp implementation of JSON-RPC-2.0"
  
  :serial t 
  :depends-on (:usocket)
  :components ((:file "package")
	       (:file "write-json")
	       (:file "read-json")
	       (:file "client")))