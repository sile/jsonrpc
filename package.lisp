(defpackage jsonrpc
  (:use :common-lisp :usocket)
  (:shadow close)
  (:export *json-ppd*
	   write-json
	   
	   read-json
	   
	   *client*
	   client-connect
	   client-close
	   call
	   call2))

  