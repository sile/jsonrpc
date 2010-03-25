(in-package :jsonrpc)

(defstruct client
  socket)

(defun client-close (client)
  (socket-close (client-socket client)))

(defvar *client*)
(defun client-connect (host port)
  (setf *client* 
	(make-client :socket (socket-connect host port))))

;; TODO: (defun notify ())

(defun call (id method params &optional (client *client*))
  (with-slots (socket) client
    (let ((stream (socket-stream socket)))
      (write-json `((jsonrpc . "2.0") (id . ,id) (method . ,method) (params . ,params)) stream)
      (force-output stream)
      
      (read-json stream))))

(defmacro a.when (exp &body body)
  `(let ((it ,exp))
     (when it
       ,@body)))
     
(defun call2 (method params &key (client *client*) (id 0))
  (with-slots (socket) client
    (let ((stream (socket-stream socket)))
      (write-json `((jsonrpc . "2.0") (id . ,id) (method . ,method) ,@params) stream)
      (force-output stream)
      
      (let ((res (read-json stream)))
	(a.when (cdr (assoc :error res))
	  (error "~S" it))
	  
	(cdr (assoc :result res))))))