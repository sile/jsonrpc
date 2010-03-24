(in-package :jsonrpc)

(defvar *json-ppd* (copy-pprint-dispatch nil))

(defmacro def-json-ppd (type (obj &rest rest) &body body)
  `(set-pprint-dispatch
    ',type
    (lambda (*standard-output* ,obj ,@rest)
      (declare (ignorable ,obj))
      ,@body)
    0
    *json-ppd*))

(let ((fn (pprint-dispatch 'string (copy-pprint-dispatch nil))))
  (defun write-quoted-string (string)
    (funcall fn *standard-output* string)))

(defun write-pair (stream pair &rest _)
  (declare (ignore _))
  (format stream "~S:~S" (car pair) (cdr pair)))

(defun alist-p (lst)
  (and (consp (car lst)) (atom (cdar lst))))

(deftype literal  () '(member :true :false :null))
(deftype true () '(eql t))

(def-json-ppd literal (obj) (write-string (string-downcase (symbol-name obj))))
(def-json-ppd true    (obj) (write-string "true"))
(def-json-ppd string  (str) (write-quoted-string str))
(def-json-ppd symbol  (sym) (write-quoted-string (string-downcase (symbol-name sym))))

(def-json-ppd vector (vec)
  (write-string "[")
  (dotimes (i (length vec))
    (unless (zerop i) (write-string ","))
    (write (aref vec i) :pprint-dispatch *json-ppd*))
  (write-string "]"))

(def-json-ppd list (lst)
  (if (alist-p lst)
      (format t "{~{~/jsonrpc::write-pair/~^,~}}" lst)
    (format t "[~{~S~^,~}]" lst)))

(def-json-ppd hash-table (hash &aux (first t))
  (write-string "{")
  (maphash 
   (lambda (k v)
     (unless first (write-string ","))
     (format t "~S:~S" k v)
     (setf first nil))
   hash)
  (write-string "}"))

(defun write-json (obj &optional (stream *standard-output*))
  (write obj :stream stream :pprint-dispatch *json-ppd*))