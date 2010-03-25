(in-package :jsonrpc)

;;; TODO: surrogate pair

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun formalize-letargs (args)
    (mapcar (lambda (a) (if (atom a) (list a) a)) args)))

(defmacro nlet (fn-name letargs &body body)
  (setf letargs (formalize-letargs letargs))
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

;; inline宣言
(declaim (inline  char2hex read-char-skip-whitespace 2byte-code-char
                  char-json-whitespace? char-json-bool? char-json-number?))
 
;; XXX:
(defvar *read-buffer* (make-array 8192 :element-type 'character))

(defun char-json-whitespace? (ch)
  (case ch ((#\Space #\Tab #\Return #\Newline) t)))

(defun char-json-bool? (ch)
  (case ch (#.(coerce (remove #\Space (remove-duplicates "true false null")) 'list) t)))

(defun char-json-number? (ch)
  (case ch (#.(coerce "0123456789.eE-+" 'list) t)))

(defun read-char-skip-whitespace (input-stream)
  (loop FOR     ch = (read-char input-stream)
        WHILE   (char-json-whitespace? ch)
        FINALLY (return ch)))

(defun read-while (fn in &aux (i -1))
  (declare (fixnum i) (function fn)
           ((simple-array character) *read-buffer*))
  (loop FOR     ch = (read-char in nil #\Space)
        WHILE   (funcall fn ch)
        DO      (setf (aref *read-buffer* (incf i)) ch)
        FINALLY (unread-char ch in))
  (subseq *read-buffer* 0 (1+ i)))


;; json
(defun read-json (in)
  (read-value (read-char-skip-whitespace in) in))

;; value
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar num-prefixs '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)))
(defun read-value (ch in)
  (case ch
    ((#\{)         (read-object  ch in))
    ((#\[)         (read-array   ch in))
    ((#\")         (read-string  ch in))
    (#.num-prefixs (read-number  ch in))
    (t             (read-boolean ch in))))

;; true,false,null
(defun read-boolean (ch in)
  (let ((tkn (read-while #'char-json-bool? in)))
    (case ch
      (#\t (and (string= tkn "rue")  (return-from read-boolean t)))
      (#\f (and (string= tkn "alse") (return-from read-boolean nil)))
      (#\n (and (string= tkn "ull")  (return-from read-boolean nil))))
    (error "~A~A is undefined literal" ch tkn)))

;; number  ※ read-from-stringにほぼ全てを任せているので、JSONには定義されていない数値も(common lispの数値として合法なら)読み込めてしまう
(defun read-number (ch in)
  (unread-char ch in)
  (let ((num (read-from-string (read-while #'char-json-number? in))))
    (check-type num number)
    num))

;; string
(defun read-string (ch in &aux (i -1))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ignore ch) (fixnum i)
           ((simple-array character) *read-buffer*))
  (nlet self ((ch #1=(read-char in t)))
    (case ch
      ;; エスケープ文字
      (#\\ (setf (aref *read-buffer* (incf i))
                 (let ((ch2 (read-char in)))
                   (case ch2
                     (#\b #\Backspace) (#\f #\Page)   (#\t #\Tab)
                     (#\n #\Newline)   (#\r #\Return)
                     (#\u (read-utf16-char in))
                     (t ch2))))
           (self #1#))
      ;; 文字列終了
      (#\")
      ;; 通常の文字
      (t (setf (aref *read-buffer* (incf i)) ch)
         (self #1#))))
  (subseq *read-buffer* 0 (1+ i)))

;; 文字を16進数と解釈して数字に変換する
(defun char2hex (ch)
  (ecase ch
    (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) 
    (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
    ((#\a #\A) 10)
    ((#\b #\B) 11)
    ((#\c #\C) 12)
    ((#\d #\D) 13)
    ((#\e #\E) 14)
    ((#\f #\F) 15)))

;; UTF-16beの値を、characterに変換する  
;; sbclの場合は、code-charを呼び出すだけで大丈夫
;; ※ 処理系依存部分
(defun 2byte-code-char (2byte-code)
  (code-char 2byte-code))

;; "\u0061"といったようなエスケープされたユニコード文字を読み込む
(defun read-utf16-char (in)
  '#1=(the (unsigned-byte 8) (char2hex (read-char in t)))
  (2byte-code-char (logior (ash #1# 12) (ash #1# 8) (ash #1# 4) #1#)))

;; pair = "key":value
(defun read-pair (ch in)
  (assert (char= ch #\") () "object key must start with ~S ." #\")
  (cons (prog1 (intern (string-upcase (read-string #\" in)) :keyword)
          (assert (char= (read-char-skip-whitespace in) #\:) () 
                  "object key and value must be delimited by ~S ." #\:))
        (read-value (read-char-skip-whitespace in) in)))

;; object, array
           ;; 共通部分
(macrolet ((common-block (read-fn end-ch &rest err-msgs)
             `(let ((ch #1=(read-char-skip-whitespace in)))
                (unless (char= ch ,end-ch)
                  (do ((seq (list (,read-fn ch in)) (cons (,read-fn #1# in) seq))
                       (ch #1# #1#))
                      ((char/= ch #\,)
                       (assert (char= ch ,end-ch) () ,@err-msgs)
                       (nreverse seq)))))))
  ;; object
  (defun read-object (ch in)
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (ignore ch))
    (common-block read-pair  #\} "missing close brace (~S)." #\}))

  ;; array
  (defun read-array (ch in)
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (ignore ch))
    (common-block read-value #\] "missing close bracket (~S)." #\])))