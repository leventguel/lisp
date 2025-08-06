(load "/home/inline/quicklisp/setup.lisp")

(asdf:initialize-source-registry
 `(:source-registry
   (:tree "/home/inline/source/quicklisp/software/")
   (:tree "/home/inline/prg/lisp/lisp/")
   :default-registry
   :inherit-configuration))

(asdf:initialize-output-translations
 `(:output-translations
   #.(let ((wild-subdir
            (make-pathname :directory '(:relative :wild-inferiors)))
           (wild-file
            (make-pathname :name :wild :version :wild :type :wild)))
       `((:root ,wild-subdir ,wild-file)
         (:user-cache ,wild-subdir ,wild-file)))
   :inherit-configuration))

(asdf:load-system :mcclim)
(in-package :clim-user)

(defun test-func (type1 type2)
  (format t "test-method called with types: ~a ~40t ~a~%" type1 type2)
  (values))

(defgeneric test-method (obj1 obj2)
  (:method ((obj1 integer) (obj2 integer))
   (test-func 'integer 'integer))
  (:method ((obj1 integer) (obj2 number))
   (test-func 'integer 'number))
  (:method ((obj1 number) (obj2 integer))
   (test-func 'number 'integer))
  (:method ((obj1 number) (obj2 number))
   (test-func 'number 'number))
  (:method (obj1 obj2)
   (test-func 't 't)))

(test-method 1 2)

(test-method 1.0 2.0)

(test-method 'one 'two)

;(let ((*standard-output* (frame-standard-output *application-frame*)))
;(format t "applicable methods: ~a~%" (compute-applicable-methods #'test-method '(1 2))))


;(defvar mystream (frame-standard-output *application-frame*))
;(let ((*standard-output* (make-synonym-stream 'mystream)))
;  (format *standard-output* "applicable methods: ~a~&" (compute-applicable-methods #'test-method '(1 2))))

(define-presentation-type mycons ())

(define-presentation-method present
  (object (type mycons) stream view &key &allow-other-keys)
  object)

(let ((mystream (or (frame-standard-output *application-frame*) *standard-output*))
      (result (compute-applicable-methods #'test-method '(1 2))))
  (format t "applicable methods:")
  (with-output-as-presentation (mystream result (presentation-type-of result) :single-box t)
			       (with-drawing-options (mystream :ink +olivedrab+)
						     (format t "~%~a" result))))
