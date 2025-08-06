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

(defclass a ()
  (slot1
   slot2))

(defclass b ()
  (slot1
   slot2))

(defclass c (a b)
		(slot1))

(defclass d (c)
  (slot2))

(let* ((mystream *standard-output*)
       (instance-c (make-instance 'c))
       (instance-d (make-instance 'd))
       (result (slot-value (class-of instance-c) 'sb-pcl::slots)))
  (declare (ignore instance-d))
  (terpri)
  ;(describe (class-of instance-c))
		(format mystream "~&~a~%~a" (first result) (second result)))

(let* ((mystream *standard-output*)
       (instance-c (make-instance 'c))
       (instance-d (make-instance 'd))
       (result (with-slots (sb-pcl::slots) (class-of instance-d) sb-pcl::slots)))
  (declare (ignore instance-d))
  (terpri)
  ;(describe (class-of instance-c))
  ;(format mystream "~&~a" (describe (first result)))
		(format mystream "~&slot allocation for slot ~a is from: ~%~a" (first result) (slot-value (first result) 'sb-pcl::allocation-class))
		(format mystream "~&slot allocation for slot ~a is from: ~%~a" (second result) (slot-value (second result) 'sb-pcl::allocation-class))
		(format mystream "~&class-precedence-list for ~a:~% ~a~%" instance-d
										(mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) (sb-pcl::class-precedence-list (class-of instance-d)))))
