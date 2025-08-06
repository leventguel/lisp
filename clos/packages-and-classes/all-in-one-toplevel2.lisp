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
(declaim (sb-ext:muffle-conditions cl:warning))
(handler-bind ((warning #'sb-ext::muffle-warning))
		
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

(defvar mystream nil)
(setf mystream (or *standard-output* (frame-standard-output *application-frame*))
      instance-a (make-instance 'a)
      instance-b (make-instance 'b)
      instance-c (make-instance 'c)
      instance-d (make-instance 'd)
      class-of-a (slot-value (class-of instance-a) 'sb-pcl::name)
      class-of-b (slot-value (class-of instance-b) 'sb-pcl::name)
      class-of-c (slot-value (class-of instance-c) 'sb-pcl::name)
      class-of-d (slot-value (class-of instance-d) 'sb-pcl::name)
      slots-in-a (with-slots (sb-pcl::slots) (class-of instance-a) sb-pcl::slots)
      slots-in-b (with-slots (sb-pcl::slots) (class-of instance-b) sb-pcl::slots)
      slots-in-c (with-slots (sb-pcl::slots) (class-of instance-c) sb-pcl::slots)
      slots-in-d (with-slots (sb-pcl::slots) (class-of instance-d) sb-pcl::slots)
      first-from-a (slot-value (first slots-in-a) 'sb-pcl::allocation-class)
      first-from-b (slot-value (first slots-in-b) 'sb-pcl::allocation-class)
      first-from-c (slot-value (first slots-in-c) 'sb-pcl::allocation-class)
      first-from-d (slot-value (first slots-in-d) 'sb-pcl::allocation-class)
      second-from-a (slot-value (second slots-in-a) 'sb-pcl::allocation-class)
      second-from-b (slot-value (second slots-in-b) 'sb-pcl::allocation-class)
      second-from-c (slot-value (second slots-in-c) 'sb-pcl::allocation-class)
      second-from-d (slot-value (second slots-in-d) 'sb-pcl::allocation-class)
      cpl-a (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
		     (sb-pcl::class-precedence-list (class-of instance-a)))
      cpl-b (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
		     (sb-pcl::class-precedence-list (class-of instance-b)))
      cpl-c (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
		     (sb-pcl::class-precedence-list (class-of instance-c)))
      cpl-d (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
		     (sb-pcl::class-precedence-list (class-of instance-d))))

(defun my-info ()
  (let* ((*print-pretty* nil)
	 (mystream (or *standard-output* (frame-standard-output *application-frame*)))
	 (instance-a (make-instance 'a))
	 (instance-b (make-instance 'b))
	 (instance-c (make-instance 'c))
	 (instance-d (make-instance 'd))
	 (class-of-a (slot-value (class-of instance-a) 'sb-pcl::name))
	 (class-of-b (slot-value (class-of instance-b) 'sb-pcl::name))
	 (class-of-c (slot-value (class-of instance-c) 'sb-pcl::name))
	 (class-of-d (slot-value (class-of instance-d) 'sb-pcl::name))
	 (slots-in-a (with-slots (sb-pcl::slots) (class-of instance-a) sb-pcl::slots))
	 (slots-in-b (with-slots (sb-pcl::slots) (class-of instance-b) sb-pcl::slots))
	 (slots-in-c (with-slots (sb-pcl::slots) (class-of instance-c) sb-pcl::slots))
	 (slots-in-d (with-slots (sb-pcl::slots) (class-of instance-d) sb-pcl::slots))
	 (first-from-a (slot-value (first slots-in-a) 'sb-pcl::allocation-class))
	 (first-from-b (slot-value (first slots-in-b) 'sb-pcl::allocation-class))
	 (first-from-c (slot-value (first slots-in-c) 'sb-pcl::allocation-class))
	 (first-from-d (slot-value (first slots-in-d) 'sb-pcl::allocation-class))
	 (second-from-a (slot-value (second slots-in-a) 'sb-pcl::allocation-class))
	 (second-from-b (slot-value (second slots-in-b) 'sb-pcl::allocation-class))
	 (second-from-c (slot-value (second slots-in-c) 'sb-pcl::allocation-class))
	 (second-from-d (slot-value (second slots-in-d) 'sb-pcl::allocation-class))
	 (cpl-a (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
			(sb-pcl::class-precedence-list (class-of instance-a))))
	 (cpl-b (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
			(sb-pcl::class-precedence-list (class-of instance-b))))
	 (cpl-c (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
			(sb-pcl::class-precedence-list (class-of instance-c))))
	 (cpl-d (mapcar (lambda (obj) (slot-value obj 'sb-pcl::name)) 
			(sb-pcl::class-precedence-list (class-of instance-d)))))
    
    (with-drawing-options (mystream :ink +red+)
                          (with-output-as-presentation 
                           (mystream instance-a (presentation-type-of instance-a))
                           (with-output-as-presentation
                            (mystream class-of-a (presentation-type-of class-of-a))
                            (with-output-as-presentation
                             (mystream (first slots-in-a) (presentation-type-of (first slots-in-a)))
                             (with-output-as-presentation
                              (mystream first-from-a (presentation-type-of first-from-a))
                              (with-output-as-presentation
                               (mystream (second slots-in-a) (presentation-type-of (second slots-in-a)))
                               (with-output-as-presentation
                                (mystream second-from-a (presentation-type-of second-from-a))
                                (with-output-as-presentation
                                 (mystream cpl-a (presentation-type-of cpl-a))
                                 (format mystream "~%Instance: ~40t~a" instance-a)
                                 (format mystream "~%Class: ~40t~a" class-of-a)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (first slots-in-a))
                                 (format mystream "~%Is from: ~40t~a" first-from-a)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (second slots-in-a))
                                 (format mystream "~%Is from: ~40t~a" second-from-a)
                                 (format mystream "~%Class-precedence-list: ~40t~a~%" cpl-a)))))))))

    (with-drawing-options (mystream :ink +green+)
                          (with-output-as-presentation 
                           (mystream instance-b (presentation-type-of instance-b))
                           (with-output-as-presentation
                            (mystream class-of-b (presentation-type-of class-of-b))
                            (with-output-as-presentation
                             (mystream (first slots-in-b) (presentation-type-of (first slots-in-b)))
                             (with-output-as-presentation
                              (mystream first-from-b (presentation-type-of first-from-b))
                              (with-output-as-presentation
                               (mystream (second slots-in-b) (presentation-type-of (second slots-in-b)))
                               (with-output-as-presentation
                                (mystream second-from-b (presentation-type-of second-from-b))
                                (with-output-as-presentation
                                 (mystream cpl-b (presentation-type-of cpl-b))
                                 (format mystream "~%Instance: ~40t~a" instance-b)
                                 (format mystream "~%Class: ~40t~a" class-of-b)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (first slots-in-b))
                                 (format mystream "~%Is from: ~40t~a" first-from-b)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (second slots-in-b))
                                 (format mystream "~%Is from: ~40t~a" second-from-b)
                                 (format mystream "~%Class-precedence-list: ~40t~a~%" cpl-b)))))))))

        (with-drawing-options (mystream :ink +orange+)
                          (with-output-as-presentation 
                           (mystream instance-c (presentation-type-of instance-c))
                           (with-output-as-presentation
                            (mystream class-of-c (presentation-type-of class-of-c))
                            (with-output-as-presentation
                             (mystream (first slots-in-c) (presentation-type-of (first slots-in-c)))
                             (with-output-as-presentation
                              (mystream first-from-c (presentation-type-of first-from-c))
                              (with-output-as-presentation
                               (mystream (second slots-in-c) (presentation-type-of (second slots-in-c)))
                               (with-output-as-presentation
                                (mystream second-from-c (presentation-type-of second-from-c))
                                (with-output-as-presentation
                                 (mystream cpl-c (presentation-type-of cpl-c))
                                 (format mystream "~%Instance: ~40t~a" instance-c)
                                 (format mystream "~%Class: ~40t~a" class-of-c)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (first slots-in-c))
                                 (format mystream "~%Is from: ~40t~a" first-from-c)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (second slots-in-c))
                                 (format mystream "~%Is from: ~40t~a" second-from-c)
                                 (format mystream "~%Class-precedence-list: ~40t~a~%" cpl-c)))))))))
    
    (with-drawing-options (mystream :ink +violet+)
                          (with-output-as-presentation 
                           (mystream instance-d (presentation-type-of instance-d))
                           (with-output-as-presentation
                            (mystream class-of-d (presentation-type-of class-of-d))
                            (with-output-as-presentation
                             (mystream (first slots-in-d) (presentation-type-of (first slots-in-d)))
                             (with-output-as-presentation
                              (mystream first-from-d (presentation-type-of first-from-d))
                              (with-output-as-presentation
                               (mystream (second slots-in-d) (presentation-type-of (second slots-in-d)))
                               (with-output-as-presentation
                                (mystream second-from-d (presentation-type-of second-from-d))
                                (with-output-as-presentation
                                 (mystream cpl-d (presentation-type-of cpl-d))
                                 (format mystream "~%Instance: ~40t~a" instance-d)
                                 (format mystream "~%Class: ~40t~a" class-of-d)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (first slots-in-d))
                                 (format mystream "~%Is from: ~40t~a" first-from-d)
                                 (format mystream "~%Slot Allocation for Slot ~40t~a" (second slots-in-d))
                                 (format mystream "~%Is from: ~40t~a" second-from-d)
                                 (format mystream "~%Class-precedence-list: ~40t~a~%" cpl-d))))))))))
  (values)))

(my-info)
