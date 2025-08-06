(defclass standard-point ()
		((x :initform 0 :initarg :x)
			(y :initform 0 :initarg :y)))


(defgeneric distance-origin (point))

(defmethod distance-origin ((point1 standard-point))
		(when (and (slot-exists-p point1 'x) (slot-exists-p point1 'y))
				(sqrt (+ (expt (sb-mop:standard-instance-access point1 0) 2)
													(expt (sb-mop:standard-instance-access point1 1) 2)))))

;;; (distance-origin p1) is the same as (distance-main p1 nil)
;;; or (distance-main nil p1) or (distance-main 'anything p1) etc..
;;; there's no real way as of now of using &optional for specialization in common-lisp
;;; such as would be desired for a case of (distance-main p1) and (distance-main p1 p2),
;;; i.e. the first or second argument to be optional but as already said
;;; that is not possible with &optional arguments to defmethod.
;;; Optional arguments in methods are allowed but there's no specialization
;;; possible for them.

(defgeneric distance-main (point1 point2))

(defmethod distance-main ((point1 null) (point2 null)))

(defmethod distance-main ((point1 standard-point) point2)
		(when (and (slot-exists-p point1 'x) (slot-exists-p point1 'y))
				(sqrt (+ (expt (sb-mop:standard-instance-access point1 0) 2)
													(expt (sb-mop:standard-instance-access point1 1) 2)))))

(defmethod distance-main (point1 (point2 standard-point))
		(when (and (slot-exists-p point2 'x) (slot-exists-p point2 'y))
				(sqrt (+ (expt (sb-mop:standard-instance-access point2 0) 2)
													(expt (sb-mop:standard-instance-access point2 1) 2)))))

(defmethod distance-main ((point1 null) (point2 standard-point))
		(when (and (slot-exists-p point2 'x) (slot-exists-p point2 'y))
				(sqrt (+ (expt (sb-mop:standard-instance-access point2 0) 2)
													(expt (sb-mop:standard-instance-access point2 1) 2)))))

(defmethod distance-main ((point1 standard-point) (point2 null))
		(when (and (slot-exists-p point1 'x) (slot-exists-p point1 'y))
				(sqrt (+ (expt (sb-mop:standard-instance-access point1 0) 2)
													(expt (sb-mop:standard-instance-access point1 1) 2)))))

(defmethod distance-main ((point1 standard-point) (point2 standard-point))
		(when (and (slot-exists-p point1 'x) (slot-exists-p point1 'y) 
													(slot-exists-p point2 'x) (slot-exists-p point2 'y))
				(sqrt (+ (expt (- (sb-mop:standard-instance-access point2 0)
																						(sb-mop:standard-instance-access point1 0)) 2)
													(expt (- (sb-mop:standard-instance-access point2 1)
																						(sb-mop:standard-instance-access point1 1)) 2)))))

(defparameter p1 (make-instance 'standard-point :x -3 :y 5))
(defparameter p2 (make-instance 'standard-point :x 7 :y -1))

(defun test () 
  (print (distance-main p2 p1)))

(test)

;;; the only available trick to circumenvent the above problem with specialization is to use a
;;; normal funcion to pass the optional argument to the method.

(defun distance (point1 &optional point2) 
  (print (distance-main point1 point2)))

(distance p1)
(distance p2)
(distance p1 p2)
