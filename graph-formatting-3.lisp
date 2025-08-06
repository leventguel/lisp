(define-application-frame graph-it ()
  ((root-node :initform (find-class 'clim:design) :initarg :root-node :accessor
     root-node)
   (app-stream :initform () :accessor app-stream)
   (dir :initarg :dir :accessor dir)
   (ori :initarg :ori :accessor ori))
  (:panes
    (display :application :display-function 'draw-display :scroll-bars :both 
      :background +black+ :foreground +white+))
  (:layouts
    (:defaults
      (horizontally ()
	display))))

(define-presentation-type node ())

(defun make-random-color ()
  (make-rgb-color (/ (+ 30 (random (- 255 30))) 255)
    (/ (+ 30 (random (- 255 30))) 255)
    (/ (+ 30 (random (- 255 30))) 255)))

(defun draw-node (object stream)
  (with-drawing-options (stream :ink (make-random-color))
    (with-output-as-presentation (stream object 'node)
      (surrounding-output-with-border (stream :shape :oval)
	(format stream "~a~&~a~&~a~&" 
		(class-name object) 
		(sb-mop:class-direct-slots object)
		(sb-mop:class-direct-default-initargs object))))))

(defun graph-it (&key (root-node (find-class 'basic-sheet)) dir ori)
  (if (atom root-node)
    (setf root-node (list root-node)))
  (let ((graph-it
	  (make-application-frame 'graph-it :frame-manager (find-frame-manager :port (find-port)) :width 1000 :height
	    800 :root-node root-node :dir dir :ori ori)))
    (run-frame-top-level graph-it)))

(defmethod draw-display ((frame graph-it) stream)
  (format-graph-from-roots (root-node frame) #'draw-node 
			   (if (eql (dir frame) :out)
			       #'sb-mop:class-direct-superclasses
			     #'sb-mop:class-direct-subclasses)
    :stream stream :arc-drawer
    (lambda (stream from-object to-object x1 y1 x2 y2 &rest drawing-options)
      (declare (dynamic-extent drawing-options))
      (declare (ignore from-object to-object))
      (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options)) 
    :orientation (if (eql (ori frame) :v)
		     :vertical
		   :horizontal))
;;    :merge-duplicates t)
  (setf (app-stream frame) stream))

(define-graph-it-command (com-exit :name "quit" :keystroke (#\q :control)) 
  () (frame-exit *application-frame*))

;; this one you can use like (run-graph-it 'clim:dialog) or
;; like (run-graph-it 'drei:view)...etc.
(defun run-graph-it (&key (root-node 'basic-sheet) (dir :in) (ori :h))
  (graph-it :root-node (find-class root-node) :dir dir :ori ori))
