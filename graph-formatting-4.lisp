(in-package :clim-user)

(define-application-frame graph-it ()
  ((root-node :initform (find-class 'clim:design) :initarg :root-node :accessor
     root-node)
   (app-stream :initform () :accessor app-stream)
   (dir :initarg :dir :accessor dir)
   (ori :initarg :ori :accessor ori))
  (:panes
   (display :application :display-function 'draw-display :scroll-bars :both
	    :end-of-line-action :wrap :end-of-page-action :wrap*
	    :background +black+ :foreground +white+
	    :text-margins '(:left (:absolute 30)
				  :right (:relative 30)
				  :top (:relative 30)
				  :bottom (:absolute 370))
	    ))
  (:layouts
    (:defaults
      (horizontally ()
	display))))

(define-presentation-type node ())

(defun make-random-color ()
  (make-rgb-color (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)))

(defparameter my-color (make-random-color))

(defun draw-node (object stream)
  (with-drawing-options (stream :ink my-color)
    (with-output-as-presentation (stream object 'node)
      (surrounding-output-with-border (stream :shape :rectangle)
	(format stream "~a~&~a~&~a~&" 
		(class-name object) 
		(sb-mop:class-direct-slots object)
		(sb-mop:class-direct-default-initargs object))))))

(defun graph-it (&key (root-node (find-class 'basic-sheet)) dir ori)
  (if (atom root-node)
    (setf root-node (list root-node)))
  (let ((fm (find-frame-manager :port (find-port)))
	(graph-it
	  (make-application-frame 'graph-it :pretty-name "class-nodes" :frame-manager (find-frame-manager :port (find-port)) :width 1000 :height
				  800 :root-node root-node :dir dir :ori ori)))

    (labels ((run () 
             (let ((*package* (find-package :clim-user)))
               (unwind-protect
		   (clim-debugger:with-debugger () (run-frame-top-level graph-it)))
                 (disown-frame fm graph-it))))
	  (values (clim-sys:make-process #'run :name "graph-it")
                  graph-it))))

(defmethod draw-display ((frame graph-it) stream)
  (format-graph-from-roots (root-node frame) #'draw-node 
			   (cond 
			    ((eql (dir frame) :sup)
			       #'sb-mop:class-direct-superclasses)
			    ((eql (dir frame) :sub)
			     #'sb-mop:class-direct-subclasses)
			    (t
			      #'sb-mop:class-direct-subclasses))
    :stream stream :arc-drawer
    (lambda (stream from-object to-object x1 y1 x2 y2 &rest drawing-options)
      (declare (dynamic-extent drawing-options))
      (declare (ignore from-object to-object))
      (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options))
    :orientation 
    (cond 
     ((eql (ori frame) :v) :vertical)
     ((eql (ori frame) :h) :horizontal)
     (t :horizontal)) 
    :graph-type :tree
     :merge-duplicates nil 
     :center-nodes nil
     :generation-separation '(3 :character)
     :within-generation-separation '(1 :character) :move-cursor nil)
  (setf (app-stream frame) stream))

(define-graph-it-command (com-home :name "forward" :keystroke :home) 
  () (drei-commands::beginning-of-buffer (point)))

(define-graph-it-command (com-end :name "forward" :keystroke :end) 
  () (drei-commands::end-of-buffer (point)))

(define-graph-it-command (com-exit :name "quit" :keystroke (#\q :control)) 
  () (frame-exit *application-frame*))

;; this one you can use like (run-graph-it 'clim:dialog) or
;; like (run-graph-it 'drei:view)...etc.
(defun run-graph-it (&key (root-node 'basic-sheet) (dir :sub) (ori :h))
  (graph-it :root-node (find-class root-node) :dir dir :ori ori))
