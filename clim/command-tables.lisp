(in-package :clim-user)

;;; Basic node representation

(defclass node ()
  ((id :accessor id :initarg :id :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (children :accessor children :initarg :children :initform nil)))

(define-presentation-method present (node (type node) stream (view textual-view) &key)
  (format stream "~%    ~A" (id node)))

(defun rootp (node) (not (parent node)))

(defmethod add-child ((parent node) (child node))
  (pushnew child (children parent))
  (setf (parent child) parent))

(defun multiply (node &key (multiple 3))
  (if (children node)
      (dolist (n (children node)) (multiply n))
      (dotimes (x multiple) 
	(add-child node (make-instance 'node
			  :id (format nil "~A.~A" (id node) (- multiple x)))))))

;;; The application frame and the pane display function

(define-application-frame tree () ()
  (:panes (pane-tree 
	   :application :scroll-bars nil
	   :display-function #'display-tree
	   :initial-cursor-visibility nil
	   :display-after-commands nil))
  (:layouts (:default pane-tree)))

(defmethod display-tree ((frame tree) stream) 
  (let ((node (make-instance 'node :id "N")))
    (dotimes (x 5) (multiply node))
    (present node 'node :stream stream)))

(define-tree-command com-clear () 
  (window-clear (get-frame-pane *application-frame* 'pane-tree)))

;;; Two comands for moving up and down the tree

(define-tree-command com-tree-down ((node 'node))
  (when (children node) 
    (com-clear)
    (let ((stream (get-frame-pane *application-frame* 'pane-tree)))
      (dolist (child (children node)) (present child 'node :stream stream)))))

(define-tree-command com-tree-up ((node 'node))
  (let ((stream (get-frame-pane *application-frame* 'pane-tree)))
    (cond ((rootp node))
	  ((rootp (parent node)) (com-clear) (present (parent node) 'node :stream stream))
	  (t (com-clear) (com-tree-down (parent (parent node)))))))

(define-presentation-to-command-translator move-down-tree
    (node com-tree-down tree
	  :gesture :select		;command activated with left-click on a node
	  :menu t)			;includes this command in right-click menu
  (object) (list object))

(define-presentation-to-command-translator move-up-tree
    (node com-tree-up tree
	  :gesture :describe		;command activated with a middle-click on a node
	  :menu t)			;includes this command in a right-click menu
  (object) (list object))

(run-frame-top-level (make-application-frame 'tree :width 200))
