(defconstant *X* 1)
(defconstant *O* -1)
(defconstant *empty* 0)

(define-presentation-type board-position ())

(define-presentation-type empty-board-position ()
  :inherit-from 'board-position)

(define-presentation-type x-board-position ()
  :inherit-from 'board-position)

(define-presentation-type o-board-position ()
  :inherit-from 'board-position)

(clim:define-application-frame tic-tac-toe
  ()
  ((board :initform (make-array '(3 3) :initial-element *empty*))
    (board-diagnosis :initform t)
    (whose-move :initform *X*)
    (user-plays :initform *X*))
  (:panes
    (board-display :application
      :display-function 'display-board
      :incremental-redisplay t
      :scroll-bars nil)
    (status :application
      :display-function 'display-status
      :incremental-redisplay t
      :scroll-bars nil))
  (:layouts
    (main
      (vertically ()
	(9/10 board-display)
	(1/10 status)))))

(defmethod initialize-instance :after ((frame tic-tac-toe) &rest args)
  (declare (ignore args))
  (go-to-stopped-state frame))

(define-tic-tac-toe-command (com-tic-tac-toe-exit :menu "Exit") ()
  (clim:frame-exit *application-frame*))

(define-tic-tac-toe-command (com-reset-tic-tac-toe :menu "Reset") ()
  (reset-board *application-frame*)
  (go-to-stopped-state *application-frame*))

(defmethod reset-board ((frame tic-tac-toe))
  (window-clear (get-frame-pane frame 'board-display))
  (with-slots (board board-diagnosis) frame
    (setf board-diagnosis t)
    (dotimes (ix 3)
      (dotimes (iy 3)
	(setf (aref board ix iy) *empty*)))))

;;; the display method for the board pane

(defmethod display-board ((frame tic-tac-toe) stream
			   &key &allow-other-keys)
  (with-slots (board) frame
    (multiple-value-bind (width height) (clim:window-inside-size stream)
      (multiple-value-bind (size x-offset y-offset)
	(if (< width height)
	  (values (floor width 3) 0 (floor (- height width) 2))
	  (values (floor height 3) (floor (- width height) 2) 0))
	(flet ((draw-element (elt position stream)
		 (cond ((= elt *X*)
			 (with-output-as-presentation
			   (stream position 'x-board-position
			     :single-box t)
			   (draw-line* stream 0.1 0.1 0.9 0.9 :line-thickness 2)
			   (draw-line* stream 0.1 0.9 0.9 0.1 :line-thickness 2)))
		   ((= elt *O*)
		     (with-output-as-presentation
		       (stream position 'o-board-position
			 :single-box t)
		       (draw-circle* stream 0.5 0.5 0.45
			 :filled nil :line-thickness 2)))
		   (t
		     (with-output-as-presentation
		       (stream position 'empty-board-position)
		       (draw-rectangle* stream 0.1 0.1 0.9 0.9 :ink +white+))))))
	  (with-translation (stream x-offset y-offset)
	    (with-scaling (stream size size)
	      (dotimes (iy 3)
		(with-translation (stream 0 iy)
		  (dotimes (ix 3)
		    (let ((elt (aref board ix iy))
			   (position (+ (* 3 iy) ix)))
		      (updating-output (stream :unique-id position
					 :cache-value elt)
			(with-translation (stream ix 0)
			  (draw-element elt position stream)))))))
	      (draw-line* stream 0.1 1.0 2.9 1.0)
	      (draw-line* stream 0.1 2.0 2.9 2.0)
	      (draw-line* stream 1.0 0.1 1.0 2.9)
	      (draw-line* stream 2.0 0.1 2.0 2.9))))))))

;;; the display method for the status pane

(defmethod display-status ((frame tic-tac-toe) stream
			    &key &allow-other-keys)
  (with-slots (board-diagnosis) frame
    (updating-output (stream :cache-value board-diagnosis)
      (let ((string (cond ((null board-diagnosis) "Cat’s game")
		      ((eql board-diagnosis *X*) "Win for X")
		      ((eql board-diagnosis *O*) "Win for O"))))
	(when string
	  (write-string string stream))))))

;;; make a move in the game

(define-tic-tac-toe-command com-user-move
  ((pos 'empty-board-position :gesture :select))
  (with-slots (board whose-move board-diagnosis) *application-frame*
    (multiple-value-bind (iy ix) (floor pos 3)
      (make-move *application-frame* ix iy))
    (if (eql board-diagnosis t)
      (multiple-value-bind (ix iy) (find-next-ttt-move board whose-move)
	(make-move *application-frame* ix iy)
	(unless (eql board-diagnosis t)
	  (go-to-stopped-state *application-frame*)))
      (go-to-stopped-state *application-frame*))))

(defmethod make-move ((frame tic-tac-toe) ix iy)
  (with-slots (board whose-move board-diagnosis) frame
    (if (= (aref board ix iy) *empty*)
      (progn (setf (aref board ix iy) whose-move)
	(setf whose-move (- whose-move))
	(setf board-diagnosis (diagnose-board board)))
      (error "Not an empty position"))))

;;; edit a position by cycling through possibilities

(define-tic-tac-toe-command com-edit-position
  ((pos 'board-position :gesture :select))
  (with-slots (board board-diagnosis) *application-frame*
    (multiple-value-bind (iy ix) (floor pos 3)
      (setf (aref board ix iy)
	(let ((old (aref board ix iy)))
	  (cond ((= old *empty*) *X*)
	    ((= old *X*) *O*)
	    (t *empty*)))))
    (setf board-diagnosis (diagnose-board board))))

(define-tic-tac-toe-command (com-play-user-first :menu "Play (user X)") ()
  (with-slots (user-plays) *application-frame*
    (setf user-plays *X*)
    (start-play *application-frame*)))

(define-tic-tac-toe-command (com-play-program-first :menu "Play (program X)") ()
  (with-slots (user-plays) *application-frame*
    (setf user-plays *O*)
    (start-play *application-frame*)))

(defmethod start-play ((frame tic-tac-toe))
  (with-slots (board whose-move user-plays board-diagnosis) *application-frame*
    (unless (eql board-diagnosis t)
      (reset-board *application-frame*))
    (go-to-playing-state *application-frame*)
    (setf whose-move (determine-whose-move board))
    (when (/= user-plays whose-move)
      (multiple-value-bind (ix iy) (find-next-ttt-move board whose-move)
	(make-move *application-frame* ix iy)))))

;;; enable and disable appropriate commands

(defmethod go-to-playing-state ((frame tic-tac-toe))
  (setf (command-enabled 'com-play-user-first frame) nil)
  (setf (command-enabled 'com-play-program-first frame) nil)
  (setf (command-enabled 'com-edit-position frame) nil)
  (setf (command-enabled 'com-user-move frame) t))

(defmethod go-to-stopped-state ((frame tic-tac-toe))
  (setf (command-enabled 'com-play-user-first frame) t)
  (setf (command-enabled 'com-play-program-first frame) t)
  (setf (command-enabled 'com-edit-position frame) t)
  (setf (command-enabled 'com-user-move frame) nil))

;;; these are the game-playing functions
;;; they have no user-interface component
;;; picks the next move for the program

(defun find-next-ttt-move (board whose-move)
  (let ((me whose-move) (you (- whose-move)))
    (let (target (points nil))
      (labels ((pushnu (x y)
		 (unless (find-if #'(lambda (xy)
				      (and (= (first xy) x)
					(= (second xy) y)))
			   points)
		   (push (list x y) points)))
		(point-= (p1 p2)
		  (and (= (first p1) (first p2)) (= (second p1) (second p2))))
		(pick-a-choice-if-ive-got-one ()
		  (when points
		    (let ((ran (random (length points))))
		      (return-from find-next-ttt-move
			(values-list (nth ran points))))))
		(almost-complete-row (elt0 x0 y0 elt1 x1 y1 elt2 x2 y2)
		  (cond ((and (= elt0 *empty*) (= elt1 elt2 target))
			  (pushnu x0 y0))
		    ((and (= elt1 *empty*) (= elt0 elt2 target))
		      (pushnu x1 y1))
		    ((and (= elt2 *empty*) (= elt0 elt1 target))
		      (pushnu x2 y2))))
		(row-with-one-only (elt0 x0 y0 elt1 x1 y1 elt2 x2 y2)
		  (cond ((and (= elt0 elt1 *empty*) (= elt2 target))
			  (pushnu x0 y0)
			  (pushnu x1 y1))
		    ((and (= elt1 elt2 *empty*) (= elt0 target))
		      (pushnu x1 y1)
		      (pushnu x2 y2))
		    ((and (= elt0 elt2 *empty*) (= elt1 target))
		      (pushnu x0 y0)
		      (pushnu x2 y2))))
		(pair-of-rows-to-fork (elt0 x0 y0
					elt1a x1a y1a
					elt2a x2a y2a
					elt1b x1b y1b
					elt2b x2b y2b)
		  (declare (ignore x1a y1a x2a y2a x1b y1b x2b y2b))
		  (when (and (= elt0 *empty*)
			  (or (and (= elt1a *empty*) (= elt2a target))
			    (and (= elt2a *empty*) (= elt1a target)))
			  (or (and (= elt1b *empty*) (= elt2b target))
			    (and (= elt2b *empty*) (= elt1b target))))
		    (pushnu x0 y0))))
	;; look for immediate win
	(setq target me)
	(map-over-ttt-board-rows board #'almost-complete-row)
	(pick-a-choice-if-ive-got-one)
	;; look for immediate loss unless I block
	(setq target you)
	(map-over-ttt-board-rows board #'almost-complete-row)
	(pick-a-choice-if-ive-got-one)
	;; look for my fork
	(setq target me)
	(map-over-pairs-of-ttt-board-rows board #'pair-of-rows-to-fork)
	(pick-a-choice-if-ive-got-one)
	;; look for opponent’s fork
	(setq target you)
	(map-over-pairs-of-ttt-board-rows board #'pair-of-rows-to-fork)
	(when points
	  (if (= (length points) 1)
	    ;; block the fork
	    (pick-a-choice-if-ive-got-one)
	    ;; two fork points - have to force
	    (let ((fork-points points))
	      (setq points nil)
	      (map-over-ttt-board-rows board #'row-with-one-only)
	      (setq points (set-difference points fork-points :test #'point-=))
	      (pick-a-choice-if-ive-got-one))))))
    ;; an opening move in a corner requires a reply in the center
    ;; else prefer a corner
    (when (= (aref board 1 1) *empty*)
      (return-from find-next-ttt-move (values 1 1)))
    (loop for ix from 0 to 2 by 2 do
      (loop for iy from 0 to 2 by 2 do
	(when (= (aref board ix iy) *empty*)
	  (return-from find-next-ttt-move (values ix iy)))))))

;;; returns *X* or *O* for a winning board
;;; T for a board still playable, nil for a cats game

(defun diagnose-board (board)
  (flet ((diagnose-row (elt0 x0 y0 elt1 x1 y1 elt2 x2 y2)
	   x0 y0 x1 y1 x2 y2
	   ;; returns *X* or *O* for a winning row
	   ;; T for a row still playable, nil for a blocked row
	   (cond ((= elt0 elt1 elt2)
		   (if (= elt0 *empty*) t elt0))
	     ((= elt0 elt1)
	       (if (or (= elt0 *empty*) (= elt2 *empty*)) t nil))
	     ((= elt0 elt2)
	       (if (or (= elt0 *empty*) (= elt1 *empty*)) t nil))
	     ((= elt1 elt2)
	       (if (or (= elt0 *empty*) (= elt1 *empty*)) t nil))
	     (t nil))))
    (let ((x-win nil) (o-win nil) (cats-game t))
      (map-over-ttt-board-rows board
	#'(lambda (elt0 x0 y0 elt1 x1 y1 elt2 x2 y2)
	    (let ((q (diagnose-row elt0 x0 y0 elt1 x1 y1 elt2 x2 y2)))
	      (cond ((eql q *X*) (setq x-win t))
		((eql q *O*) (setq o-win t))
		((eql q t) (setq cats-game nil))))))
      (cond (x-win *X*)
	(o-win *O*)
	(cats-game nil)
	(t t)))))

;;; used when starting the program in the middle of a game

(defun determine-whose-move (board)
  (let ((xs 0) (os 0))
    (dotimes (x 3)
      (dotimes (y 3)
	(let ((elt (aref board x y)))
	  (cond ((= elt *X*) (incf xs))
	    ((= elt *O*) (incf os))))))
    (if (> xs os) *O* *X*)))

(defun map-over-ttt-board-rows (board function)
  (macrolet ((dorow (x0 y0 dx dy)
	       (let* ((x1 (+ x0 dx)) (y1 (+ y0 dy))
		       (x2 (+ x1 dx)) (y2 (+ y1 dy)))
		 (setq x2 (mod x2 3) y2 (mod y2 3))
		 `(funcall function
		    (aref board ,x0 ,y0) ,x0 ,y0
		    (aref board ,x1 ,y1) ,x1 ,y1
		    (aref board ,x2 ,y2) ,x2 ,y2))))
    (dorow 0 0 0 1)
    (dorow 1 0 0 1)
    (dorow 2 0 0 1)
    (dorow 0 0 1 0)
    (dorow 0 1 1 0)
    (dorow 0 2 1 0)
    (dorow 0 0 1 1)
    (dorow 0 2 1 -1)))

(defun map-over-pairs-of-ttt-board-rows (board function)
  (macrolet ((dorows (x0 y0 dxa dya dxb dyb)
	       (let* ((x1a (+ x0 dxa)) (y1a (+ y0 dya))
		       (x2a (+ x1a dxa)) (y2a (+ y1a dya))
		       (x1b (+ x0 dxb)) (y1b (+ y0 dyb))
		       (x2b (+ x1b dxb)) (y2b (+ y1b dyb)))
		 (setq x2a (mod x2a 3) y2a (mod y2a 3)
		   x2b (mod x2b 3) y2b (mod y2b 3))
		 `(funcall function
		    (aref board ,x0 ,y0) ,x0 ,y0
		    (aref board ,x1a ,y1a) ,x1a ,y1a
		    (aref board ,x2a ,y2a) ,x2a ,y2a
		    (aref board ,x1b ,y1b) ,x1b ,y1b
		    (aref board ,x2b ,y2b) ,x2b ,y2b))))
    (dorows 0 0 0 1 1 0)
    (dorows 0 0 0 1 1 1)
    (dorows 0 0 1 0 1 1)
    (dorows 0 1 1 0 0 1)
    (dorows 0 2 0 -1 1 0)
    (dorows 0 2 0 -1 1 -1)
    (dorows 0 2 1 0 1 -1)
    (dorows 1 0 0 1 1 0)
    (dorows 1 1 0 1 1 0)
    (dorows 1 1 0 1 1 1)
    (dorows 1 1 0 1 1 -1)
    (dorows 1 1 1 0 1 1)
    (dorows 1 1 1 0 1 -1)
    (dorows 1 1 1 1 1 -1)
    (dorows 1 2 0 -1 1 0)
    (dorows 2 0 0 1 -1 0)
    (dorows 2 0 0 1 -1 1)
    (dorows 2 0 -1 0 -1 1)
    (dorows 2 1 -1 0 0 1)
    (dorows 2 2 0 -1 -1 0)
    (dorows 2 2 0 -1 -1 -1)
    (dorows 2 2 -1 0 -1 -1)))

#||
()
(setq ttt (clim:make-application-frame 'tic-tac-toe
	    :left 400 :right 800 :top 150 :bottom 400))
(clim:run-frame-top-level ttt)
||#
