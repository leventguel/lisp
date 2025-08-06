(in-package #:clim-demo)

(define-application-frame tracking-pointer-test ()
  ((mwp :initform nil :accessor mwp)
   (tp :initform nil :accessor tp)
   (ts :initform 'app2 :accessor ts))
  (:geometry :width 800 :height 600)
  (:panes (pane :basic-pane)
          (app1 :application :display-function #'display :scroll-bars nil)
          (app2 :application :display-function #'display)
          (int  :interactor))
  (:pointer-documentation t)
  (:layouts (default (vertically ()
                       (horizontally ()
                         (labelling (:label "app1 (application)") app1)
                         (labelling (:label "app2 (application)") app2))
                       (horizontally ()
                         (labelling (:label "pane (basic pane)") pane)
                         (labelling (:label "int  (interactor)") int))
                       ;; All below gadgets should be replacable with
                       ;; accepting-values.
                       (horizontally ()
                         (vertically ()
                           (labelling (:label "Tracked sheet")
                             (with-radio-box (:value-changed-callback
                                              (lambda (g v)
                                                (setf (ts *application-frame*)
                                                      (alexandria:switch (v :test #'string=
                                                                            :key #'gadget-label)
                                                        ("app1" 'app1)
                                                        ("app2" 'app2)
                                                        ("pane" 'pane)
                                                        ("int" 'int)))))
                               "app1" "app2" "int" "pane"))
                           (labelling (:label "Presentations")
                             (with-radio-box (:type :some-of)
                               "Any" "Blank" "Integer" "String" "Foo")))
                         (labelling (:label "Handlers")
                           (with-radio-box (:type :some-of)
                             "pointer-motion"
                             "pointer-button-press"
                             "pointer-button-release"
                             "presentation"
                             "presentation-button-press"
                             "presentation-button-release"
                             "keyboard"
			     "otherwise"))
                         (labelling (:label "Options")
                           (vertically ()
                             (with-radio-box (:orientation :horizontal)
                               "Highlight" "Not highlight" "Unspecified")
                             (make-pane :toggle-button :label "Multiple Window"
                                        :value-changed-callback
                                        (lambda (gadget value)
                                          (setf (mwp *application-frame*) value)))
                             (make-pane :toggle-button :label "Transformp"
                                        :value-changed-callback
                                        (lambda (gadget value)
                                          (setf (tp *application-frame*) value)))
                             (make-pane :toggle-button :label "Remove last")
                             (make-pane 'push-button :label "Track pointer"
                                        :activate-callback
                                        (lambda (gadget)
                                          (execute-frame-command *application-frame*
                                                                 '(track-pointer)))))))))))

(define-presentation-type foo ())

(defmethod display ((frame tracking-pointer-test) pane)
  (when (eql (pane-name pane) 'app2)
    (setf (medium-transformation (sheet-medium pane))
          (make-scaling-transformation* 2 2)))
  (with-output-as-presentation (pane 42 'integer)
    (format pane "Presentation integer 42."))
  (fresh-line pane)
  (with-output-as-presentation (pane "42" 'string)
    (format pane "Presentation string \"42\"."))
  (fresh-line pane)
  (with-output-as-presentation (pane "42" 'foo)
    (format pane "Presentation FOO \"42\".")))

(defun draw-cursor (window x y id)
  (if (output-recording-stream-p window)
      (with-output-recording-options (window :record nil)
        (draw-point* window x y :line-thickness 32)
        (draw-point* window x y :line-thickness 24 :ink +red+))
      (progn
        (draw-point* window x y :line-thickness 32)
        (draw-point* window x y :line-thickness 24 :ink +red+)))
  
  (window-clear *pointer-documentation-output*)
  (format *pointer-documentation-output* "Clause ID: ~s.~%" id))

(define-tracking-pointer-test-command (track-pointer :name t) ()
  (let* ((tracked-sheet (find-pane-named *application-frame* (ts *application-frame*))))
    (tracking-pointer (tracked-sheet :multiple-window (mwp *application-frame*)
                                     :transformp (tp *application-frame*)
                                     :highlight nil
                                     :context-type 'foo)
      (:pointer-motion
       (&key window x y)
       (draw-cursor window x y :pointer-motion))
      (:pointer-button-press
       (&key event x y)
       (draw-cursor (event-sheet event) x y :pointer-button-press))
      (:pointer-button-release
       (&key event x y)
       (draw-cursor (event-sheet event) x y :pointer-button-release)
       (return-from track-pointer))
      (:presentation
       (&key presentation window x y)
       (draw-cursor window x y :presentation))
      (:presentation-button-release
       (&key presentation event x y)
       (return-from track-pointer)))))

(run-frame-top-level (make-application-frame 'tracking-pointer-test))

;;; tracking pointer should signal error on invalid clause
;;; context-type fails to limit presentation integer vs string 
;;; character gestures in keyboard clause should be processed
;;; transformp should use event's sheet medium transformation
;;; transformp transforms coordinate "the wrong way", i.e it doubles scaling
;;; tracking-pointer should not take into account presentation-translators
