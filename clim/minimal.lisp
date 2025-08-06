
(define-application-frame word		;name
    ()					;superclasses
  ()					;slots
  ;; options
  (:panes				;panes option
   (title				;pane name 
    :application)			;pane type
   (document				;pane name
    :application))			;pane type
  (:layouts				;layouts option
   (default				;layout name
       (vertically ()			;layout macros
	 (1/8 title)
	 (7/8 document)))))

(run-frame-top-level (make-application-frame 'word :height 300 :width 300))

