(defpackage #:clock
  (:use #:cl)
  (:export #:clock))

(in-package #:clock)

(defun romanize (arg)
  (if (zerop arg)
      "O"
      (format nil "~@R" arg)))

;(defun clock-string ()
;  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
;    (format nil "~a ~a ~a" (romanize h) (romanize m) (romanize s))))

(defun clock-string ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~a ~a ~a" h m s)))

(defvar done nil)
(defvar *modstate* nil)
(declaim (list *modstate*))
(defun make-shift-foo ()
  (let ((rv 0))
    (if (member :shift *modstate*)
        (setf rv 1))
    (if (member :character-set-switch *modstate*)
        (setf rv (+ rv 2)))
    rv))


(defun clock ()
  (let ((display (xlib:open-default-display))
        (abort))
    (unwind-protect
         (progn
           (let*
               ((screen (xlib:display-default-screen display))
             
                (colormap (xlib:screen-default-colormap screen))
             
                (font (xlib:open-font display "fixed"))
             
                (window (multiple-value-bind (width ascent)
                            (xlib:text-extents font "XVIIII XXXVIIII XXXVIIII")
                          (xlib:create-window
                           :parent (xlib:screen-root screen)
                           :x 512
                           :y 512
                           :width (+ 20 width) 
                           :height (+ 20 ascent)
                           :background 
                           (xlib:alloc-color 
                            colormap
                            (xlib:lookup-color colormap "midnightblue"))
                           :event-mask 
                           (xlib:make-event-mask :key-press :enter-window :exposure))))
             
                (gcontext (xlib:create-gcontext
                           :drawable window
                           :fill-style :solid
                           :background 
                           (xlib:screen-white-pixel screen)
                           :foreground 
                           (xlib:alloc-color 
                            colormap
                            (xlib:lookup-color
                             colormap
                             "yellow"))
                           :font font))
             
                (background (xlib:create-gcontext
                             :drawable window
                             :fill-style :solid
                             :background (xlib:screen-white-pixel screen)
                             :foreground (xlib:alloc-color colormap
                                                           (xlib:lookup-color colormap
                                                                              "midnightblue"))
                             :font font))
             
                (strng (lambda () (clock-string)))
             
                (string-width (xlib:text-width gcontext (funcall strng))))
          
             (flet ((draw-stuff ()
                      (xlib:draw-rectangle 
                       window background
                       0 0
                       (xlib:drawable-width window)
                       (xlib:drawable-height window)
                       :fill-p)
                    
                      (xlib:draw-glyphs 
                       window gcontext
                       (- (truncate
                           (- (xlib:drawable-width window) string-width)
                           2)
                          10)
                       (- (xlib:drawable-height window) 10)
                       (funcall strng))
                      (xlib:display-force-output display)))
            
               (xlib:map-window window)
            
               (loop with done-p do
                        (xlib:event-case 
                         (display :timeout 1)
                         (exposure ()
                                   ;; initial window already appears with stuff drawn!
                                   ;; if we remove or comment-out (draw-stuff) here
                                   ;; our window only appears with the blue rectangle
                                   ;; and no glyphs, since glph drawing would be the second step
                                   (draw-stuff)
                                   nil)
                    
                         (enter-notify () nil)
                    
                         (key-press (code)
                                    (case 
                                        (xlib:keysym->character
                                         display
                                         (xlib:keycode->keysym display code (make-shift-foo)))
                                      (#\q (return-from clock t) nil)
                                      (#\t (setq done-p t) nil))
                                    nil))
                  
                        (draw-stuff)
                        (when done-p (loop-finish))))))
      (when display
        (xlib:close-display display :abort abort)))))
