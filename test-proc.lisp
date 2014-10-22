(defpackage test
  (:use :common-lisp)
  (:export 
   #:putprop #:defprop #:getprop #:square #:average #:close-enough #:good-enough #:root-search #:half-interval-method
   #:fixed-point #:fact-iter #:factorial #:fib-iter #:fib #:cont-change #:divides #:primep #:expmod #:cube #:inc 
   #:sum #:average-damp #:cube-root #:deriv #:integral #:newton-transform #:newtons-method #:newt-sqrt
   #:fixed-point-of-transform #:fp-trans-sqrt #:new-trans-sqrt #:compose #:list-ref #:list-rec #:same-parity 
   #:for-each #:last-elem #:deep-reverse #:deep-reverse2 #:deep-reverse3 #:deep-reverse4 #:filter-for #:eliminate
   #:accumulate #:accumulator-iter #:accumulator #:enum-interval #:product #:scale-list #:scale-tree #:subsets
   #:powerset #:combinations #:square-tree #:horner-eval #:accumulate-n #:mat*vec #:vec*mat #:transpose #:mat*mat
   #:fold-right #:fold-left #:lottery #:lottery2 #:lottery3 #:lottery4 #:lottery5 #:random-seq #:row-sum #:col-sum #:levels #:node-p #:leaf-p
   #:count-nodes-flat #:count-leaves-flat #:count-nodes #:count-leaves #:count-nodes-deep #:count-leaves-deep 
   #:count-node #:count-leaf #:gen-dec #:gen-inc #:mappend #:push-in #:pull-out #:distribute-a #:distribute-b 
   #:distribute-c #:distribute-d #:dot-product #:part #:pairs #:separate-pairs  #:part3 #:part4 #:dot-product))

(in-package :test)

(setf *read-default-float-format* 'double-float)
(defparameter tolerance 0.00001)


(defun putprop (sym indic val)
  (and (symbolp sym)
       (setf (get sym indic) val)))

(defmacro defprop (sym indic val)
  `(putprop ',sym ',indic ',val))

(defun getprop (sym indic)
  (and (symbolp sym)
       (get sym indic)))

;;(defun ge (x y)
;;  (or
;;    (not (< x y))
;;    (or (> x y) (= x y))))
;;
;;(defun le (x y)
;;  (or
;;    (not (> x y))
;;    (or (< x y) (= x y))))
;;
;;(defun gt (x y)
;;  (or
;;    (not (<= x y))
;;    (and (> x y) (not (= x y)))))
;;
;;(defun lt (x y)
;;  (or
;;    (not (>= x y))
;;    (and (< x y) (not (= x y)))))

(defun square (x) (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun my-abs (x)
  (cond ((< x 0) (- x))
        (t x)))

;;:test (average 1.0 2.0)
;;=> 1.5

(defun close-enough (x y) 
  (< (my-abs (- x y)) 0.001))

(defun good-enough (guess x)
  (< (my-abs (- (square guess) x)) 0.001))

(defun root-search (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough neg-point pos-point)
        midpoint
        (let ((test-value (funcall f midpoint)))
          (cond 
            ((> 0 test-value)
             (root-search f neg-point midpoint))
            ((< 0 test-value)
             (root-search f midpoint pos-point))
            (t midpoint))))))

(defun half-interval-method (f a b) 
  (let ((a-value (funcall f a)) (b-value (funcall f b))) 
    (cond 
      ((and (< 0 a-value) (> 0 b-value)) (root-search f a b)) 
      ((and (< 0 b-value) (> 0 a-value)) (root-search f b a)) 
      (t (error "Values are not of opposite sign") a b))))

;;:test (half-interval-method #'sin 2.0 4.0)
;;=> 3.141590118408203

;;:test (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
;;=> 1.8932914733886719

(defun fixed-point (f first-guess)
  (labels ((close-enough (v1 v2)
              (< (my-abs (- v1 v2)) tolerance)))
    (labels ((try (guess)
                (let ((next (funcall f guess)))
                  (if (close-enough guess next)
                      next
                      (try next)))))
      (try first-guess))))

;;:test (fixed-point #'cos 1.0)
;;=> 0.7390822985224024

;;:test (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;;=> 1.2587315962971173

;; oscillating sqrt (won't give an answer in the repl! instead the computation will go on forever....)
(defun my-sqrt-nosc (x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;; converging sqrt
(defun my-sqrt-c (x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;:test (my-sqrt 2.0)
;;=> 1.4142135623746899

;;:test (sqrt 2.0)
;;=> 1.4142135623730951

;;localisation into one body
(defun my-sqrt-l (x)
  (labels ((good-enough (guess x)
              (< (my-abs (- (square guess) x)) 0.001)))
    (labels ((improve (guess x)
                (average guess (/ x guess))))
      (labels ((sqrt-iter (guess x)
                  (if (good-enough guess x)
                      guess
                      (sqrt-iter (improve guess x) x))))
        (sqrt-iter 1.0 x)))))

;; localisation into one body and lexical scoping

(defun my-sqrt-i (x)
  (labels ((good-enough (guess)
              (< (my-abs (- (square guess) x)) 0.001)))
    (labels ((improve (guess)
                (average guess (/ x guess))))
      (labels ((sqrt-iter (guess)
                  (if (good-enough guess)
                      guess
                      (sqrt-iter (improve guess)))))
        (sqrt-iter 1.0)))))

;; iterative version of factorial
(defun fact-iter (product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(defun factorial (n)
  (fact-iter 1 1 n))


;; localised lexically scoped iterative version of factorial
(defun factorial (n)
  (labels ((iter (product counter)
              (if (> counter n)
                  product
                  (iter (* counter product)
                        (+ counter 1)))))
    (iter 1 1)))

;; iterative version of fib
(defun fib-iter (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(defun fib (n)
  (fib-iter 1 0 n))

;; localiside lexically scoped iterative version of fib

(defun fib (n)
  (labels ((iter (a b count)
              (if (= count 0)
                  b
                  (iter (+ a b) a (- count 1)))))
    (iter 1 0 n)))

;; number of ways to change money into n kinds of coins (here 5)
(defun count-change (amount)
  (labels ((first-denomination (kinds-of-coins)
              (svref #(1 5 10 25 50) (1- kinds-of-coins))))
    (declare (inline first-denomination))
    (labels ((my-cc (amount kinds-of-coins)
                (cond
                  ((= amount 0) 1)
                  ((or (< amount 0) (= kinds-of-coins 0)) 0)
                  (t (+ (my-cc amount (- kinds-of-coins 1))
                        (my-cc (- amount (first-denomination kinds-of-coins))
                               kinds-of-coins))))))
      (declare (inline my-cc))
      (my-cc amount 5))))

;;:test (count-change 100)
;;=> 292
;; 100 designating $1 (US-currency) in terms of the smallest coin kind


;;    (cond
;;      ((= kinds-of-coins 1) 1)
;;      ((= kinds-of-coins 2) 5)
;;      ((= kinds-of-coins 3) 10)
;;      ((= kinds-of-coins 4) 25)
;;      ((= kinds-of-coins 5) 50)))

;; linear recursive version of expt

(defun lr-expt (b n)
  (if (eq n 0)
      1
      (* b (lr-expt b (- n 1)))))

;; linear iterative version of expt
(defun expt-iter (b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(defun li-expt (b n)
  (expt-iter b n 1))

;; logarithmically growing version of expt
(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((evenp n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

(defun my-gcd (a b)
  (if (= b 0)
      a
      (my-gcd b (rem a b))))

(defun smallest-divisor (n)
  (labels ((dividesp (a b)
              (= (rem b a) 0)))
    (labels ((find-divisor (n test-divisor)
                (cond
                  ((> (square test-divisor) n) n)
                  ((dividesp test-divisor n) test-divisor)
                  (t (find-divisor n (+ test-divisor 1))))))
      (find-divisor n 2))))

(defun dividesp (a b)
  (= (rem b a) 0))

(defun find-divisor (n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((dividesp test-divisor n) test-divisor)
    (t (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun primep (n)
  (= n (smallest-divisor n)))

(defun expmod (base exp m)
  (cond
    ((= exp 0) 1)
    ((evenp exp) (rem (square (expmod base (/ exp 2) m)) m))
    (t (rem (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (labels ((try-it (a)
              (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(defun fast-primep (n times)
  (cond
    ((= times 0) t)
    ((fermat-test n) (fast-primep n (- times 1)))
    (t nil)))

;;(defun sum-integers (a b)
;;  (if (> a b)
;;    0
;;    (+ a b (sum-integers (+ a 1) b))))

(defun cube (x)
  (* x x x))

;;(defun sum-cubes (a b)
;;  (if (> a b)
;;    0
;;    (+ (funcall 'cube a) (sum-cubes (+ a 1) b))))

;;(defun pi-sum (a b)
;;  (if (> a b)
;;    0
;;    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defun inc (n)
  (+ n 1))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))

(defun sum-cubes (a b)
  (sum 'cube a 'inc b))

(defun ident (x) x)

(defun sum-integers (a b)
  (sum 'ident a 'inc b))

(defun sum-squares (a b)
  (sum 'square a 'inc b))

(defun quad (x) (* x x x x))

(defun sum-quads (a b)
  (sum 'quad a 'inc b))

;; (* 8 (pi-sum 1 1000000)) overflows the stack (6 zeros!)
(defun pi-sum (a b)
  (labels ((piterm (x)
              (/ 1.0 (* x (+ x 2)))))
    (labels ((pinext (x)
                (+ x 4)))
      (sum #'piterm a #'pinext b))))

;;test (* 8 (pi-sum 100000))
;;=> 3.141572653589795

;; anonymous functions (procedures) instead of named ones version
;; again overflows with 6 zeros

(defun pi-sum (a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))
;;:test (funcall (average-damp 'square) 10)
;;=> 55

;; numerical def. integral
(defun integral (f a b dx)
  (labels ((add-dx (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx b) dx)))
;;:test (integral 'cube 0 1 0.01)
;;=>0.24998750000000042

;; anon proc. version
(defun integral (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(defun fp-sqrt (x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(defun cube-root (x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(defparameter dx 0.00001)

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x)) dx)))
;;:test (funcall (deriv 'cube) 5)
;;=> 75.00014999664018

;;:test (mapcar (deriv 'cube) '(5 4 3 2 1 0))
;;=> (75.00014999664018 48.00011999748221 27.000089999873463 12.000060000261213
;;   3.000030000110953 1.0000000000000002e-10)

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun newt-sqrt (x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(defun fixed-point-of-transform (g transform guess)
  (fixed-point (funcall transform g) guess))

(defun fp-trans-sqrt (x)
  (fixed-point-of-transform (lambda (y) (/ x y)) 'average-damp 1.0))
;;:test (fp-trans-sqrt 1632)
;;=> 40.39801975344831

(defun newt-trans-sqrt (x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) 'newton-transform 1.0))
;;:test (newt-trans-sqrt 1632)
;;=> 40.39801975344831

(defun compose (f g x)
  (funcall f (funcall g x)))
;;:test (funcall 'compose 'square 'inc 6)
;;=> 49
;;:test (compose 'square 'inc 6)
;;=> 49

;; RATS!
;;(defun make-rat (n d)
;;  (if
;;    (and (numberp n) (numberp d))
;;    (cond
;;      ((and (plusp n) (plusp d)) (cons n d))
;;      ((and (minusp n) (plusp d)) (cons (- n) d))
;;      ((and (plusp n) (minusp d)) (cons n (- d)))
;;      ((and (minusp n) (minusp d)) (cons (- n) (- d)))
;;      ((or (zerop n) (zerop d)) (cons 0 0)) (t 0)) 0))

;;;;; procedural
(defun my-cons (x y)
(lambda (m) (funcall m x y)))

(defun my-car (z)
  (funcall z (lambda (p q) (declare (ignorable p) (ignorable q)) p)))

(defun my-cdr (z)
  (funcall z (lambda (p q) (declare (ignorable p) (ignorable q)) q)))

;;;;;;;;;;;;;;;;;;;; Church numerals, procedural
(defvar zero
  (lambda (f)
    (declare (ignorable f))
    (lambda(x) (declare (ignorable x)) x)))

(defun succ(n)
  (lambda (f)
    (declare (ignorable f))
    (lambda(x) (declare (ignorable x)) (funcall f (funcall (funcall n f) x)))))

(defun plus (m n)
  (lambda (f)
    (declare (ignorable f))
    (lambda(x) (declare (ignorable x)) (funcall (funcall m f) (funcall (funcall n f) x)))))

(defun mult (m n)
  (lambda (f)
    (declare (ignorable f))
    (funcall n (funcall m f))))

(defun pred (n)
  (lambda (f)
    (declare (ignorable f))
    (lambda (x)
      (declare (ignorable x))
      (funcall (funcall (funcall n (lambda (g)
                                     (declare (ignorable g))
                                     (lambda (h)
                                       (declare (ignorable h))
                                       (funcall h (funcall g f)))))
                        (lambda (u) (declare (ignorable u)) x))
               (lambda (u) (declare (ignorable u)) u)))))

(defun sub (m n)
  (funcall (funcall n #'pred) m))

(defun cexp (m n)
  (funcall n m))

(defun czerop (n)
  (funcall (funcall n (lambda (x) (declare (ignorable x)) nil)) t))

(defun int-to-cn (n)
  (if (= n 0)
      (lambda (f)
        (declare (ignorable f))
        (lambda (x) (declare (ignorable x)) x))
      (lambda (f)
        (declare (ignorable f))
        (lambda (x)
          (declare (ignorable x))
          (funcall f (funcall (funcall (int-to-cn (- n 1)) f) x))))))

(defun cn-to-int (cn)
  (declare (ignorable cn))
  (funcall (funcall cn (lambda (x) (declare (ignorable x)) (+ x 1))) 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rat (n d)
  (cond
    ((and (symbolp n) (symbolp d)) (cons n d))
    ((and (minusp n) (minusp d)) (cons (- n) (- d)))
    ((and (plusp n) (minusp d)) (cons (- n) (- d)))
    ((and (minusp n) (plusp d)) (cons n d))
    (t (if (and (= n 0) (not (= d 0)))
           (cons 0 d)
           (cons n d)))))

(defun numer (x)
  (cond
    ((if (or (consp x) (listp x)) (car x)))
    ((if (symbolp (car x)) (car x)))
    (t (let ((g (gcd (car x) (cdr x))))
         (cond
           ((and (not (zerop (car x))) (or (plusp (car x)) (minusp (car x)))) (/ (car x) g))
           ((zerop (car x)) 0))))))

(defun denom (x)
  (cond
    ((if (or (consp x) (listp x))  (cdr x)))
    ((if (symbolp (cdr x)) (cdr x)))
    (t (let ((g (gcd (cdr x) (car x))))
         (cond
           ((and (not (zerop (cdr x))) (or (plusp (cdr x)) (minusp (cdr x)))) (/ (cdr x) g))
           ((zerop (cdr x)) 0))))))

(defun print-rat (x)
  (let ((*read-eval* nil))
    (progn (format *standard-output* "~%~a~%" (cond ((and (not (zerop (numer x))) (zerop (denom x))) 'undef) ((and (zerop (numer x)) (zerop (denom x))) 'undef) (t (/ (numer x) (denom x))))) (values))))


(defun add-rat (x y)
  (cond
    (t (if (= (denom x) (denom y))
           (make-rat (+ (numer x) (numer y)) (or (denom x) (denom y)))
           (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))))))

(defun sub-rat (x y)
  (cond 
    ((= (denom x) (denom y))
     (make-rat (- (numer x) (numer y)) (or (denom x) (denom y))))
    (t (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(defun div-rat (x y)
  ;;  (cond
  ;;    ((and (minusp (numer x)) (minusp (denom x)) ) (numer (- x) (denom (- y))))
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(defun equalp-rat (x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

;;:test (print-rat (add-rat '(1 . 3) '(1 . 2)))
;;=> 5/6
;;:test (defvar one-half (make-rat 1 2))
;;:test (defvar one-third (make-rat 1 3))
;;:test (defvar minus-one-third (make-rat -1 3))
;;:test (defvar minus-two-third (make-rat 2 -3))
;;:test (defvar one-zeroth (make-rat 1 0))
;;:test (print-rat (add-rat one-third one-third))
;;=> 2/3
;;:test (print-rat (add-rat one-third one-half))
;;=> 5/6
;;:test (print-rat (add-rat minus-one-third one-third))
;;=> 0
;;:test (print-rat (add-rat minus-one-third minus-two-third))
;;=> -1
;;:test (print-rat (add-rat one-half minus-two-third))
;;=> -1/6
;;:test (print-rat (add-rat one-half one-half))
;;=> 1
;;:test (print-rat (add-rat one-third minus-one-third))
;;=> 0
;;:test (print-rat (mul-rat one-third one-third))
;;=> 1/9
;;:test (print-rat (div-rat one-third one-third))
;;=> 1
;;:test (print-rat (div-rat one-third minus-one-third))
;;=> -1
;;:test (print-rat (sub-rat one-third minus-one-third))
;;=> 2/3
;;:test (print-rat (sub-rat one-third one-third))
;;=> 0
;;:test (print-rat (div-rat one-third one-zeroth))
;;=> 0
;;:test (print-rat (mul-rat one-third one-zeroth))
;;=> UNDEF
;;:test (print-rat (sub-rat one-third one-zeroth))
;;=> UNDEF

(defun my-cons (x y)
  (labels ((dispatch (m)
     (cond ((= m 0) x)
           ((= m 1) y)
           (t (cerror "Argument not 0 or 1 - CONS" m))))) #'dispatch))

(defun my-car (z) (funcall z 0))
(defun my-cdr (z) (funcall z 1))


;;; different rep, same thing

(defun our-cons (x y)
  (lambda (m) (funcall m x y)))

(defun our-car (z)
  (funcall z (lambda (p q) p)))

(defun our-cdr (z)
  (funcall z (lambda (p q) q)))

;;;; 0,0, 0,0, 0,0, 0,0, 0,0, 0,0
;;;; 1,0
;;;; 0,1
;;;; 2,0
;;;;
;;;; 2,3  2,3  2,3  2,3  2,3  2,3
;;;;
;;;;
;;;;2^0*3^1=1*3=3
;;;;
;;;; 0,0
;;;;
;;;;2^0*3^0=1*1=1
;;;;2^1*3^0=2*1=2
;;;;2^0*3^1=1*3=3
;;;;2^2*3^0=4*1=4
;;;;2^
;;;;2^1*3^1=2*3=6

(defun zero ()
  (lambda (x) 
    (lambda (x) x)))

(defun add-1 (n) 
  (lambda (f)
    (lambda (x) 
      (funcall f (funcall (funcall n f) x)))))

(defun one () (lambda (f) (lambda (x) (funcall f x))))

;;test
;;(cn-to-int (one))
;;=> 1

(defun two () (lambda (f) (lambda (x) (funcall f (funcall (funcall (lambda (x) x) f) x)))))

;;test
;;(cn-to-int (two))
;;=> 2

(defun list-ref (items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(defun list-rec (items)
  (if items
      (append (list (car items))
      (list-rec (cdr items)))))

(defun our-append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(defun same-parity (arg &rest rest)
  (labels ((sp-builder (result tail)
              (if (null tail)
                  result
                  (if (evenp (+ arg (car tail)))
                      (sp-builder (append result (list (car tail))) (cdr tail))
                      (sp-builder result (cdr tail))))))
    (sp-builder (list arg) rest)))

(defun our-mapcar (proc items)
  (if (null items)
      items
      (cons (funcall proc (car items)) (our-mapcar proc (cdr items)))))

(defun for-each (proc items)
  (if (null items) 
      nil
      (apply proc (car items) nil))
  (if (not (null (cdr items)))
      (for-each proc (cdr items))))

(defun for-each (items proc)
  (if (null items)
      nil
      (apply proc (car items) nil))
  (if (not (null (cdr items)))
      (for-each (cdr items) proc)))

(defun count-leaves (x)
  (cond
    ((null x) 0)
    ((not (consp x)) 1)
    (t
     (+ (count-leaves (car x)) (count-leaves (cdr x))))))


(defun last-elem (list)
  (cond
    ((null list) nil)
    (t
     (car (reverse list)))))

(defun my-reverse (list)
  (let ((l list) (result))
    (dotimes (i (length l)) (push (pop l) result))
    result))

(defun rev (l &optional (r nil)) (cond ((consp l) (rev (cdr l) (cons (car l) r))) (t r)))

(defun my-reverse2 (list)
  (labels ((rev (l &optional (r nil))
             (cond ((consp l) (rev (cdr l) (cons (car l) r))) (t r))))
    (rev list)))

(defun my-reverse3 (list)
  (labels ((rev (l &optional (r nil))
              (cond
                ((consp l)
                 (progn (print "oldlist: ") (princ l) (princ " , newlist: ") (princ r))
                 (rev (cdr l)
                      (cons (car l) r)))
                (t (progn (print "oldlist: ") (princ l) (princ ", final: ") (princ r))))))
    (rev list)))

(defun my-reverse4 (list)
  (let ((result))
    (labels ((rev (l &optional (r nil))
               (cond
                ((consp l) (append (list "oldlist: " l " , newlist: " r) (rev (cdr l) (cons (car l) r))))
                (t (list "oldlist: " l ", final: " r)))))
      (values-list (paul-graham:group (rev list) 4)))))

(defun my-reverse5 (list)
  (in-clim (common-lisp-user:nil-as-list)
           (values-list
            (prog1 
                (multiple-value-list (my-reverse4 list))
              (common-lisp-user:remove-nil-as-list)))))

(defun our-reverse (list)
  (cond
    ((null list) list)
    (t
     (append (our-reverse (cdr list)) (list (car list))))))

(defun deep-reverse (list)
  (cond
    ((null list) list)
    ((not (consp list)) list)
    ((consp list) (append (deep-reverse (cdr list)) (list (deep-reverse (car list)))))
    (t
     (deep-reverse list))))


(defun deep-reverse (list)
  (labels ((iter (tree result)
              (cond
                ((null tree) result)
                ((not (consp (car tree)))
                 (iter (cdr tree) (cons (car tree) result)))
                (t
                 (iter (cdr tree) (cons (deep-reverse (car tree)) result))))))
  (iter list nil)))


(defun deep-reverse2 (list)
  (cond ((or (null list) (not (consp list))) list)
        ((consp list) (append (deep-reverse (cdr list)) (list (deep-reverse (car list)))))
        (t (deep-reverse list))))

(defun deep-reverse3 (list)
  (labels ((iter (tree result)
              (cond
                ((null tree) result)
                ((not (consp (car tree)))
                 (iter (cdr tree) (cons (car tree) result)))
                (t
                 (iter (cdr tree) (cons (deep-reverse (car tree)) result))))))
  (iter list nil)))

(defun deep-reverse4 (list)
  (labels ((iter (tree &optional (result nil))
             (cond ((null tree) result) ((not (consp (car tree))) (iter (cdr tree) (cons (car tree) result)))
                   (t (iter (cdr tree) (cons (deep-reverse (car tree)) result))))))
    (iter list)))

;; filtering for!
(defun filter-for (pred seq)
  (cond
    ((null seq) nil)
    ((funcall pred (car seq))
     (cons (car seq) (filter-for pred (cdr seq))))
    (t (filter-for pred (cdr seq)))))

;; filtering against!
(defun eliminate (pred seq)
  (cond
    ((null seq) nil)
    ((not (funcall pred (car seq)))
     (cons (car seq) (eliminate pred (cdr seq))))
    (t (eliminate pred (cdr seq)))))

(defun accumulate (op init seq)
  (if (null seq)
      init
      (funcall op (car seq)
               (accumulate op init (cdr seq)))))

;;test
;;(accumulate #'cons nil (list 1 2 3 4 5))
;;=>(1 2 3 4 5)
;;(accumulate #'* 1 (list 1 2 3 4 5))
;;=>120
;;(accumulate #'+ 0 (list 1 2 3 4 5))
;;=>15
;;(accumulate #'list nil (list 1 2 3 4 5))
;;=>(1 (2 (3 (4 (5 nil)))))

;; ellie's recursive version
(defun accumulator (combiner null-value term a next b)
  (if (> a b)
    null-value
    (funcall combiner 
      (funcall term a)
      (accumulator combiner null-value term (funcall next a) next b))))

;; ellie's iterative version
(defun accumulator-iter (combiner null-value term a next b)
  (labels ((iter (a result)
    (if (> a b)
      result
      (iter (funcall next a)
            (funcall combiner (funcall term a) result)))))
  (iter a null-value)))

(defun enum-interval (low high)
  (if (> low high)
      nil
      (cons low (enum-interval (+ low 1) high))))
;;test
;;(enum-interval 2 7)
;;=>(2 3 4 5 6 7)

(defun enum-interval (low high &optional p)
  (let* ((result nil)
         (end-result 0))
    (do* ((low low (+ low 1)))
      ((> low high) (if (not p) end-result (values-list end-result)))
      (setf end-result (reverse (push low result))))))

(defun enum-tree (tree)
  (cond
    ((null tree) nil)
    ((not (consp tree)) (list tree))
    (t (append
        (enum-tree (car tree))
        (enum-tree (cdr tree))))))

;;test
;;enum-tree is the inverse of (accumulate #'list nil '(list))
;;(enum-tree (accumulate #'list nil (list 1 2 3 4 5)))
;;=>(1 2 3 4 5)

(defun sum (term a next b)
  (accumulator #'+ 0 term a next b))

(defun product (term a next b)
  (accumulator #'* 1 term a next b))

(defun scale-list (items factor)
  (mapcar (lambda (x) (* x factor))
              items))

(defun scale-tree (tree factor)
  (cond
    ((null tree) nil)
    ((not (consp tree)) (* tree factor))
    (t
     (cons (scale-tree (car tree) factor)
           (scale-tree (cdr tree) factor)))))

;;another version of scale-tree!
(defun scale-tree (tree factor)
  (our-mapcar (lambda (sub-tree)
         (if (consp sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (mapcar (lambda (x) (cons (car s) x)) rest)))))


(defun powerset (s) (subsets s))
(defun n-powerset (n) (expt 2 n))

(defun permutations (s)
  (if (null s)
      (list nil)
      (mapcan (lambda (x) (mapcar (lambda (p) (cons x p)) (permutations (remove x s)))) s)))
;;mapcan == flatmap, mapcar == map for CL == Scheme

(defun n-permutations (s)
	(test:factorial (length s)))

(defun permutations2 (list)
  (let ((result))
    (car (append (dolist (m list (nreverse result)) (push (permutations m) result) result)))))

(defun permutations3 (&rest list)
  (let ((result))
    (car (reverse (append (dolist (m list result) (push (permutations2 m) result) result))))))


(defun permutations4 (list)
  (let ((result))
    (values (car (permutations3 list)) (cdr (permutations3 list)))))

(defun choose (n k)
  (labels ((prod-enum (s e)
             (do ((i s (1+ i))
                  (r 1 (* i r)))
                 ((> i e) r)))
           (fact (n)
             (prod-enum 1 n)))
    (/ (prod-enum (- (1+ n) k) n) (fact k))))

(defun choose (n k)
  (when (<= k n)
    (/ (factorial n)
       (facttorial k) (facttorial (- n k)))))

(defun binom (n k)
  (if (or (< n k) (< k 0))
    NIL  ; there are better ways to handle errors in Lisp
    (binom-r n k 1)))

;; acc is an accumulator variable
(defun binom-r (n k acc)
  (if (or (= k 0) (= n k))
    acc
    (binom-r (- n 1) (- k 1) (* acc (/ n k)))))

(defun binom (n k &optional (acc 1))
  (cond ((or (< n k) (< k 0)) NIL)
        ((or (= k 0) (= n k)) acc)
        (T (binom (- n 1) (- k 1) (* acc (/ n k))))))

(defun combinations (count list)
  (cond ((zerop count) (list nil)) 
        ((endp list) nil)
        (t
         (nconc
          (mapcar
           (let ((item (car list)))
             (lambda (combi) (cons item combi)))
           (combinations (1- count) (cdr list)))
          (combinations count (cdr list))))))

(defun square-tree (tree)
  (cond
    ((null tree) nil)
    ((not (consp tree)) ((lambda (x) (* x x)) tree))
    (t
     (cons (square-tree (car tree)) (square-tree (cdr tree))))))

;;;2 + 3 x + ........5 x^3 + .......2 x^5
;;; 2 + 3x + 0x^2 + 5x^3 + 0x^4+ 2x^5 at x=2

;;test
;; (+ (* (+ (* (+ (* (+ (* (+ (* 2 2) 0) 2) 5) 2) 0) 2) 3) 2) 2)
;;=> 112

;;; 2 3 0 5 0 2 is the coeff-seq from left to right
;;; but it's processed above from right-to-left (or inwards to outwards)
;;; as 2 0 5 0 3 2
;;; the (* ...2) things are for eval at x=2
;;; the inntermost (* 2...) 2 is the highest coeff factor (the 2 for x^5)
;;; the outermost + is the last (or first, depends how you write the poly) term of the poly
;;; for an order of 5 it gives 5 +'es 5 *'s and 10 paren-pairs (since the *'s and +'s are alternating
;;; an order of 5 polynomial contains tho 6 terms!

(defun horner-eval (x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coeff-seq))

;;test
;; the coeff-seq's for our polys are writeen from left-to-right (least-term->higher-order)
;;(horner-eval 2 (list 2 3 0 5 0 2))
;;=> 112

(defun accumulate-n (op init seq)
  (if (null (car seq))
      nil
      (cons (accumulate op init (our-mapcar #'car seq))
            (accumulate-n op init (our-mapcar #'cdr seq)))))

;;
(defvar matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(defun dot-product (v1 v2)
  (accumulate #'+ 0 (mapcar #'* v1 v2)))

;;column-major (math standard)
(defun mat*vec (m v)
  (our-mapcar (lambda (m-col) (dot-product m-col v)) m))

;;row-major
(defun vec*mat (v m)
  (our-mapcar (lambda (m-row) (dot-product v m-row)) m))

(defun transpose (m)
  (accumulate-n #'cons nil m))

(defun mat*mat (m n)
  (let ((n-cols (transpose n)))
    (our-mapcar 
     (lambda (m-row)
       (our-mapcar 
        (lambda (n-col)
          (dot-product m-row n-col))
        n-cols))
     m)))

;;inner proc is mat*vec
(defun mat*mat (m n)
  (let ((n-cols (transpose n)))
    (our-mapcar (lambda (m-row) (mat*vec n-cols m-row)) m)))


;; accumulate is fold-right!
(defun fold-right (op init seq)
  (if (null seq)
      init
      (funcall op (car seq)
               (fold-right op init (cdr seq)))))

(defun fold-left (op init seq)
  (labels ((iter (result rest)
              (if (null rest)
                  result
                  (iter (funcall op result (car rest))
                        (cdr rest)))))
  (iter init seq)))

;;test

;;(fold-right #'/ 1 (list 1 2 3))
;;=> 3/2
;; (fold-left #'/ 1 (list 1 2 3))
;;=> 1/6
;; (fold-right #'list nil (list 1 2 3))
;;=>(1 (2 (3 nil)))
;; (fold-left #'list nil (list 1 2 3))
;;(((nil 1)2)3)

(defun reverse-using-right (list)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              list))

(defun reverse-using-left (list)
  (fold-left (lambda (x y) (append (list y) x))
             nil
             list))

;;test
;;(reverse-using-right '(1 2 3))
;;=>(3 2 1)

;; flatmap of scheme is equal to mapcar of common-lisp (upto handling multiple seqs....)
(defun flatmap (proc seq)
  (accumulate 'append nil (our-mapcar proc seq)))


(defun lottery (&optional (p ()))
  (let ((non-randoms '(49 49 49 49 49 49)) (random-tuple))
    (dolist
        (x non-randoms
           (if (not p)
               random-tuple
               (values-list random-tuple)))
      (push (1+ (random x)) random-tuple))))


(defun lottery2 (&optional (p ()))
  (let ((random-tuple))
    (dotimes
        (i 6 (if (not p)
                 random-tuple
                 (values-list random-tuple)))
      (push (1+ (random 49)) random-tuple))))

(defun lottery3 (&optional (p ()))
  (let ((result))
    (push (mapcar (lambda (x) (1+ (random x))) '(49 49 49 49 49 49)) result)
    (if p
        (values-list (nreverse (car result)))
        (nreverse (car result)))))

(defun lottery4 (&optional (p ()))
  (let ((result))
    (push (mapc (lambda (x) (1+ (random x))) '(49 49 49 49 49 49)) result)
    (if p
        (values-list (nreverse (car result)))
        (nreverse (car result)))))

(defun lottery5 (&optional p)
  (do* ((non-randoms '(49 49 49 49 49 49) non-randoms)
        (randoms nil (mapcar (lambda (x) (1+ (random x))) non-randoms))
        (result nil result)
        (i 1 (1+ i)))
    ((> i 6)
     (let ((final (nreverse (car (push randoms result)))))
       (if p
           (values-list final)
           final)))))

(defun random-seq (seq count &optional multi)
  (let ((result))
    (dotimes (x count) (push (elt seq (random (length seq))) result))
    (if multi
        (values-list (nreverse result))
        (nreverse result))))

(defun row-sum (min max n)
  (let ((list (test::enum-interval min max)) (result))
    (values-list
     (dotimes (i (length (pg:group list n)) (values (nreverse result)))
       (push
        (list (nth i (pg:group list n))
              (format nil "sum: ~a" (reduce '+ (nth i (pg:group list n)))))
        result)))))

(defun col-sum (min max n)
  (let ((list (test::enum-interval min max)) (result))
    (values-list
     (dotimes (i (1+ (length (pg:group list n))) (values (nreverse result)))
       (push
        (list (remove 'nil (mapcar (lambda (x) (nth i x)) (pg:group list n)))
              (format nil "sum: ~a" (reduce '+ (remove 'nil (mapcar (lambda (x) (nth i x)) (pg:group list n))))))
        result)))))

(defun levels (tree &optional (depth -1))
  "print depth of nesting of each atom in exp"
  (let ((result))
    (if (atom tree)
        (append result depth)
        (dolist (leaf tree (nreverse result)) (push (levels leaf (1+ depth)) result)))))

(defun node-p (tree)
  "predicate for top-level atoms/nodes in exp"
	(cond 
		((null tree) 1)
		((consp tree) 1) 
		(t 0)))

(defun leaf-p (tree)
  "predicate for top-level atoms/leafs in exp"
	(cond 
		((null tree) 0) 
		((atom tree) 1) 
		(t 0)))

(defun count-nodes-flat (tree)
  "return the top-level number of atoms in exp"
	(apply #'+
				 (mapcar
					(lambda (tree)
						(cond 
							((null tree) 1) 
							((consp tree) 1) 
							(t 0)))
					tree)))

(defun count-leaves-flat (tree)
  "return the top-level number of atoms in exp"
	(apply #'+ 
				 (mapcar 
					(lambda (tree) 
						(cond 
							((null tree) 0) 
							((atom tree) 1) 
							(t 0))) 
					tree)))

(defun count-nodes-flat (tree)
  "return the top-level number of atoms in exp"
	(apply '+ (mapcar #'node-p tree)))

(defun count-leaves-flat (tree)
  "return the top-level number of atoms in exp"
	(apply '+ (mapcar #'leaf-p tree)))


(defun count-nodes-flat (tree)
  "return the total number of non-nil elems in exp"
  (cond ((null tree) 1) 
        ((atom tree) 0) 
        ((consp (car tree)) (+ 1 (count-nodes-flat (cdr tree))))
        (t (count-nodes-flat (cdr tree)))))

;; counts same level ones, but recurses down
(defun count-nodes (tree)
  "return the total number of non-nil elems in exp"
  (cond ((null tree) 1) 
        ((atom tree) 0) 
        ((consp (car tree)) (+ 1 (count-nodes (cdr tree))))
        (t (+
            (count-nodes (car tree))
            (count-nodes (cdr tree))))))

;; counts same level ones, but recurses down
(defun count-leaves (tree)
  "return the total number of non-nil elems in exp"
  (cond 
		((null tree) 0) 
		((atom tree) 1) 
		(t (+ 
				(count-leaves (car tree)) 
				(count-leaves (cdr tree))))))

;; counts all levels
(defun count-nodes-deep (tree &optional (if-null 1))
  "return the total number of atoms in the expression.
counting nil as an atom only in non-tail position"
  (cond ((null tree) if-null) 
        ((atom tree) 0) 
        ((consp (car tree)) (+ 1 (count-nodes-deep (cdr tree) 1)))
        (t (+ 
            (count-nodes-deep (car tree) 0) 
            (count-nodes-deep (cdr tree) 1)))))

;; counts all levels
(defun count-leaves-deep (tree &optional (if-null 1))
	"return the total number of atoms in the expression.
counting nil as an atom only in non-tail position"
	(cond 
		((null tree) if-null) 
		((atom tree) 1)
		(t (+ 
				(count-leaves-deep (car tree) 1)
				(count-leaves-deep (cdr tree) 0)))))

(defun count-node (node tree)
  "count the times item appears anywhere within tree"
  (cond 
		((eql node tree) 1)
    ((consp (car tree)) (+ 1 (count-node node (cdr tree))))
    ((atom tree) 0)
		(t (+ 
				(count-node node (car tree)) 
				(count-node node (cdr tree))))))


(defun count-leaf (leaf tree)
  "count the times item appears anywhere within tree"
  (cond 
		((eql leaf tree) 1) 
		((atom tree) 0)
		(t (+ 
				(count-leaf leaf (car tree)) 
				(count-leaf leaf (cdr tree))))))

(defun gen-dec (n)
  (loop for i from 2 to n do
        (eval
         `(defun ,(pg:symb i '-) (x) 
            (- x ,i)))))

(defun gen-inc (n)
  (loop for i from 2 to n do
        (eval
         `(defun ,(pg:symb i '+) (x) 
            (+ x ,i)))))

(defun mappend (fn list)
  (apply 'append (mapcar fn list)))

(defun part (list)
  (let ((odds) (evens))
    (mapcar
     (lambda (x)
       (if (oddp x)
           (push x odds)
           (push x evens)))
     list)
    (list (nreverse odds) (nreverse evens))))

(defun pairs (items)
  (let ((result
         (cond ((null items) nil)
               ((null (cdr items)) nil)
               ((listp items) (cons (list (car items) (cadr items)) (pairs (cddr items)))))))
    result))

(defun offpairs (items)
  (let ((result
         (cond 
           ((null items) nil)
           ((listp items) (cons (list (car items) (cadr items)) (offpairs (cddr items)))))))
    result))

(defun separate-pairs (list)
  (let* ((list (pairs list))
         (fs
          (loop for elt in list
                collect (subst (car elt) list list)))
         (ss
          (loop for elt in list
                collect (subst (cadr elt) list list))))
    (list fs ss)))


(defun intervals (items)
  (let ((result
         (cond 
           ((null items) nil)
           ((null (cdr items)) nil)
           ((listp items)
            (cons (list (car items) (cadr items)) (intervals (cdr items)))))))
    result))

(defun part3 (list)
  (labels ((recurser (items)
             (let ((cons
                    (cond ((null items) nil) 
                          ((atom items) (list (car items)))
                          ((listp items) (cons (list (car items) (cadr items)) (recurser (cddr items)))))))
               cons)))
    (values (map 'list 'car (recurser list)) (map 'list 'cadr (recurser list)))))

;;; this one is a copy of partition-if from paip so to say!
(defun part4 (pred list)
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values 
     (nreverse no-list) 
     (nreverse yes-list))))

(defun cartesian (sets)
  (cond ((null sets) (list nil))
        (t (mapcan #'(lambda (cartesian)
                       (mapcar #'(lambda (elmt)
                                   (cons elmt cartesian))
                               (car sets)))
                   (cartesian1 (cdr sets))))))

 (defun partition (set)
   (cond ((null set) (list nil))
         (t (let ((first-elem (car set))
                  (new-partition nil))
              (dolist (part (partition (cdr set)))
                (push (cons (list first-elem) part)
                      new-partition)
                (do ((p part (cdr p)))
                    ((null p))
                  (push (nconc (list (cons first-elem (car p)))
                               (ldiff part p)
                               (cdr p))
                        new-partition)))
              new-partition))))
 
(defun bell (n)
  "Bell's number B(n), the number of partitions of an n-element set"
  (if (= n 0)
      1
    (loop for k below n
          sum (* (binomial k (1- n)) (bell k)))))

;; this one is from paip too just translated to/for lists
(defun dot-product (a b)
  "compute the dot product of two lists."
  (if (or (null a) (null b))
      nil
      (cons (append (list (car a)) 
                    (list (car b)))
            (dot-product (cdr a) (cdr b)))))
;;; test like
;; (defvar test '(0 1 2 3 4 5 6 7 8 9))
;; (dot-product (cadr (part test)) (car (part test)))
;; (pairs test)
;; (pg:group test 2)

(defun push-in (list)
  (let ((result))
    (dolist (x list (nreverse result))
      (cond (t (push (list x) result))))))

;;; test
;; (pull-out test)

(defun pull-out (list)
  (let ((result))
  (dolist (x list (nreverse result))
    (cond ((atom x) (push x result))
          ((listp x) (push (car x) result))
          (t nil)))))

;;; test
;; (push-in (pull-out test))

;; this one is combine-all from paip
(defun distribute-a (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (distribute '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append  x y)) xlist))
           ylist))

(defun distribute-b (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (distribute '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append y x)) ylist))
           xlist))

;;; test
;; (distribute-a (push-in test) (push-in test))
;; (distribute-b (push-in test) (push-in test))


;; with these ones there're implicit push-ins
(defun distribute-c (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (distribute '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append  (list x) (list y))) xlist))
           ylist))

(defun distribute-d (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (distribute '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append (list y) (list x))) ylist))
           xlist))

;;; test
;; (distribute-c test test)
;; (distribute-d test test)

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       ylist))
           xlist))

(defun truth-table (op)
  (let ((x '(0 1))
        (y '(0 1)))
  (cross-product (lambda (x y) (eval `(,op ,x ,y))) x y)))

(defun isomorph-p (a b)
  (cond
    ((atom a) (atom b))
    ((atom b) nil)
    ((isomorph-p (car a) (car b))
     (isomorph-p (cdr a) (cdr b)))
    (t nil)))

(defun mklist (x)
  (if (listp x) x (list x)))


(defun nodes-deep (tree &optional (if-null nil))
  "return the subnodes in the expression."
  (cond ((null tree) if-null)
        ((atom tree)
         (if (listp tree)
             (append (cons (car tree) (nodes-deep (car tree))) (nodes-deep (cdr tree)))
             tree))
        ((listp (car tree))
         (cons (car tree) (nodes-deep (cdr tree))))
        (t (cons (nodes-deep (car tree)) (nodes-deep (cdr tree))))))

(defun intersperse (thing list)
  "Put thing between each of the elements in list.
  (intersperse '|,| '(1 2 3)) => (1 |,| 2 |,| 3)"
  (rest (mapcan (lambda (x) (list thing x)) list)))

(defun pre2in (expr)
  "translate prefix to infix expressions.
  handles operators with any number of args."
  (if (atom expr)
      expr
      (intersperse (car expr) (mapcar #'pre2in (cdr expr)))))

(defun remove-brackets (lst)
   "reduses lists with just one item to the item itself"
   (do ((result lst (car result))) ((or (not (consp result)) (not (null (cdr result)))) result)))

 (defun separate-list (lst separator test)
   "returns list of sub-sequences defined by separator"
   (if (not (consp lst))
       lst
       (let ((result (cons separator nil))
             (end 0)
             (sub)
             (lst
              (if (funcall test (car lst) separator)
                  (cdr lst)
                  lst)))
         (do ()
             ((null lst) result)
           (setf end (position separator lst :test test))
           (setf sub (cons (subseq lst 0 end) nil))
           (setf result (append result sub))
           (setf lst
                   (if end
                       (nthcdr (+ 1 end) lst)
                       nil)))
         (setf (cdr result) (mapcar #'remove-brackets (cdr result)))
         result)))
 
 (defun separate-tree (lst separator test)
   "apply separate-list on all sublists"
   (if (or (not (consp lst)) (eql (first lst) 'quote))
       lst
       (progn
        (setf lst
                (mapcar
                 #'(lambda (x)
                     (if (not (consp x))
                         x
                         (separate-tree x separator test)))
                 lst))
        (if (not (find separator (rest lst)))
            lst
            (separate-list lst separator test)))))
 
 (defun in2pre (infix-expr &key (test #'eql))
   "converts an infix expression to prefix"
   (let ((result infix-expr) (separators '(+ - * /)))
     (dolist (sep separators) (setf result (separate-tree result sep test)))
     (remove-brackets result)))
 
(defun simple-interest (interest-rate balance time-periods)
  (* (/ (/ interest-rate 100.0) 12) balance time-periods))

(defun simple-pay-off (interest-rate balance time-periods)
  (+ balance (simple-interest interest-rate balance time-periods)))

(defun find-simple-balance (pay-off interest-rate)
  (/ pay-off (1+ (/ interest-rate 100.0))))
