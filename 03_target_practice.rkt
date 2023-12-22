(require malt)

;; This is for Chapter-5 in the
;; Little Learner book

(declare-hyper revs)
(declare-hyper alpha)

;; revise 'theta' 'revs' times, by calling 'f' every time.
;; seems to do something like fixed point arithmetic ?
;; try calling this f which implements a round of koprekar's transformation

(define revise
  (lambda (f revs theta)
    (cond
     ((zero? revs) theta)
     (else
      (revise f (sub1 revs) (f theta))))))

;; f - function that calculates the gradient of
;; objective function at a particular theta and calculates
;; a new theta based on the gradient and the
;; learning rate (a hyper parameter)

;; gradient-descent accepts objective function and initial theta
;; as input. 'f' is then computed which internally needs
;; the objective function. 
;; it then revises the theta, 'revs' times.
;; every revision  is a 'learning' !!
;; final theta after 'revs' revision is the learned parameter.


(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (theta)
	       (map (lambda (p g)
		      (- p (* alpha g))) ;gradient 'g' is multiplied by the learning rate alpha.
		    theta
		    (gradient-of obj theta)))))
      (revise f revs theta))))

;; target-fn : predicts ys for xs (using some approach e.g.linear, quadratic...)
;; expectant-fn = (l2-loss target-fn)
;; objfn = (expectant-fn xs ys)
(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
	(let ((pred-ys ((target xs) theta)))
	  (sum
	   (sqr
	    (- ys pred-ys))))))))

(define line-xs (tensor 2.0 1.0 4.0 3.0))

(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define line
  (lambda (x)
    (lambda (theta)
      (let ((theta0 (first theta))
	    (theta1 (second theta)))
	(+ theta1 (* theta0 x))))))

;; run gradient descent

(with-hypers
 ((revs 1000)
  (alpha 0.01)) 
 (gradient-descent  
  ((l2-loss line) line-xs line-ys)
  (list 0 0)))
 



	       
