(require malt)

;; This is for Chapter-5 in the
;; Little Learner book

(declare-hyper revs)
(declare-hyper alpha)

;; revise
;; seems to do something like fixed point arithmetic ?
;; try calling this f which implements a round of koprekar's transformation

(define revise
  (lambda (f revs theta)
    (cond
     ((zero? revs) theta)
     (else
      (revise f (sub1 revs) (f theta))))))

(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (theta)
	       (map (lambda (p g)
		      (- p (* alpha g))) ;gradient 'g' is multiplied by the learning rate alpha.
		    theta
		    (gradient-of obj theta)))))
      (revise f revs theta))))

(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
	(let ((pred-ys ((target xs) theta)))
	  (sum
	   (sqr
	    (- ys pred-ys))))))))

	       
