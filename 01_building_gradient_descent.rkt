;; This covers up to chapter-4
;; of the book "The Little Learner"
;; by Daniel P.Friedman and Anurag Mendhekar

#lang racket

(require malt)

;; we determine values for theta0 and theta for a known x coordinate.
;; this is the reason why we first take x as input

;; since theta is used to calculate y for a given x, it is called as a 'parameter'

;; x is called as the argument.

;; functions that take parameters after the arguments are called as 'parameterized functions'
;; here 'line' is a parameterized function.


;; step-1: provide a x -> receive a function A (aka objective function)
;; step-2: evaluate A(theta): get the output 
;;
;; functions that receive theta as input are called as objective functions.
;; their output is loss (scalar).
;; called so,may be because their objective is to produce well fitting final output,
;; by making loss as close to zero as possible

;; identifying the values of parameters from input data is called as 'learning'

;; (UDL book)
;; Since we use a linear model, we have a family of possible parameters that can be considered.
;; Loss is a quantification of mismatch between the predicted and actual outputs.
;; A loss function is used to determine a member of the family that results in minimum loss.

(define line
  (lambda (x)
    (lambda (theta)
      (let ((theta0 (first theta))
	    (theta1 (second theta)))
	(+ theta1 (* theta0 x))))))

;; High Speed Intro to Tensors (learn by examples)

;; scalars : 0, 1, 42, 100... all of these are scalars.
;; scalars also called tensor-0
;;
;; now lets consider an example tensor
;;
;; (define T (tensor (tensor 1 2 3) (tensor 4 5 6)))
;;  here, T is a tensor-2                    
;;
;; Utility tensor functions
;;
;; (shape T) ---outputs--> (2 3)
;;
;; (rank T) ---outputs--> 2
;;
;; (tlen T) --outputs--> 2
;; 
;; (tref T 0) --outputs--> (tensor 1 2 3)
;;
;; (tlen (tref T 0)) --outputs--> 3
;;
;; (tensor 1 2) - this creates a tensor-1 tensor comprised of two tensor-0s (scalars)
;; a tensor-m tensor has tensor-(m-1) tensors as its members. each of the tensor-(m-1) tensors must have same shape [this is a basic requirement for tensors]
;; e.g.
;; (tensor (tensor 1 2) (tensor 3 4)) -> valid tensor
;; (tensor (tensor 1 2) (tensor 3)) --> invalid tensor
;; (tensor (tensor 1) (tensor 3)) --> valid tensor
;;


(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
	(let ([pred-ys ((target xs) theta)])
	  (sum (sqr
		(- ys pred-ys))))))))

;;

(define line-xs (tensor 2.0 1.0 4.0 3.0))

(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define theta (list 0.0 0.0))

;; this function accepts the parameters (theta) as input
;; and outputs the predicted y's

(define obj-fn
  ((l2-loss line) line-xs line-ys))

;; revise theta , 'revs' number of times.
;; f is the function that provides next theta value, based on the
;; current value of theta
;; revs is the number of revisions to be made

(define (revise f revs theta)
  (cond
   ((zero? revs) theta)
   (else (revise f (sub1 revs) (f theta)))))

;; output of below function is given as
;; input (f) for the revise function.	

(define (next-theta obj-fn learning-rate)
  (lambda (theta)
    (map
     (lambda (x y) (- x y))
     theta
     (map
      (lambda (x) (* x learning-rate))
      (gradient-of obj-fn theta)))))


;; as per book - version 1
;; alpha - learning rate
;; notice that gradients are computed for each value of theta.
;; theta is revised by a small value that is proportional to the gradient.


(let ((alpha 0.01)
      (obj ((l2-loss line) line-xs line-ys)))
  (let ((f (lambda (theta)
	     (let ((gs (gradient-of obj theta)))
	       (list
		(- (ref theta 0) (* alpha (ref gs 0)))
		(- (ref theta 1) (* alpha (ref gs 1))))))))
    (revise f 10000 (list 0.0 0.0))))

;; using my next-theta function
(revise (next-theta obj-fn 0.01) 10000 (list 0 0))

;; as per book - version 2
;; only change, compared to earlier version, is that the parameters are now
;; revised using a map function.

(let ((alpha 0.01)
      (obj ((l2-loss line) line-xs line-ys)))
  (let ((f (lambda (theta)
	     (let ((gs (gradient-of obj theta)))
	       (map
		(lambda (p g)
		  (- p (* alpha g)))
		theta
		gs)))))
    (revise f 10000 (list 0.0 0.0))))

;; gradient descent - version 1

(define alpha 0.01)
(define revs 1000)

(define gradient-descent
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
	       (let ((gs (gradient-of obj big-theta)))
		 (map
		  (lambda (p g)
		    (- p (* alpha g)))
		  big-theta
		  gs)))))
      (revise f revs theta))))

