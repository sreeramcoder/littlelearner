;; TEXT section
;; functions in the text
;; present *exactly* as they appear in the text.
;; nothing more, nothing less

#lang racket

(require malt)


(define line
  (lambda (x)
    (lambda (theta)
      (let ((theta0 (first theta))
	    (theta1 (second theta)))
	(+ theta1 (* theta0 x))))))


(define (scalar? n)
  (number? n))

(length '(1 2 3))

# this gets the shape of the tensor
(define shape
  (lambda (t)
    (cond
     ((scalar? t) '())
     (else (cons (length t)
		 (shape (car t)))))))

# the rank of a tensor is the length of its shape

(define (rank t)
  (define (ranked a t1)
    (cond
     ((scalar? t1) a)
     (else (ranked (add1 a) (car t1)))))
  (ranked 0 t))

;; any function below here, can be deleted
;; 

