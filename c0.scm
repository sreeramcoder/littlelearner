;; TEXT section
;; functions in the text
;; present *exactly* as they appear in the text.
;; nothing more, nothing less

#lang racket

(define add3
  (lambda (x)
    (+ 3 x)))

(define pie 3.14)

(cond
 ((= pie 4) 28)
 ((< pie 4) 33)
 (else 17))

(define z 20)

(define double-result-of-f
  (lambda (f)
    (lambda (z)
      (* 2 (f z)))))

(define add
  (lambda (n m)
    (cond
     ((= 0 m) n)
     (else (add (add1 n) (sub1 m))))))



(define line
  (lambda (x)
    (lambda (theta)
      (let ((theta0 (first theta))
	    (theta1 (second theta)))
	(+ theta1 (* theta0 x))))))


(define (scalar? n)
  (number? n))


;; any function below here, can be deleted
;; 

(add3 10)

(define (adder x)
  (+ x y))

(define y 30)
