;; This covers my solutions for interlude-2
;; of the book "The Little Learner"
;; by Daniel P.Friedman and Anurag Mendhekar

#lang racket

(require malt)

;; it is necessary to declare the hyper parameters
;; before using them in 'with-hypers'
(declare-hyper smaller)
(declare-hyper larger)

;; with-hypers modifies the environment with new values of smaller
;; and larger.

(with-hypers
 ((smaller 1)
  (larger 2))
  (+ smaller larger))

(define nonsense?
  (lambda (x)
    (= (sub1 x) smaller)))

(with-hypers
 ((smaller 5))
 (nonsense? 6))

