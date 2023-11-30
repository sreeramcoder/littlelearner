#lang racket
(require malt/learner)

( print-tensor (+ (tensor 2) (tensor 7)))

(+ (tensor 1) (tensor 2))

(define a (tensor 1 2 3))

(define s (+ (tensor 5 6 7) (tensor 2 0 1)))

(define t (+ (tensor (tensor 4 6 7) (tensor 2 0 1))
	     (tensor (tensor 1 2 2) (tensor 6 3 1))))

