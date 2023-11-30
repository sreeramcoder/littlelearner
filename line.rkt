#lang racket/gui

(define frame (new frame% [label "Example"] [width 400] [height 300]))

(define canvas (new canvas% [parent frame]
                            [paint-callback (λ (canvas dc)
                                             (send dc draw-line 0 0 100 100))]))

(send frame show #t)


(λ (x) (+ x x))
