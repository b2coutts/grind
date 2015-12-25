#lang racket

(require "init.rkt" "backend.rkt" "frontend.rkt")

(define st game-state)

(display-map st)

(attack! st (cons 3 4))
(attack! st (cons 1 3))

(move-user! st (cons 1 2))
(attack! st (cons 3 4))
(attack! st (cons 1 3))
(attack! st (cons 1 3))

(display-map st)
