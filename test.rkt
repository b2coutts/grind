#lang racket

(require "init.rkt" "backend.rkt")

(define st game-state)

(attack! st (cons 3 4))
(attack! st (cons 1 3))

(move-user! st (cons 1 2))
(attack! st (cons 3 4))
(attack! st (cons 1 3))
(attack! st (cons 1 3))
