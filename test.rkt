#lang racket

(require "init.rkt" "backend.rkt" "frontend.rkt")

(define st game-state)

(display-map st)
(display-skills st)

(learn-skill! st 0 0)

(attack! st (cons 3 4))
(attack! st (cons 1 3))

(move-user! st (cons 1 2))
(attack! st (cons 3 4))
(attack! st (cons 1 3))
(attack! st (cons 1 3))

(display-map st)
(display-skills st)

(display-skill-info st 0 0)
(display-skill-info st 1 1)
(display-skill-info st 0 2)
(display-skill-info st 3 3)
