#lang racket

(provide apply-attack!)

(require "state.rkt")

;; applies the effects of an attack; produces the damage dealt
(define/contract (apply-attack! attacker target)
  (-> actor? actor? integer?)
  (define dmg (max 0 (+ 2 (- (actor-str attacker) (actor-def target)))))
  (set-actor-hp! target (- (actor-hp target) dmg))
  dmg)
