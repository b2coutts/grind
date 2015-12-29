#lang racket

(provide apply-damage!)

(require "state.rkt")
(require data/gvector)

(define response? (listof any/c))

;; applies damage to an enemy, handling effects (i.e. death)
(define/contract (apply-damage! st target-idx dmg)
  (-> state? integer? integer? response?)
  (define enms (grmap-enemies (state-fmap st)))
  (define target (gvector-ref enms target-idx))
  (set-actor-hp! target (max 0 (- (actor-hp target) dmg)))
  (append
    (list (list 'damage dmg target))
    (cond
      [(= (actor-hp target) 0)
        (gvector-remove! enms target-idx)
        (set-actor-sp! (state-user st) (+ (actor-sp (state-user st)) (actor-sp target)))
        (list (list 'death target))
        (list (list 'sp (actor-sp target)))]
      [else '()])))
