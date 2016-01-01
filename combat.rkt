#lang racket

(provide atk-damage enemy-damage! user-damage!)

(require "state.rkt" "util.rkt")
(require data/gvector)

(define response? (listof any/c))

;; calculates the damage of an attack
(define/contract (atk-damage atker target)
  (-> actor? actor? integer?)
  (max 0 (- (actor-stat atker 'str) -2 (actor-stat target 'def))))

;; applies damage to an enemy, handling effects (i.e. death)
(define/contract (enemy-damage! st target-idx dmg skname)
  (-> state? integer? integer? (or/c 'attack string?) response?)
  (define enms (grmap-enemies (state-fmap st)))
  (define target (gvector-ref enms target-idx))
  (set-actor-hp! target (max 0 (- (actor-hp target) dmg)))
  (append
    (list (list 'enemy-damage skname dmg target))
    (cond
      [(= (actor-hp target) 0)
        (gvector-remove! enms target-idx)
        (set-actor-sp! (state-user st) (+ (actor-sp (state-user st)) (actor-sp target)))
        (list (list 'death target))
        (list (list 'sp (actor-sp target)))]
      [else '()])))

;; applies damage to the user (atkr is attacking offender, skname is the name of the skill used, for
;; frontend message purposes.
;; TODO: make something actually happen when user dies
(define/contract (user-damage! st dmg atkr skname)
  (-> state? integer? actor? (or/c 'attack string?) response?)
  (set-actor-hp! (state-user st) (max 0 (- (actor-hp (state-user st)) dmg)))
  (cons (list 'user-damage atkr skname dmg)
        (if (<= (actor-hp (state-user st)) 0) '((user-death)) '())))
