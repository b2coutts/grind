#lang racket

(provide
  first-aid second-aid third-aid
  breeze static ember snipe
)

(require "state.rkt" "util.rkt" "combat.rkt")
(require data/gvector)

;; --------------------------------skill helper functions--------------------------------

;; constructs a healing skill
(define/contract (make-heal name power)
  (-> string? integer? skill?)
  (skill name 'heal power 0 (lambda (st loc)
    (define usr (state-user st))
    (define amt (+ (actor-stat usr 'skl) power))
    (set-actor-hp! usr (min (actor-stat usr 'maxhp) (+ (actor-hp usr) amt)))
    (list (list 'heal amt usr)))))

;; constructs a damaging skill
(define/contract (make-dmgr name power range)
  (-> string? integer? integer? skill?)
  (skill name 'damage power range (lambda (st loc)
    (define usr (state-user st))
    (define enms (grmap-enemies (state-fmap st)))
    (define target-idx (find-target st loc))
    (cond
      [(not target-idx) (err (format "No target at ~a,~a!" (car loc) (cdr loc)))]
      [(> (loc-dist (actor-loc usr) loc) range)
        (err "Target out of range!")]
      [else (define target (gvector-ref enms target-idx))
            (define dmg (+ (actor-stat usr 'skl) power
                           (- (actor-stat target 'def))))
            (append
              (list (list 'info (format "You used ~a on ~a." name (actor-name target))))
              (apply-damage! st target-idx dmg))]))))


;; ------------------------------------actual skills------------------------------------=
(define first-aid (make-heal "First Aid" 0))
(define second-aid (make-heal "Second Aid" 5))
(define third-aid (make-heal "Third Aid" 5))

(define breeze (make-dmgr "Breeze" 0 2))
(define static (make-dmgr "Static" 3 1))
(define ember (make-dmgr "Ember" 4 1))
(define snipe (make-dmgr "Snipe" 3 6))
