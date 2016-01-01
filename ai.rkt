#lang racket
;; provides functions for controlling enemies

(provide enemy-turn!)

(require "state.rkt" "util.rkt" "combat.rkt")

;; TODO: make enemy radius of vision enemy-specific
(define vision-radius 12)

;; executes one turn for a single enemy
(define/contract (single-turn! st enm)
  (-> state? actor? (listof any/c))
  (match-define (cons ux uy) (actor-loc (state-user st)))
  (match-define (cons x y) (actor-loc enm))
  (define usr-dist (loc-dist (cons ux uy) (cons x y)))
  (define moves (filter (lambda (loc) (and (< (loc-dist loc (cons ux uy)) usr-dist)
                                           (space-free? st (car loc) (cdr loc))))
                        (map (lambda (off) (cons (+ x (car off)) (+ y (cdr off))))
                             '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))))
  (cond
    [(<= (actor-mvs enm) 0) '()]
    [(> usr-dist vision-radius) '()]
    [(<= usr-dist (actor-stat enm 'ran))
      (user-damage! st (atk-damage enm (state-user st)) enm 'attack)]
    [(empty? moves) '()]
    [else (define mv (list-ref moves (random (length moves))))
          (set-actor-loc! enm mv)
          (set-actor-mvs! enm (sub1 (actor-mvs enm)))
          (cons (list 'enemy-move mv) (single-turn! st enm))]))

;; executes one turn for all enemies; resets user's mvs
;; TODO: enemy skills
(define/contract (enemy-turn! st)
  (-> state? (listof any/c))
  (define resp (append-map (curry single-turn! st) (liv-enms st)))
  (for ([actr (cons (state-user st) (liv-enms st))])
    (set-actor-mvs! actr (actor-stat actr 'spd)))
  resp)
