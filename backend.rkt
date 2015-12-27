#lang racket
;; provides user-facing functions for manipulating the game state

(provide response? move-user! attack! learn-skill! use-skill!)

(require "state.rkt" "util.rkt" "combat.rkt")
(require data/gvector)

;; a response to the frontend's move request
(define response? (listof any/c))

;; moves the user to the given location
;; TODO: should trigger AI
(define/contract (move-user! st loc)
  (-> state? location? response?)
  (match-define (state usr _ fmap) st)
  (cond
    [(> (loc-dist (actor-loc usr) loc) 1) (err "Cannot move that far!")]
    [(or (equal? (grmap-ref fmap (car loc) (cdr loc)) 'wall)
         (member loc (map actor-loc (liv-enms st))))
      (err "There's something in the way!")]
    [else (set-actor-loc! usr loc)
          (list (list 'move loc))]))

;; makes the user attack a location
(define/contract (attack! st loc)
  (-> state? location? response?)
  (match-define (state usr _ fmap) st)
  (define enms (grmap-enemies fmap))
  (define enm-idx (find-target st loc))
  (cond
    [(not enm-idx) (err (format "No target at ~a,~a!" (car loc) (cdr loc)))]
    [(> (loc-dist (actor-loc usr) loc) (actor-stat usr 'ran))
      (err "Target out of range!")]
    [else (define dmg (+ 2 (actor-stat usr 'str)
                         (- (actor-stat (gvector-ref enms enm-idx) 'def))))
          (apply-damage! st enm-idx dmg)]))

;; teaches/applies the skill/statup at (x,y)
(define/contract (learn-skill! st x y)
  (-> state? integer? integer? (listof any/c))
  (define usr (state-user st))
  (match-define (sarray width height _) (actor-skills usr))
  (define (apply-sp! cost)
    (set-actor-sp! usr (- (actor-sp usr) cost))
    (vector-set! (get-skill st x y) 0 #t)
    (for* ([dx '(-2 -1 0 1 2)]
           [dy '(-2 -1 0 1 2)]
           #:when (and (< -1 (+ x dx) width)
                       (< -1 (+ y dy) height)
                       (< (+ (abs dx) (abs dy)) 3)))
      (vector-set! (get-skill st (+ x dx) (+ y dy)) 1 #t)))
  (cond
    [(not (can-learn? st x y)) (err "You can't learn this skill yet!")]
    [else (match (get-skill st x y)
      [#f (err "No skill at (~a,~a)!" x y)]
      [(vector #t _ _ _) (err "Already learned this skill!")]
      [(vector _ _ (? (curry < (actor-sp usr))) _) (err "You don't have enough SP!")]
      [(vector #f #t cost _)
        (apply-sp! cost)
        (list (list 'skill-gain x y))])]))

;; use a skill at target
(define/contract (use-skill! st x y loc)
  (-> state? integer? integer? location? response?)
  (match (get-skill st x y)
    [#f (err "Invalid skill")]
    [(vector #f _ _ _) (err "You have not learned that skill yet.")]
    [(vector _ _ _ (? stats?)) (err "That is not an active skill.")]
    [(vector #t _ _ sk) ((skill-effect sk) st loc)]))
