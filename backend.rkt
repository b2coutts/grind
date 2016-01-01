#lang racket
;; provides user-facing functions for manipulating the game state

(provide response? move-user! attack! learn-skill! use-skill! echo)

(require "state.rkt" "util.rkt" "combat.rkt" "ai.rkt")
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
    [(not (space-free? st (car loc) (cdr loc))) (err "There's something in the way!")]
    [else (set-actor-loc! usr loc)
          (set-actor-mvs! usr (sub1 (actor-mvs usr)))
          (append (list (list 'move loc))
                  (if (<= (actor-mvs usr) 0) (enemy-turn! st) '()))]))

;; makes the user attack a location
(define/contract (attack! st loc)
  (-> state? location? response?)
  (match-define (state usr _ fmap) st)
  (define enms (grmap-enemies fmap))
  (define enm-idx (find-target st loc))
  (cond
    [(not enm-idx) (err (format "No target at ~a,~a!" (car loc) (cdr loc)))]
    [(> (loc-dist (actor-loc usr) loc) (actor-stat usr 'ran)) (err "Target out of range!")]
    [else (append (apply-damage! st enm-idx (atk-damage usr (gvector-ref enms enm-idx)))
                  (enemy-turn! st))]))

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
    [(not (can-learn? st x y)) (err "You can't learn this skill yet.")]
    [else (match (get-skill st x y)
      [#f (err "No skill at (~a,~a)!" x y)]
      [(vector #t _ _ s) (err "You have already learned ~a." (ss-name s))]
      [(vector #f _ (? (curry < (actor-sp usr))) _) (err "You don't have enough SP!")]
      [(vector #f _ cost _)
        (apply-sp! cost)
        (list (list 'skill-gain x y))])]))

;; use a skill at target
(define/contract (use-skill! st idx loc)
  (-> state? integer? location? response?)
  (match (vector-ref (sarray-skills (actor-skills (state-user st))) idx)
    [#f (err "Invalid skill")]
    [(vector #f _ _ s) (err "You have not learned ~a yet." (ss-name s))]
    [(vector _ _ _ (? stats? s)) (err "~a is not an active skill." (ss-name s))]
    [(vector #t _ _ sk) (append ((skill-effect sk) st loc) (enemy-turn! st))]))

;; echo function for debug purposes
(define/contract (echo str)
  (-> string? response?)
  (list (list 'info str)))
