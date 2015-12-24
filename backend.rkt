#lang racket
;; provides user-facing functions for manipulating the game state

(provide move-user! attack!)

(require "state.rkt" "util.rkt" "combat.rkt")
(require data/gvector)

;; a response to the frontend's move request
(define response? string?)

;; moves the user to the given location
;; TODO: should trigger AI
(define/contract (move-user! st loc)
  (-> state? location? response?)
  (match-define (state usr _ fmap) st)
  (cond
    [(> (loc-dist (actor-loc usr) loc) (actor-spd usr)) "Cannot move that far!"]
    [else (set-actor-loc! usr loc)
          (format "Moved to ~a,~a" (car loc) (cdr loc))]))

;; makes the user attack a location
(define/contract (attack! st loc)
  (-> state? location? response?)
  (match-define (state usr _ fmap) st)
  (define enms (grmap-enemies fmap))
  (define enm-idx (for/first ([idx (gvector-count enms)]
                              #:when (equal? (actor-loc (gvector-ref enms idx)) loc))
                    idx))
  (cond
    [(not enm-idx) (format "No target at ~s,~s!" (car loc) (cdr loc))]
    [(> (loc-dist (actor-loc usr) loc) (actor-range usr))
      "Target out of range!"]
    [else (define enm (gvector-ref enms enm-idx))
          (define dmg (apply-attack! usr enm))
          (cond
            [(<= (actor-hp enm) 0)
              (gvector-remove! (grmap-enemies fmap) enm-idx)
              (format "Dealt ~a damage to ~a; ~a died" dmg (actor-name enm) (actor-name enm))]
            [else
              (format "Dealt ~a damage to ~a" dmg (actor-name enm))])]))
