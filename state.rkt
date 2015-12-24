#lang racket

(provide
  cell?
  location?
  (struct-out actor)
  (struct-out grmap)
  (struct-out state)
)

(require data/gvector)

;; a single cell of a battle map
(define cell? (or/c 'floor 'wall))

;; a location in a battle map
(define location? (cons/c integer? integer?))

;; contains the state of a single character (i.e., the user or an enemy)
(define-struct/contract actor (
  [name           string?]
  [lvl            integer?]
  [hp             integer?]
  [maxhp          integer?]
  [str            integer?]
  [def            integer?]
  [spd            integer?]
  [range          integer?] ;; TODO: probably want this skill-based
  [loc            (or/c location? #f)]
) #:mutable #:transparent)

;; contains battle state (aside from actor)
(define-struct/contract grmap (
  [width          integer?]
  [height         integer?]
  [cells          (vectorof cell?)]
  [enemies        gvector?] ;; gvector of actor?
) #:mutable #:transparent)

;; contains an entire game state
(define-struct/contract state (
  [user           actor?]
  [context        (or/c 'battle 'overworld)]
  [fmap           (or/c grmap? #f)]
) #:mutable #:transparent)
