#lang racket

(provide
  cell?
  location?
  stats?
  (struct-out skill)
  (struct-out sarray)
  (struct-out actor)
  (struct-out grmap)
  (struct-out state)
)

(require data/gvector)

;; a single cell of a battle map
(define cell? (or/c 'floor 'wall))

;; a location in a battle map
(define location? (cons/c integer? integer?))

;; contains a skill
(define-struct/contract skill (
  [name           string?]
  [type           (or/c 'damage 'heal)]
  [ran            integer?]
  [effect         (-> any/c location? any/c)] ;; first arg is state?
) #:mutable #:transparent)

;; contains actor stats
(define (stats? x)
  (and (hash? x) (equal? (sort (hash-keys x) symbol<?)
                         (sort '(maxhp str skl def spd ran) symbol<?))))

;; contains a skill array
(define-struct/contract sarray (
  [width          integer?]
  [height         integer?]
  ;; (vector learned? visible? cost skill)
  [skills         (vectorof (vector/c boolean? boolean? integer? (or/c skill? stats?)))]
) #:mutable #:transparent)

;; contains the state of a single character (i.e., the user or an enemy)
(define-struct/contract actor (
  [name           string?]
  [lvl            integer?]
  [sp             integer?] ;; skill points
  [hp             integer?]
  [stats          stats?]
  [skills         sarray?]
  [glyph          string?]
  [loc            (or/c location? #f)]
) #:mutable #:transparent)

;; contains battle state (aside from actor)
(define-struct/contract grmap (
  [width          integer?]
  [height         integer?]
  [cells          (vectorof cell?)] ;; size width*height, row-major
  [enemies        gvector?] ;; gvector of actor?
) #:mutable #:transparent)

;; contains an entire game state
(define-struct/contract state (
  [user           actor?]
  [context        (or/c 'battle 'overworld)]
  [fmap           (or/c grmap? #f)]
) #:mutable #:transparent)
