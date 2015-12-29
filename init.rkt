#lang racket
;; defines some initial state for testing

(provide game-state)
(require "state.rkt" "util.rkt" "skills.rkt" data/gvector)

;; statups
(define all1 '#hash((maxhp . 1) (str . 1) (skl . 1) (def . 1) (spd . 0) (ran . 0)))
(define all2 '#hash((maxhp . 2) (str . 2) (skl . 2) (def . 2) (spd . 0) (ran . 0)))
(define all3 '#hash((maxhp . 3) (str . 3) (skl . 3) (def . 3) (spd . 0) (ran . 0)))
(define hp5  '#hash((maxhp . 5) (str . 0) (skl . 0) (def . 0) (spd . 0) (ran . 0)))
(define ran1 '#hash((maxhp . 0) (str . 0) (skl . 0) (def . 0) (spd . 0) (ran . 1)))
(define spd1 '#hash((maxhp . 0) (str . 0) (skl . 0) (def . 0) (spd . 1) (ran . 0)))

(define (wrap s) (vector #f #f 2 s))
(define skill-table (sarray 4 4 (vector-map wrap (vector
  breeze      all1      second-aid      hp5
  first-aid   static    ran1            all2
  spd1        ran1      ember           all1
  all3        third-aid snipe           all3
))))
(for ([idx '(0 2 14)])
  (vector-set! (vector-ref (sarray-skills skill-table) idx) 0 #t))

(define usr (actor
  "miles"
  1
  2
  8
  '#hash((maxhp . 10) (str . 5) (skl . 5) (def . 5) (spd . 1) (ran . 1))
  skill-table
  (cons 20 20)
))

(define slime (actor
  "slime"
  2
  2
  8
  '#hash((maxhp . 8) (str . 2) (skl . 0) (def . 1) (spd . 1) (ran . 1))
  (sarray 0 0 (vector))
  (cons 40 4)
))

(define rock-golem (actor
  "rock golem"
  3
  3
  22
  '#hash((maxhp . 25) (str . 1) (skl . 0) (def . 4) (spd . 1) (ran . 1))
  (sarray 0 0 (vector))
  (cons 4 1)
))

(define btlmap (read-map "test.map"))
(set-grmap-enemies! btlmap (gvector slime rock-golem))

(define game-state (state usr 'battle btlmap))
