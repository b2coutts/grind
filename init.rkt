#lang racket
;; defines some initial state for testing

(provide game-state)
(require "state.rkt" data/gvector)

(define usr (actor
  "miles"
  1
  10
  10
  5
  5
  1
  1
  (cons 1 1)
))

(define enemy1 (actor
  "slime"
  1
  8
  8
  2
  1
  1
  1
  (cons 1 3)
))

(define enemy2 (actor
  "rock golem"
  1
  12
  12
  1
  4
  1
  1
  (cons 3 3)
))

;; 5x5 grid with the following layout
;; X X X X X
;; X E _ E X
;; X _ _ _ X
;; X U _ _ X
;; X X X X X
(define cells (vector
  'wall  'wall  'wall  'wall  'wall
  'wall  'floor 'floor 'floor 'wall
  'wall  'floor 'floor 'floor 'wall
  'wall  'floor 'floor 'floor 'wall
  'wall  'wall  'wall  'wall  'wall
))

(define btlmap (grmap 5 5 cells (gvector enemy1 enemy2)))

(define game-state (state usr 'battle btlmap))
