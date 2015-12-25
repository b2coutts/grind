#lang racket
;; rudimentary frontend functionality

(provide display-map)

(require "state.rkt" "util.rkt")
(require data/gvector)

;; produces a string (conventionally, 1 char and maybe escape codes) representing a cell
(define/contract (cell->string cell)
  (-> cell? string?)
  (match cell
    ['floor     (fmt #\space 40)]
    ['wall      (fmt #\X 97)]))

;; displays the game map
(define/contract (display-map st)
  (-> state? void?)
  (match-define (state usr 'battle (and gm (grmap width height cells enemies))) st)
  (define glyphs (for/hash ([ent (cons usr (gvector->list enemies))])
    (values (actor-loc ent) (actor-glyph ent))))
  (for ([row height])
    (for ([col width])
      (display (hash-ref glyphs (cons col row) (thunk (cell->string (grmap-ref gm col row))))))
    (newline)))
