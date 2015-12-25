#lang racket
;; rudimentary frontend functionality

(provide display-map display-skills)

(require "state.rkt" "util.rkt" "backend.rkt")
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

;; gets the color of a skill type
(define/contract (skill->color sk)
  (-> skill? integer?)
  (match (skill-type sk)
    ['damage        31] ;; red
    ['heal          32] ;; green
  ))


;; formats a skill nicely for printing; x,y MUST be a valid skill index
(define/contract (format-skill st x y)
  (-> state? integer? integer? string?)
  (match-define (vector learned visible cost s) (get-skill st x y))

  (define base-color (cond
    [(not visible) 90]
    [(skill? s) (skill->color s)]
    [(stats? s) 35]))

  (define bold (if learned '(1) '()))

  (define intensity (if (can-learn? st x y) 60 0))

  (apply (curry fmt (if visible cost #\?))
         (append (list (+ base-color intensity)) bold)))

;; displays the skill table
(define/contract (display-skills st)
  (-> state? void?)
  (match-define (sarray width height skills) (actor-skills (state-user st)))
  (for ([row height])
    (for ([col width])
      (display (format-skill st col row)))
    (newline)))
