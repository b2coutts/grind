#lang racket
;; rudimentary frontend functionality

(provide display-map display-skills display-skill-info)

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


;; produces the appropriate terminal escape codes for a given skill
(define/contract (skill-ansi-codes st x y)
  (-> state? integer? integer? (listof integer?))
  (match-define (vector learned visible cost s) (get-skill st x y))

  (define base-color (cond
    [(not visible) 90]
    [(skill? s) (skill->color s)]
    [(stats? s) 35]))

  (define bold (if learned '(1) '()))

  (define intensity (if (can-learn? st x y) 60 0))

  (append (list (+ base-color intensity)) bold))

;; displays the skill table
(define/contract (display-skills st)
  (-> state? void?)
  (match-define (sarray width height skills) (actor-skills (state-user st)))
  (for ([row height])
    (for ([col width])
      (define sk (get-skill st col row))
      (define skill-char (if (vector-ref sk 1) (vector-ref sk 2) #\?))
      (display (apply (curry fmt skill-char) (skill-ansi-codes st col row))))
    (newline)))

;; displays a line of n dashes
(define (dash n [char #\-])
  (-> integer? void?)
  (for ([i n])
    (display char))
  (newline))

;; prints info about a specific skill
(define/contract (display-skill-info st x y)
  (-> state? integer? integer? void?)
  (dash 80 #\=)
  (match (get-skill st x y)
    [(vector _ #f _ _)
      (printf "?????\n")
      (dash 5)
      (printf "You cannot see this skill yet.\n")]
    [(vector learned #t cost s)
      (define name (apply (curry fmt (if (skill? s) (skill-name s) "Stats"))
                          (skill-ansi-codes st x y)))
      (printf "~a~a (cost: ~a SP)\n" name (if learned " (LEARNED)" "") cost)
      (dash 80)
      (match s
        [(skill _ 'damage pwr ran _)
          (printf "Range: ~a. Deals (~a+skl) damage to target.\n" ran pwr)]
        [(skill _ 'heal pwr _ _)
          (printf "Range: N/A. Heals self for (~a+skl) HP.\n" pwr)]
        [(? stats?)
          (display (string-join (for/list ([(stat val) s] #:when (> val 0))
                                  (format "~a+~a" stat val))
                                ", "
                                #:before-first "Increases "
                                #:after-last ".\n"))])])
  (dash 80 #\=))
