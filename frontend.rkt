#lang racket
;; rudimentary frontend functionality

(provide display-map display-skills display-skill-info)

(require "init.rkt") ;; TODO: this shouldn't be needed
(require "state.rkt" "util.rkt" "backend.rkt")
(require (planet neil/charterm) data/gvector)

;; various UI constants
(define map-width 40)
(define map-height 40)
(define hud-width 40)

(open-charterm)
(charterm-clear-screen)

;; gray bar
(define bar (fmt #\| 90))

;; produces a string (conventionally, 1 char and maybe escape codes) representing a cell
(define/contract (cell->string cell)
  (-> cell? string?)
  (match cell
    ['floor     (fmt #\space 40)]
    ['wall      (fmt #\X 97)]))

;; creates a bar of length len with the form +-----+
(define/contract (make-bar len [mid #\-] [end #\+])
  (->* (integer?) (char? char?) string?)
  (fmt (string-append (~a end) (make-string (- len 2) mid) (~a end)) 90))

;; creates a health bar
(define/contract (make-health-bar hp maxhp [width 10])
  (->* (integer? integer?) (integer?) string?)
  (define pct (/ hp maxhp))
  (define slashes (ceiling (* pct width)))
  (define color (cond
    [(< pct 0.35) 31] ;; red
    [(< pct 0.65) 33] ;; yellow
    [else         32])) ;; green
  (string-append
    (fmt "[" 37)
    (fmt (make-string slashes #\/) color)
    (fmt (make-string (- width slashes) #\-) 37)
    (fmt "]" 37)))

;; displays the game map
(define/contract (display-map st)
  (-> state? void?)
  (match-define (state usr 'battle (and gm (grmap width height cells enemies))) st)
  (define glyphs (for/hash ([ent (cons usr (gvector->list enemies))])
    (values (actor-loc ent) (actor-glyph ent))))
  (charterm-cursor 1 1)
  (charterm-display (make-bar map-width))
  (for ([row (- map-height 2)])
    (charterm-cursor 1 (+ row 2))
    (charterm-display bar)
    (for ([col (- map-width 2)])
      (define x (+ col (car (actor-loc usr)) (quotient map-width -2)))
      (define y (+ row (cdr (actor-loc usr)) (quotient map-height -2)))
      (charterm-display (cond
        [(not (and (< -1 x width) (< -1 y height))) #\space]
        [else (hash-ref glyphs (cons x y) (thunk (cell->string (grmap-ref gm x y))))])))
    (charterm-display bar))
  (charterm-cursor 1 map-height)
  (charterm-display (make-bar map-width)))

;; displays user info
(define/contract (display-user st)
  (-> state? void?)
  (match-define (and usr (actor name lvl _ hp _ _ _ _)) (state-user st))
  (charterm-cursor map-width 1)
    (charterm-display (make-bar hud-width))

  (charterm-cursor map-width 2)
    (charterm-display (format "~a ~a (~a/~a)" bar (fmt name 32) hp (actor-stat usr 'maxhp)))
  (charterm-cursor (+ map-width hud-width -14) 2)
    (charterm-display (make-health-bar hp (actor-stat usr 'maxhp) 10))
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width 3)
    (charterm-display (format "~a LV ~a (MV ~a/~a)" bar lvl #\? (actor-stat usr 'spd)))
  (define stat-str (string-join (for/list ([stat '(str skl def spd ran)])
                                  (~a (actor-stat usr stat)))
                                "/"))
  (charterm-cursor (+ map-width hud-width (- (string-length stat-str)) -2) 3)
    (charterm-display stat-str)
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width 4)
    (charterm-display (make-bar hud-width #\=)))

;; display actor info at the given slot (slots are 0-indexed)
(define/contract (display-actor actr slot)
  (-> actor? integer? void?)
  (match-define (actor name lvl _ hp _ _ _ _) actr)
  (define y1 (+ 5 (* slot 3)))

  (charterm-cursor map-width y1)
    (charterm-display bar)
    (charterm-display (format " ~a: ~a (~a/~a)" #\? (fmt name 31) hp (actor-stat actr 'maxhp)))
  (charterm-cursor (+ map-width hud-width -14) y1)
    (charterm-display (make-health-bar hp (actor-stat actr 'maxhp) 10))
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width (+ y1 1))
    (charterm-display (format "~a    LV ~a" bar lvl))
  (define stat-str (string-join (for/list ([stat '(str skl def spd ran)])
                                  (~a (actor-stat actr stat)))
                                "/"))
  (charterm-cursor (+ map-width hud-width (- (string-length stat-str)) -2) (+ y1 1))
    (charterm-display stat-str)
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width (+ y1 2))
    (charterm-display (make-bar hud-width)))

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

(printf "here\n")
(display-map game-state)
(display-user game-state)
(display-actor (gvector-ref (grmap-enemies (state-fmap game-state)) 0) 0)
(display-actor (gvector-ref (grmap-enemies (state-fmap game-state)) 1) 1)
(charterm-read-key)
