#lang racket
;; rudimentary frontend functionality

(require "init.rkt") ;; TODO: this shouldn't be needed
(require "state.rkt" "util.rkt" "backend.rkt" "bindings.rkt")
(require (planet neil/charterm) data/gvector data/queue)

#| Profiler block 1
(require profile profile/render-text)
(profile-thunk (thunk
|#

;; various UI constants
(define map-width 40)
(define map-height 40)
(define hud-width 40)

;; location of skill select window
;; TODO: should fix width/height, or let it adjust dynamically?
(define skill-x0 1)
(define skill-y0 1)

;; location of skill table window
(define stable-x0 1)
(define stable-y0 1)

;; some global state
(define console-lines (make-queue))
  (for ([i map-height]) (enqueue! console-lines ""))
(define active-enemies '())
(define frontend-state 'map)
(define cursor-vis #f)

(open-charterm)
(charterm-clear-screen)

;; gray bar
(define bar (fmt #\| 90))

;; ------------------various string building helper functions------------------
;; converts an enemy index to a letter
(define/contract (idx->glyph idx)
  (-> integer? string?)
  (fmt (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" (modulo idx 52)) 31))

;; wraps a string to lines of at most N characters each
;; TODO: handle terminal codes nicely
(define/contract (wrap N str)
  (-> integer? string? (listof string?))
  (cond
    [(<= (string-length str) N) (list str)]
    [else (define idx (match (for/last ([ch (substring str 0 N)]
                                        [idx N]
                                        #:when (equal? ch #\space))
                              idx)
                        [#f N]
                        [idx idx]))
          (cons (substring str 0 idx) (wrap N (substring str (+ idx 1))))]))

;; creates a bar of length len with the form +-----+
(define/contract (make-bar len [mid #\-] [end #\+])
  (->* (integer?) (char? char?) string?)
  (fmt (string-append (~a end) (make-string (- len 2) mid) (~a end)) 90))

;; creates a health bar
(define/contract (make-health-bar hp maxhp [width 10])
  (->* (integer? integer?) (integer?) string?)
  (define pct (/ hp maxhp))
  (define slashes (min width (ceiling (* pct width))))
  (define color (cond
    [(< pct 0.35) 31] ;; red
    [(< pct 0.65) 33] ;; yellow
    [else         32])) ;; green
  (string-append
    (fmt "[" 37)
    (fmt (make-string slashes #\/) color)
    (fmt (make-string (- width slashes) #\-) 37)
    (fmt "]" 37)))

;; produces a string (conventionally, 1 char and maybe escape codes) representing a cell
(define/contract (cell->string cell)
  (-> cell? string?)
  (match cell
    ['floor     (fmt #\space 40)]
    ['wall      (fmt #\X 97)]))

;; convert terminal coordinates to map coordinates
(define/contract (term->map st col row)
  (-> state? integer? integer? location?)
  (cons (+ col (car (actor-loc (state-user st))) (quotient map-width -2) -2)
        (+ row (cdr (actor-loc (state-user st))) (quotient map-height -2) -2)))

;; convert map coordinates to terminal coordinates
(define/contract (map->term st loc)
  (-> state? location? (values integer? integer?))
  (values (- (car loc) (car (actor-loc (state-user st))) (quotient map-width -2) -2)
          (- (cdr loc) (cdr (actor-loc (state-user st))) (quotient map-height -2) -2)))

;; TODO: add comments to group the rest of the functions

(define/contract (update-active-enemies! st)
  (-> state? void?)
  (define uloc (actor-loc (state-user st)))
  (set! active-enemies
    (for/list ([enm (grmap-enemies (state-fmap st))]
               [idx (in-naturals)]
               #:when (let-values ([(x y) (if enm (map->term st (actor-loc enm)) (values 0 0))])
                        (and (< 1 x map-width) (< 1 y map-height))))
      idx)))

;; set cursor visibility
;; TODO: this and other things should really use tput
(define/contract (set-cursor-vis! vis)
  (-> boolean? void?)
  (set! cursor-vis vis)
  (printf "\x1b\x5b\x3f\x32\x35~a" (if cursor-vis "\x68" "\x6c"))
  (flush-output))

;; displays user info
(define/contract (display-user st)
  (-> state? void?)
  (match-define (and usr (actor name lvl sp hp _ _ _)) (state-user st))
  (charterm-cursor map-width 1)
    (charterm-display (make-bar hud-width))

  (charterm-cursor map-width 2)
    (charterm-display (format "~a ~a (~a/~a)" bar (fmt name 32) hp (actor-stat usr 'maxhp)))
  (charterm-cursor (+ map-width hud-width -14) 2)
    (charterm-display (make-health-bar hp (actor-stat usr 'maxhp) 10))
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width 3)
    (charterm-display (format "~a SP ~a (MV ~a/~a)" bar sp #\? (actor-stat usr 'spd)))
  (define stat-str (string-join (for/list ([stat '(str skl def spd ran)])
                                  (~a (actor-stat usr stat)))
                                "/"))
  (charterm-cursor (+ map-width hud-width (- (string-length stat-str)) -2) 3)
    (charterm-display stat-str)
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width 4)
    (charterm-display (make-bar hud-width #\=)))

;; display enemy info at the given slot (slots are 0-indexed)
(define/contract (display-actor st enm-idx slot)
  (-> state? integer? integer? void?)
  (define enm (gvector-ref (grmap-enemies (state-fmap st)) enm-idx))
  (match-define (actor name lvl _ hp _ _ _) enm)
  (define y1 (+ 5 (* slot 3)))

  (charterm-cursor map-width y1)
    (charterm-display bar)
    (charterm-display (format " ~a: ~a (~a/~a)"
                              (idx->glyph enm-idx) (fmt name 31) hp (actor-stat enm 'maxhp)))
  (charterm-cursor (+ map-width hud-width -14) y1)
    (charterm-display (make-health-bar hp (actor-stat enm 'maxhp) 10))
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width (+ y1 1))
    (charterm-display (format "~a    LV ~a" bar lvl))
  (define stat-str (string-join (for/list ([stat '(str skl def spd ran)])
                                  (~a (actor-stat enm stat)))
                                "/"))
  (charterm-cursor (+ map-width hud-width (- (string-length stat-str)) -2) (+ y1 1))
    (charterm-display stat-str)
    (charterm-display (fmt " |" 90))

  (charterm-cursor map-width (+ y1 2))
    (charterm-display (make-bar hud-width)))

;; displays all active enemies
(define/contract (display-active-enemies st)
  (-> state? void?)
  (for ([enm-idx active-enemies]
        [slot-idx (in-naturals)])
    (display-actor st enm-idx slot-idx)))

;; refreshes the console display
(define/contract (display-console)
  (-> void?)
  ;; TODO: deal with having too many enemies on screen
  (define top (+ 4 (* 3 (length active-enemies))))
  (for ([line (reverse (queue->list console-lines))]
        [row (in-range (- map-height 1) top -1)])
    (charterm-cursor (+ map-width 1) row)
    (charterm-display (fmt (~a line #:min-width 38 #:align 'left) 37))
    (charterm-display bar))
  (charterm-cursor map-width map-height)
  (charterm-display (make-bar hud-width)))

;; displays the game map
(define/contract (display-map st)
  (-> state? void?)
  (match-define (state usr 'battle (and gm (grmap width height cells _))) st)
  (define glyphs (hash-set (for/hash ([ent (grmap-enemies (state-fmap st))]
                                      [idx (in-naturals)])
                              (values (actor-loc ent) (idx->glyph idx)))
                           (actor-loc usr) (fmt "@" 32)))
  (charterm-cursor 1 1)
  (charterm-display (make-bar map-width))
  (for ([row (- map-height 2)])
    (match-define (cons x0 y) (term->map st 2 (+ row 2)))
    (charterm-cursor 1 (+ row 2))
    (charterm-display (apply string-append `(
      ,bar
      ,@(for/list ([col (- map-width 2)])
          (define x (+ x0 col))
          (cond
            [(not (and (< -1 x width) (< -1 y height))) " "]
            [else (hash-ref glyphs
            (cons x y)
            (thunk
            (cell->string
            (grmap-ref gm x y))))]))
      ,bar))))
  (charterm-cursor 1 map-height)
  (charterm-display (make-bar map-width)))

;; gets the color of a skill type
(define/contract (skill->color sk)
  (-> skill? integer?)
  (match (skill-type sk)
    ['damage        31] ;; red
    ['heal          32] ;; green
  ))


;; printf for console
(define/contract (consolef lbl fstr . args)
  (->* (string? string?) #:rest (listof any/c) void?)
  (define lines (wrap (- hud-width 3 (string-length lbl)) (apply (curry format fstr) args)))
  (enqueue! console-lines (format " ~a ~a " lbl (first lines)))
  (for ([line (rest lines)])
    (enqueue! console-lines (format " ~a ~a " (make-string (string-length lbl) #\space) line)))
  (for ([i (max 0 (- (queue-length console-lines) map-height))])
    (dequeue! console-lines))
  (display-console))

;; displays a menu of usable skills to choose from
;; TODO: should include attack?
(define/contract (display-skill-select st idcs active)
  (-> state? (listof integer?) integer? void?)
  (define sks (vector-map (curryr vector-ref 3) (sarray-skills (actor-skills (state-user st)))))
  (define width (apply max (map (lambda (i) (string-length (skill-name (vector-ref sks i)))) idcs)))
  (define height (+ 2 (length idcs)))
  (charterm-cursor skill-x0 skill-y0)
  (charterm-display (make-bar (+ width 2)))
  (for ([i idcs]
        [pos (in-naturals)])
    (define style (if (equal? pos active) 44 0))
    (charterm-cursor skill-x0 (+ skill-y0 pos 1))
    (charterm-display (string-append
      bar
      (fmt (~a (skill-name (vector-ref sks i)) #:min-width width #:align 'left) style)
      bar)))
  (charterm-cursor skill-x0 (+ skill-y0 height -1))
  (charterm-display (make-bar (+ width 2))))

;; produces the appropriate terminal escape codes for a given skill
(define/contract (skill-ansi-codes st x y)
  (-> state? integer? integer? (listof integer?))
  (match-define (vector learned visible cost s) (get-skill st x y))

  (define base-color (cond
    [(not visible) 97]
    [(skill? s) (skill->color s)]
    [(stats? s) 35]))

  (define bold (if learned '(1) '()))

  (define intensity (if (can-learn? st x y) 60 0))

  (append (list (+ base-color intensity)) bold))

;; displays the skill table
(define/contract (display-stable st)
  (-> state? void?)
  (match-define (sarray width height skills) (actor-skills (state-user st)))
  (charterm-cursor stable-x0 stable-y0)
  (charterm-display (make-bar (+ width 2)))
  (for ([row height])
    (charterm-cursor stable-x0 (+ 1 stable-y0 row))
    (charterm-display (string-append
      bar
      (apply string-append (for/list ([col height])
        (define sk (get-skill st col row))
        (define skill-char (if (vector-ref sk 1) (vector-ref sk 2) #\?))
        (apply (curry fmt skill-char) (skill-ansi-codes st col row))))
      bar)))
  (charterm-cursor stable-x0 (+ 1 stable-y0 height))
  (charterm-display (make-bar (+ width 2))))

#|
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
|#

;; redisplays everything
(define/contract (display-all st)
  (-> state? void?)
  (match frontend-state
    [(list 'skill idx) (display-skill-select st (known-skills st) idx)]
    [(list 'target _ _ _) (void)]
    [(list 'skill-table _ _) (display-stable st)
                             (display-console)]
    [_ (update-active-enemies! st)
       (display-map st)
       (display-user st)
       (display-active-enemies st)
       (display-console)]))

;; handles responses from the backend
;; TODO: should maybe make it only refresh what needs to be refreshed
(define/contract (handle-response st resp)
  (-> state? response? void?)
  (define old-cursor-vis cursor-vis)
  (set-cursor-vis! #f)
  (define-values (old-x old-y) (match frontend-state
    [(list 'target x y _) (values x y)]
    [(list 'skill-table x y) (values (+ 1 x skill-x0) (+ 1 y skill-y0))]
    [_ (values 1 1)]))
  (for ([msg resp])
    (match msg
      [(list 'err str) (consolef "!" str)]
      [(list 'info str) (consolef "*" str)]
      [(list 'move loc) (void)]
      [(list 'skill-gain x y)
        (match (vector-ref (get-skill st x y) 3)
          [(skill name _ _ _ _) (consolef "*" "You learned ~a."
                                          (apply (curry fmt name) (skill-ansi-codes st x y)))]
          [(? stats? s) (consolef "*" (string-join (for/list ([(stat val) s] #:when (> val 0))
                                                (format "~a+~a" stat val))
                                               ", "
                                               #:before-first "You gained "
                                               #:after-last "."))])
        (set-cursor-vis! #f)
        (set! frontend-state 'map)]
      [(list 'heal amt target) (consolef "*" "~a heals ~a HP." (actor-name target) amt)]
      [(list 'damage dmg target) (consolef "*" "~a takes ~a damage." (actor-name target) dmg)]
      [(list 'death target) (consolef "*" "~a died." (actor-name target))]
      [(list 'sp amt) (consolef "*" "You gained ~a SP." amt)]
      [_ (error (format "Unexpected msg from backend: ~s" msg))]))
  (display-all st)
  (charterm-cursor old-x old-y)
  (set-cursor-vis! old-cursor-vis)
  (void))


;; initialize UI
(define st game-state)
(set-cursor-vis! #f)
(display-all st)

;; changes frontend state to target (starting at user, w/ skill sk)
(define/contract (frontend-target! st sk)
  (-> state? (or/c 'attack integer?) void?)
  (define-values (col row) (map->term st (actor-loc (state-user st))))
  (set! frontend-state (list 'target col row sk))
  (charterm-cursor col row)
  (set-cursor-vis! #t))

(define (loop)
  ;; note: in asciitan I discovered a charterm bug where this would not sync despite input being
  ;; ready (would lag 1 behind user input); may want to workaround this if it comes up
  (match-define (cons x y) (actor-loc (state-user st)))
  (define usr (state-user st))
  (define skill-vec (sarray-skills (actor-skills usr)))
  (define evt (sync (current-charterm)))
  (define input (hash-ref bindings (charterm-read-key) (thunk #f)))
  (when (equal? input 'quit)
    (close-charterm)
    (set-cursor-vis! #t)
    (error "exit"))
  (handle-response st (match (state-context st)
    ['battle (match frontend-state
      ['map (match input
        ['left   (move-user! st (cons (sub1 x) y))]
        ['down   (move-user! st (cons x (add1 y)))]
        ['up     (move-user! st (cons x (sub1 y)))]
        ['right  (move-user! st (cons (add1 x) y))]
        ['select (frontend-target! st 'attack) '()]
        ['back   (set! frontend-state (list 'skill 0)) '()]
        ['menu   (set! frontend-state (list 'skill-table 0 0))
                 (set-cursor-vis! #t)
                 '()]
        [#f      '()])]
      [(list 'skill idx) (match input
        ['up     (set! frontend-state (list 'skill (max 0 (sub1 idx)))) '()]
        ['down   (set! frontend-state (list 'skill
                    (min (sub1 (length (known-skills st))) (add1 idx)))) '()]
        ['select (match (vector-ref skill-vec (list-ref (known-skills st) idx))
          [(vector #t _ _ (skill _ 'damage _ _ _)) 
            (display-map st)
            (frontend-target! st (list-ref (known-skills st) idx)) '()]
          [(vector #t _ _ (skill _ 'heal _ _ _))
            (set! frontend-state 'map)
            (use-skill! st (list-ref (known-skills st) idx) (cons x y))])]
        ['back   (set! frontend-state 'map) '()]
        [_ '()])]
      [(list 'skill-table sx sy) (define resp (match input
        ['left   (set! sx (max 0 (sub1 sx)))]
        ['right  (set! sx (min (sub1 (sarray-width (actor-skills usr))) (add1 sx)))]
        ['up     (set! sy (max 0 (sub1 sy)))]
        ['down   (set! sy (min (sub1 (sarray-height (actor-skills usr))) (add1 sy)))]
        ['back   (set! frontend-state 'map)
                 (set-cursor-vis! #f)
                 '()]
        ['select (learn-skill! st sx sy)]
        [_ '()]))
       (cond
        [(not (member input '(left down up right))) resp]
        [else (set! frontend-state (list 'skill-table sx sy))
              (charterm-cursor (+ 1 sx stable-x0) (+ 1 sy stable-y0))
              '()])]
      [(list 'target tx ty sk) (define resp (match input
        ['left   (set! tx (max 2 (sub1 tx)))]
        ['down   (set! ty (min (- map-height 1) (add1 ty)))]
        ['up     (set! ty (max 2 (sub1 ty)))]
        ['right  (set! tx (min (- map-width 1) (add1 tx)))]
        ['select (set! frontend-state 'map)
                 (set-cursor-vis! #f)
                 (match sk
                  ['attack    (attack! st (term->map st tx ty))]
                  [skill-idx  (use-skill! st skill-idx (term->map st tx ty))])]
        ['back   (set! frontend-state 'map)
                 (set-cursor-vis! #f)
                 '()]
        [_ '()]))
       (cond
        [(not (member input '(left down up right))) resp]
        [else (set! frontend-state (list 'target tx ty sk))
              (charterm-cursor tx ty)
              '()])])]))
  (loop))
(loop)

;; Profiler parens
;; ))
