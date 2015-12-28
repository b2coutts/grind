#lang racket
;; miscellaneous helper functions

(provide err loc-dist grmap-ref fmt find-target get-skill actor-stat can-learn? liv-enms read-map)

(require "state.rkt")
(require data/gvector)

;; produces a simple error response
(define/contract (err str)
  (-> string? (listof any/c))
  (list (list 'err str)))

;; produces a string displaying an object with given escape codes, then changes to white-on-black
(define/contract (fmt datum . codes)
  (->* (any/c) #:rest (listof integer?) string?)
  (define esc-str (cond
    [(empty? codes) ""]
    [else (format "\x1b[~am" (string-join (map ~a codes) ";"))]))
  (string-append esc-str (format "~a\x1b[0;97m" datum)))

;; manhattan distance between two locations
(define/contract (loc-dist a b)
  (-> location? location? integer?)
  (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b)))))

;; get cell at location (x,y)
(define/contract (grmap-ref fmap x y)
  (-> grmap? integer? integer? cell?)
  (vector-ref (grmap-cells fmap) (+ (* (grmap-width fmap) y) x)))

;; finds a target at given location, or #f if none exists
(define/contract (find-target st loc)
  (-> state? location? (or/c #f integer?))
  (define enms (grmap-enemies (state-fmap st)))
  (for/first ([idx (gvector-count enms)]
              #:when (and (gvector-ref enms idx)
                          (equal? (actor-loc (gvector-ref enms idx)) loc)))
    idx))

;; produces the skill/statup at (x,y)
(define/contract (get-skill st x y)
  (-> state? integer? integer?
      (or/c #f (vector/c boolean? boolean? integer? (or/c skill? stats?))))
  (match-define (sarray width height skills) (actor-skills (state-user st)))
  (cond
    [(not (and (< -1 x width) (< -1 y height))) #f]
    [else (vector-ref skills (+ (* y width) x))]))

;; produces the given stat of the actor
(define/contract (actor-stat actr stat)
  (-> actor? symbol? integer?)
  (hash-ref (actor-stats actr) stat))

;; determines whether a skill can be learned (i.e., is adjacent to a learned skill)
(define/contract (can-learn? st x y)
  (-> state? integer? integer? any/c)
  (or (and (= x 0) (= y 0))
      (for*/or ([dx '(-1 0 1)]
                [dy '(-1 0 1)]
                #:when (= (+ (abs dx) (abs dy)) 1))
        (match (get-skill st (+ x dx) (+ y dy))
          [(vector #t _ _ _) #t]
          [_ #f]))))

;; produces a list of all living enemies
(define/contract (liv-enms st)
  (-> state? (listof actor?))
  (for/list ([enm (grmap-enemies (state-fmap st))]
             #:when (> (actor-hp enm) 0))
    enm))

;; reads a map from a file
(define/contract (read-map file)
  (-> path-string? grmap?)
  (with-input-from-file file (thunk
    (define width (string->number (read-line)))
    (define height (string->number (read-line)))
    (define cells
      (for/vector ([ch (current-input-port)]
                   #:unless (char=? (integer->char ch) #\newline))
        (match (integer->char ch)
          [#\X        'wall]
          [#\space    'floor]
          [c (error (format "read-map: unexpected character: ~s" c))])))
    (grmap width height cells (gvector)))))
