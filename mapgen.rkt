#lang racket
;; improvement ideas:
;;   instead of square/diamond/circle, create p-norm balls with random p value
;;   apply random linear transformations to the rooms
;;   in make-path!, sometimes move in a wrong direction

(provide random-map)

(require
  "util.rkt"
  "state.rkt"

  (planet williams/science/random-distributions/gaussian)
  rnrs/sorting-6
  data/union-find
  data/gvector
)

(define (map-ref rm x y)
  (vector-ref (grmap-cells rm) (+ (* y (grmap-width rm)) x)))

(define (map-set! rm x y val)
  (match-define (grmap width height cells _) rm)
    (vector-set! cells (+ (* y width) x) val))

(define/contract (display-map rm)
  (-> grmap? void?)
  (match-define (grmap width height cells _) rm)
  (for ([y height])
    (for ([x width])
      (display (match (map-ref rm x y)
        ['floor #\space]
        ['wall  #\#])))
    (newline)))

(define (empty-map width height)
  (grmap width height (make-vector (* width height) 'floor) (gvector)))

(define/contract (full-map width height)
  (-> integer? integer? grmap?)
  (grmap width height (make-vector (* width height) 'wall) (gvector)))

(define (set-rect! rm x y w h fn)
  (match-define (grmap width height cells _) rm)
  (for* ([dx w] [dy h]
         #:when (and (< -1 (+ x dx) width) (< -1 (+ y dy) height)))
    (map-set! rm (+ x dx) (+ y dy)
      (fn (map-ref rm (+ x dx) (+ y dy))))))

(define (set-diamond! rm x y rad fn)
  (match-define (grmap width height cells _) rm)
  (for* ([dx (in-range (- rad) (+ rad 1))]
         [dy (in-range (- rad) (+ rad 1))]
         #:when (and (<= (+ (abs dx) (abs dy)) rad)
                     (< -1 (+ x dx) width)
                     (< -1 (+ y dy) height)))
    (map-set! rm (+ x dx) (+ y dy)
      (fn (map-ref rm (+ x dx) (+ y dy))))))

(define (set-circle! rm x y rad fn)
  (match-define (grmap width height cells _) rm)
  (for* ([dx (in-range (- rad) (+ rad 1))]
         [dy (in-range (- rad) (+ rad 1))]
         #:when (and (<= (sqrt (+ (* dx dx) (* dy dy))) rad)
                     (< -1 (+ x dx) width)
                     (< -1 (+ y dy) height)))
    (map-set! rm (+ x dx) (+ y dy)
      (fn (map-ref rm (+ x dx) (+ y dy))))))

;; weights for rect, diamond, circle
(define-values (wrect wdiam wcirc) (values 1.0 3.0 2.0))
(define wsum (+ wrect wdiam wcirc))

;; applies a random-ish modification to a map; returns a position in the modification
(define (randomize! rm mu sigma fn)
  (match-define (grmap width height cells _) rm)
  (define num (random))
  (match-define (cons x y) (cons (random width) (random height)))
  (define (r) (inexact->exact (abs (round (random-gaussian mu sigma)))))
  (cond
    [(< num (/ wcirc wsum))           (set-circle! rm x y (r) fn)]
    [(< num (/ (+ wcirc wdiam) wsum)) (set-diamond! rm x y (r) fn)]
    [else                             (set-rect! rm x y (quotient (r) 2) (quotient (r) 2) fn)])
  (cons x y))

;; empties all spaces adjacent to an empty space
(define (expand-space! rm eprob)
  (match-define (grmap width height cells _) rm)
  (define opens (for/vector ([c cells]
                             [i (in-naturals)]
                             #:when (equal? c 'floor))
                  (cons (remainder i width) (quotient i width))))
  (for ([xy opens])
    (for ([dxdy '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))])
      (define x (+ (car xy) (car dxdy)))
      (define y (+ (cdr xy) (cdr dxdy)))
      (when (and (< -1 x width) (< -1 y height)
              (< (random) eprob))
        (map-set! rm x y 'floor)))))

;; randomly creates a path in rm from loc1 to loc2
(define (make-path! rm loc1 loc2)
  (when (not (equal? loc1 loc2))
    (define dx (- (car loc2) (car loc1)))
    (define dy (- (cdr loc2) (cdr loc1)))
    (define next (cond
      [(< (random) (/ (abs dx) (+ (abs dx) (abs dy))))
        (cons (+ (car loc1) (quotient dx (abs dx))) (cdr loc1))]
      [else (cons (car loc1) (+ (cdr loc1) (quotient dy (abs dy))))]))
    (map-set! rm (car next) (cdr next) 'floor)
    (make-path! rm next loc2)))

;; connects a vector of locations with a minimum-length spanning tree
(define/contract (connect! rm locs)
  (-> grmap? vector? void?)
  (define ufs (vector-map uf-new locs))
  (define pairs (for*/vector ([a (vector-length locs)] [b (vector-length locs)])
    (cons a b)))
  (define (dist pair) (loc-dist (vector-ref locs (car pair)) (vector-ref locs (cdr pair))))

  ;; draw paths
  (vector-sort! (lambda (x y) (< (dist x) (dist y))) pairs)
  (for ([pair pairs]
        #:unless (uf-same-set? (vector-ref ufs (car pair)) (vector-ref ufs (cdr pair))))
    (make-path! rm (vector-ref locs (car pair)) (vector-ref locs (cdr pair)))
    (uf-union! (vector-ref ufs (car pair)) (vector-ref ufs (cdr pair)))))

                
;; generates a random map
;;  width   - width of map generated
;;  height  - height of map generated
;;  N       - number of rooms in map
;;  mu      - average room radius
;;  sigma   - standard devation of room radius
;;  erounds - number of expansion rounds
;;  eprob   - probability of an expansion in a single round
;;  enms    - gvector of enemies to spawn
(define/contract (random-map width height N mu sigma erounds eprob enms)
  (-> integer? integer? integer? number? number? integer? number? gvector?
    (values grmap? location?))
  (define rm (full-map width height))
  (set-grmap-enemies! rm enms)
  (define locs (for/vector ([i N])
    (randomize! rm mu sigma (const 'floor))))
  (connect! rm locs)
  (for ([i erounds])
    (expand-space! rm eprob))

  ;; TODO: generate these differently
  (define usr-loc (vector-ref locs 0))
  (for ([enm enms]
        [loc (vector-drop locs 1)])
    (set-actor-loc! enm loc))

  (values rm usr-loc))



;; MAIN
#|
;(random-seed 1337)
;(define rm (random-map 160 96 20 (void) (void)))
;(display-map rm)

(define-values (rm usr-loc) (random-map
  160 ;; width
  96  ;; height
  20  ;; number of rooms
  7   ;; mean room radius
  3   ;; std dev of room radius
  5   ;; number of expansion rounds
  0.2 ;; expansion probability
  (gvector)
))
(display-map rm)
|#
