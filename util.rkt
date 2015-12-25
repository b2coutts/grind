#lang racket
;; miscellaneous helper functions

(provide loc-dist grmap-ref fmt)

(require "state.rkt")

;; produces a string displaying an object with given escape codes
(define/contract (fmt datum . codes)
  (->* (any/c) #:rest (listof integer?) string?)
  (define esc-str (cond
    [(empty? codes) ""]
    [else (format "\x1b[~am" (string-join (map ~a codes) ";"))]))
  (string-append esc-str (~a datum)))

;; manhattan distance between two locations
(define/contract (loc-dist a b)
  (-> location? location? integer?)
  (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b)))))

;; get cell at location (x,y)
(define/contract (grmap-ref fmap x y)
  (-> grmap? integer? integer? cell?)
  (vector-ref (grmap-cells fmap) (+ (* (grmap-width fmap) y) x)))
