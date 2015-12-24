#lang racket
;; miscellaneous helper functions

(provide loc-dist)

(require "state.rkt")

;; manhattan distance between two locations
(define/contract (loc-dist a b)
  (-> location? location? integer?)
  (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b)))))
