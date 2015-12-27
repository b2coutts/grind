#lang racket
;; config file for the controls

(provide bindings)

(define/contract bindings
  (hash/c (or/c char? symbol?) symbol?)
  `#hash(
    (#\h            . left)
    (#\j            . down)
    (#\k            . up)
    (#\l            . right)
    (#\f            . select)
    (#\d            . back)
    (#\s            . menu)
    (ctrl-q         . quit)
  ))
