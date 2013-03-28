#lang racket
;; lower bound of the possible answers
(define *small* 1)
;; upper bound of the possible answers
(define *big* 100)
;; makes a guess by picking the center of the two numbers
;; the shift is a fast integer based divide by two making this
;; an average of two numbers
(define (guess-my-number) (arithmetic-shift (+ *small* *big*) -1))
;; Sets the upper bound to one less than the current guess
(define (smaller)
  (set! *big* (sub1 (guess-my-number)))
  (guess-my-number))
;; Sets the lower bound to one higher than the current guess
(define (bigger)
  (set! *small* (add1 (guess-my-number)))
  (guess-my-number))
;; resets the bounds to start searching again
(define (start-over)
  (set! *small* 1)
  (set! *big* 100)
  (guess-my-number))