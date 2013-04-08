#lang racket

;; Not yet completed

(require racket/dict)

(define *nodes* '((living-room "You are in the living-room. A Wizard is snoring loudly on the couch.")
                  (garden "You are in a beautiful garden.  There is a well in front of you.")
                  (attic "You are in the attic.  There is a giant welding torch in the corner.")))
(define *edges* '((living-room (garden west door) (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))
(define *objects* '(whiskey bucket frog chain))
(define *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))
(define *location* 'living-room)
(define *allowed-commands* '(look walk pickup inventory))

(define (describe-location location nodes)
  (cadr (assoc location nodes)))
(define (describe-path edge)
  (let ((ss (list "there is a "
                  (symbol->string (caddr edge))
                  " going "
                  (symbol->string (cadr edge))
                  " from here.")))
  (string-join ss)))
(define (describe-paths location edges)
  (map describe-path (cdr (assoc location edges))))
(define (describe-objects loc objs obj-loc)
  (labels ((describe-obj obj) (string-join (list "You see a " (symbol->string obj) " on the foor."

(describe-location 'living-room *nodes*)
(describe-paths 'living-room *edges*)
