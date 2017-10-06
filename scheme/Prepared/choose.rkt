#lang racket

;choose: it is used to choose among a list of choices.
;if, at some point of the computation, the choice is not the right one, one can
;just fail.
;it is very convenient e.g. to represent nondeterminism
;think about automata: when we have a nondeterministic choice among say a, b, or c, we can just (choose ’(a b c))
;main idea: we use continuations to store the alternative paths when we choose
;if we fail, we backtrack

(define *paths* '())

(define (choose choices)
  (if (null? choices)
      (fail)
      (call/cc
       (λ (cc)
         (set! *paths*
               (cons (λ ()
                       (cc (choose (cdr choices)))) ; I save the current continuation in my path list
                     *paths*))
         (car choices))))) ; return first choice

(define fail #f)
(call/cc ; this should be done here to save the current continuation 
 (λ (cc)
   (set! fail
         (λ ()
           (if (null? *paths*)
               (cc '!!failure!!)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))

;;EXAMPLE

(define (is-the-sum-of sum)
  (unless (and (>= sum 0) (<= sum 10))
    (error "Out of range" sum))
  (let ((x (choose '(0 1 2 3 4 5)))
        (y (choose '(0 1 2 3 4 5))))
        (if (= (+ x y) sum)
            (list x y)
            (fail))))

                