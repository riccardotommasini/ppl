#lang racket

;; MACRO

;;   Here are the rules for pattern matching:
;;
;;  - A constant pattern will only match against an EQUAL? constant.
;;    We'll exploit this later on.
;;
;;  - A symbol that is one of the `literals' can only match against the
;;    exact same symbol in the form, and then only if the macro user
;;    hasn't shadowed it.
;;
;;  - A symbol that is *not* one of the literals can match against *any*
;;    complete form.  (Forgetting this can lead to surprising bugs.)
;;
;;  - A proper list of patterns can only match against a list form of
;;    the same length and only if the subpatterns match.
;;
;;  - An improper list of patterns can only match against a list form of
;;    the same or greater length and only if the subpatterns match.  The
;;    `dotted tail' of the pattern will be matched against the remaining
;;    elements of the form.  It rarely makes sense to use anything but a
;;    symbol in the dotted tail of the pattern.
;;
;;  - The ... token is special and will be discussed a bit later.


(define-syntax ++
  (syntax-rules ()
    ((_ i)
       (set! i (+ 1 i)))))

;; CHOOSE-ARG takes a number + some arguments
;; (choose 2 a b c) -> c

(define (factorial n)
  (foldl (lambda (i x) (* i x)) 1 (range 1 (+ n 1))))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))
                                   
(define choose-arg 
  (lambda (i . args)
    (if (zero? i)
        (car args)
        (apply choose-arg (- i 1) (cdr args)))))





(define-syntax choosem
  (syntax-rules ()
    ((_ i f) f)
    ((_ i f f1 ...)
     (if (zero? i)
         f
         (choosem (- i 1) f1 ...)))))

;; (choosem 1 (factorial 5) (+ 1 2) (fib 42))
;; (choosem 1 (fact 5) (+ 1 2) (fib 42))
;;  (if (zero? 1)
;;      (fact 5)
;;      (choosem (- 1 1) (+ 1 2) (fib 42))

;; (choosem 0 (+ 1 2) (fib 42))
;;  (if (zero? 0)
;;      (+ 1 2)
;;      (choosem (- 1 0) (fib 42))

