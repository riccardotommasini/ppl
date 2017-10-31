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

;; Simple examples


(define-syntax ++
  (syntax-rules ()
    ((_ i)
     (let ((a 0))
       (set! i (+ 1 i))))))

(let ((i 0))
  (begin
    (display i)
    (newline)
    (++ i)
    (display i)
    (newline)))

(define-syntax **
  (syntax-rules ()
    ((_ i)
     (let ((a i))
       (set! i (* a i))))))

(let ((i 5))
  (begin
    (display i)
    (newline)
    (** i)
    (display i)
    (newline)))

(define (fun+* i . rest)
  (if (null? rest)
      (list (+ i 1))
      (append (list (+ 1 i)) (apply fun+* rest))))

(fun+* 1)
(fun+* 1 2 4)

(let ((i 0)) (++ i) (display i))

;; Fun +* a s MACRO
;

(define-syntax +*
  (syntax-rules ()
    ((_ i) (begin (set! i (+ 1 i))))
    ((_ i ...) (begin (set! i (+ 1 i)) ...))))

(let ((i 0) (j 2)) (+* i j) (display i) (newline) (display j))
    
; Macro to compute the max between two values
(define-syntax my-max
  (syntax-rules ()
    (( _ a b)
     (if (> a b) a b))))

;;WHY WRITING MACROS

(define (fact n)
    (foldl (λ (i x) (* i x)) 1 (range 1 (add1 n))))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

;; CHOOSE-ARG takes a number and some arguments. The whole expression should evaluate to the argument selected by the number argument. 
(define choose-arg
  (λ (i . args)
    (if (zero? i)
        (car args)
        (apply choose-arg (- i 1) (cdr args)))))

;(time (choose-arg 1 (factorial 5) (+ 1 2) (fib 40))) 
;; Very slow because Scheme must fully evaluate all procedure arguments before applying a procedure
;; We can speed up using thunks (functional wrappers, i.e. lambdas)

;(time ((choose-arg 1 (λ () (fact 5)) (λ () (+ 1 2)) (λ () (fib 40)))))

;; UNREADABLE!

(define-syntax choose-arg-m
  (syntax-rules ()
    ((_ i f) f)
    ((_ i f f1 ...) (if (zero? i) f (choose-arg-m (sub1 i) f1 ...)))))

;; (time (choose-arg-m 1 (fact 5) (+ 1 2) (fib 40)))
;; EXPANSION
; (choose-arg-m 1 (fact 5) (+ 1 2) (fib 40)) =>
;    (if (zero? 1)
;      (fact 5)
;      (choose-arg-m (+ 1 2) (fib 40))
;
; (choose-arg-m (sub1 1) (+ 1 2) (fib 40)) =>
;    (if (zero? (sub1 1))
;        (+ 1 2)
;        (chose-arg-m (sub1 (sub1 1)) (fib 40)))
;              ... -> single function case


;; N-ary macros

;; WHEN

(define-syntax when0
  (syntax-rules ()
    ((when condition . body ) (if condition (begin rest) #f))))

;(when0 (< 1 2) (display 5)(newline)) => (if (< 1 2) (begin ((display 5) (newline)) #f) Begin takes a list

(define-syntax when1
  (syntax-rules ()
    ((when condition . body ) (if condition (begin . rest) #f))))

;; (when (< 1 2)) => (if (< 1 2) (begin) #f)
;; let's ensure there at least one instruction
(define-syntax when
  (syntax-rules ()
    ((when condition first . rest) (if condition (begin first . rest) #f))))


;; Importance of . in begin
;; Importance of first in pattern (begin)

(let ((x 1)) (when (> 2 x) (newline)(display x)(newline)))

;; Multiple patterns
;; Patterns are matched in order (the first that matches is the one which gets expanded)
;; If no match -> error

;; Errors

(define-syntax prohibit-one-arg
   (syntax-rules ()
     ((prohibit-one-arg function argument) (if)) ;; bogus expansion
     ((prohibit-one-arg function . arguments)
      (function . arguments))))

;(prohibit-one-arg + 2 3)
;; Expected 5

;(prohibit-one-arg display 'foo)
;; Expected if: bad syntax (has 0 parts after keyword) in: (if)

;; Better errors: we want the raised at expansion time

;; Recursive macro for error expansion
(define-syntax syntax-error
   (syntax-rules ()
     ((syntax-error) (syntax-error "Bad use of syntax error!"))))

(define-syntax prohibit-one-arg-errors
   (syntax-rules ()
     ((_ function argument)
      (syntax-error
       "Prohibit-one-arg-errors cannot be used with one argument."
       function argument))
     ((_ function . arguments)
      (function . arguments))))

;(prohibit-one-arg-errors display 3)

;; BEST PRACTICE: SAY WHAT YOU DON'T WANT
;; Protect against accidental pattern matching by writing guard  patterns that match the bad syntax.

;; Recursive Macro

;; On Syntax Error The syntax-error macro is designed to fail to match
;; anything but a no-argument call.  A no-argument call to syntax-error
;; expands into a one-argument call to syntax-error which fails to match.

;; Ellipses ...
;; The ... operator modifies how the PREVIOUS form is interpreted by the macro language.
;; In a macro pattern, ellipses can only appear as the last token in a
;; LIST or VECTOR pattern

;; SYNTAX RULES

; for-expression a la Python
; (for x in '(1 2 3) (display x) (newline))
(define-syntax for
  (syntax-rules (in)
    ((_ var in lst fun ...)
     (for-each (λ (var)
                 fun ...) lst))))


;; CONTINUATIONS & CLOSURES

(display
  (call/cc (lambda (cc)
            (display "I got here.\n")
            (cc "This string was passed to the continuation.\n")
            (display "But not here.\n"))))

(define (continuation-factorial n)
  (letrec
      ([f (λ (n cc)
            (if (zero? n)
                (cc 1)
                (f (sub1 n)
                   (λ (a)
                     (cc (* n a))))))])
    (call/cc
     (λ (cc)
       (f n cc)))))


; right-now : -> moment
(define (right-now)
  (call/cc
   (lambda (cc) 
     (cc cc))))

; go-when : moment -> ...
(define (go-when then)
  (then then))


; An infinite loop:
;(let ((the-beginning (right-now)))
;  (display "Hello, world!")
;  (newline)
;  (go-when the-beginning))


; "break" in a loop
(define (break-test)
  (call/cc (lambda (break)
             (for-each
              (lambda (i)
                (if (= (remainder i 2) 0)
                    (break)
                    (begin
                      (display i)
                      (newline))))
              '(1 2 3 4 5 6)))))

; "continue" in a loop
(define (continue-test)
  (for-each
   (lambda (i)
     (call/cc (lambda (continue)
                (if (= (remainder i 2) 0)
                    (continue)
                    (begin
                      (display i)
                      (newline))))))
   '(1 2 3 4 5 6)))
