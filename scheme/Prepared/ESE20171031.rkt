#lang racket

;;Warm-up with macros

(define-syntax repeat
  (syntax-rules (until)
    ((_ body ... until cond)
     (let loop ()
       (begin
         body ...
         (unless cond
           (loop)))))))

(let ((x 5))
    (repeat
     (display x)
     (newline)
     (set! x (sub1 x))
     until (zero? x)))

;; N-ary macros WHEN

;; Using the dotted tail we can an arbitrary number of arguments to the macro

(define-syntax when0
  (syntax-rules ()
    ((when condition . body ) (if condition (begin body) #f))))

;;Usage

(define (answer? x)
  (= x 42))

;(when0 (answer? 42)
;      (newline)
;       (display "Bad number: is not the answer."))

;; although, the pattern variable body is the list of the remaining arguments
;; be careful

;; we expect the pattern to match as
;; condition = (answer? x)
;; body = ((newline) (display "Bad number:  is not the answer."))
;; but it actually expands to a list, i.e:
;; (if (answer? x)
;; (begin ((newline) (display "Bad number:  negative."))) #f)

(define-syntax when1
  (syntax-rules ()
    ((when condition . body ) (if condition (begin . body) #f))))

;;USAGE
(when1 (answer? 42)
      (newline)
       (display "Bad number: is not the answer."))

;; (when (answer? 42)) => (if (answer? 42) (begin) #f)

;; the pattern match the variable body to the empty list
;; this may actually print a newline and display the string and then raising the error (hard to debug)
;; let's ensure there at least one instruction

;; The right one
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

;; (prohibit-one-arg + 2 3)
;; Expected 5

;; (prohibit-one-arg display 'foo)
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

;: (prohibit-one-arg-errors display 3)

;; BEST PRACTICE: SAY WHAT YOU DON'T WANT
;; Protect against accidental pattern matching by writing guard  patterns that match the bad syntax.

;; Recursive Macro

(define-syntax myor
  (syntax-rules ()
    [(myor) false]
    [(myor e1 e2 ...)
     (cond [e1 true]
           [else (myor e2 ...)])]))

;; On Syntax Error The syntax-error macro is designed to fail to match
;; anything but a no-argument call.  A no-argument call to syntax-error
;; expands into a one-argument call to syntax-error which fails to match.

;;TODO

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

; Macro: rotate a list
(define-syntax-rule (swap x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))

(define-syntax rotate
  (syntax-rules ()
    ((rotate a) (void))
    ((rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...)))))

;; Continuations Applications

;; Coroutines. [http://bit.ly/2ewHEHc]
;; It is often convenient to structure a computation as an alternation between two different processes,
;; perhaps one that produces items and another that consumes them.
;; It may not be convenient to either of those processes into a subroutine that can be called once to get an item,
;; because each process may have complex state encoded into its control structure.

;; Coroutines allow you to structure a program with its own natural control structure;
;; suspending its operation when it needs the other to do its cooperating subprograms,
;; without making one program subservient to another.

;; In practice we want to design a procedure that each time it gets called it perform a portion of its
;; computation, suspends it, and return the value it computed, freezing its internals to that moment (yelding).
;; Such a procedure, if called again, resumes the computation from the last point of it. 

;; Let's define a queue to save the "stages" of computation and some handy methods.
(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (make-coroutine proc)
  (call/cc
   (λ (cc)
     (enqueue cc) ; save the current continuation
     (proc)))) ; evaluate the procedure

(define (yield) ;; return the current stage after saving what was "before" so that it can be resumed
  (call/cc
   (λ (cc)
     (enqueue cc) ; save the current continuation
     ((dequeue))))) ; returns the control to the caller, return the result of the evaluation resumed (car of queue)

(define (co-exit x)
  (if (empty-queue?)
      (exit)
      (begin
        (displayln x)
        ((dequeue))))) ; returns the control to the caller, return the result of the evaluation resumed (car of queue)

(define (do-something e max)
  (λ ()
    (let loop ([n 0])
      (display e)(display " ")(display n)(newline)
      (yield)
      (if (< n max)
          (loop (+ 1 n))
          (co-exit "do")))))

(define (coroutine-test)
  (begin
    (make-coroutine (do-something "Procedure A\n" 5))
    (make-coroutine (do-something "Procedure B\n" 5))
    (make-coroutine (do-something "Procedure C\n" 5))
    (co-exit)))

(define ((do-stuff-n-print str max))
  (let loop ((n 0))
    (display str)
    (display " ")
    (display n)
    (newline)
    (yield)
    (if (< n max)
        (loop (+ 1 n))
        (co-exit (string-append "do " str)))))

(define (main)
  (begin
    (make-coroutine (do-stuff-n-print "This is A" 1))
    (make-coroutine (do-stuff-n-print "This is B" 2))
    (displayln "End")
    (co-exit "main" )))
;(coroutine-test)
;; Domanda per matteo: ma non dovrei poter rivalutare la funzione a mano ed avere un risultato progressivo?
;; Posso, usando una macro, definire una funzione che ha un set di variabili interne? 



;; Non Deterministic Operators: Backtracking it is used to choose among a list of choices.
;; if, at some point of the computation, the choice is not the right one, one can
;; just fail.
;; it is very convenient e.g. to represent nondeterminism
;; think about automata: when we have a nondeterministic choice among say a, b, or c, we can just (choose ’(a b c))
;; main idea: we use continuations to store the alternative paths when we choose
;; if we fail, we backtrack

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


;; Higher order functions

;; given a list and an interval check if each element in the list is in t

(define (range-check min max l)
  (foldl (λ (x xs)
           (if (and (< x max) (> x min))
               (append xs (list x))
               xs)) '() l))


(range-check 1 9 (range 0 11))

(define (range-checkr min max l)
  (foldr (λ (x xs)
           (if (and (< x max) (> x min))
               (append (list x) xs)
               xs)) '() l))

(range-checkr 1 9 (range 0 11))

;; Function which converts a list of digits to the corespodning number.

(define (^ a b)
  (let loop ((n 0) (res 1))
            (if (< n b)
                (loop (+ 1 n) (* a res))
                res)))

(define (binary-converter l)
  (foldr (λ (exp acc res) (+ res (* acc (^ 2 exp)))) 0 (reverse (range 0 (length l))) l))

(binary-converter '(0))
(binary-converter '(1))
(binary-converter '(0 1))
(binary-converter '(1 0))
(binary-converter '(0 0 1))
(binary-converter '(0 1 0))
(binary-converter '(0 1 1))
(binary-converter '(1 0 0))
(binary-converter '(1 0 1))
(binary-converter '(1 1 0))
(binary-converter '(1 1 1))

(define (converter base l)
  (foldr (λ (exp acc res) (+ res (* acc (^ base exp)))) 0 (reverse (range 0 (length l))) l))

(converter 2 '(0))
(converter 2 '(1))
(converter 2 '(0 1))
(converter 2 '(1 0))
(converter 2 '(0 0 1))
(converter 2 '(0 1 0))
(converter 2 '(0 1 1))
(converter 2 '(1 0 0))
(converter 2 '(1 0 1))
(converter 2 '(1 1 0))
(converter 2 '(1 1 1))
(converter 5 '(1 3 2)) ;-> 42

;; Function which creates a function representing the composition of two functions.

(define (o f g)
 (if (and (procedure? f) (procedure? g))
  (λ (args) (f (g args)))
  (error "Not a function")))

((o list (λ (x) (+ 1 x))) 1)


;; Sorting

(define (sort lst)
  (define (min lst)
    (foldl (λ (x y) (if (< x y) x y)) (car lst) (cdr lst)))
  (if (null? lst)
      '()
      (let ((m (min lst)))
            (cons m (sort (remove m lst))))))

;; bucket : (listof number) -> (listof (listof number))
;; divides a list into a list of sublists,
;; where the sublists are
;; composed of adjacent equal numbers in the original list.

;; Others

; repeat-until, a variant of the do-while construct



#|
(let ((x 5))
  (repeat
   (display x)
   (newline)
   (set! x (- x 1))
   until (positive? x)))
|#

