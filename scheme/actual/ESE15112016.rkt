#lang racket

;; Clarify Continuations: call/cc returns a procedure called "Escape Procedure" which, if evaulated,
;; restores the status of the computation the moment of the call/cc invocation.
;; The escape procedure can also take one arguement. If an arument is passed to it, that is what it returns
;; otherwise it returns the current continuation.

;; Clarification (1) (Right now example)

(define (right-now)
  (call/cc
   (lambda (cc) 
     (cc cc))))

;; Is the same of writing

(define (right-now2)
  (call/cc
   (λ (cc)
     (cc))))

;; Clarification (2) String Example

(define (call/cc-test)
  (call/cc (λ (escape)
             (display "Before Escaping\n")
             (escape "Escaping\n")
             (display "After Escaping\n"))))

(display ; <- Display!! 
  (call/cc (lambda (cc)
            (display "I got here.\n")
            (cc "This string was passed to the continuation.\n")
            (display "But not here.\n"))))

(call/cc (lambda (cc)
            (display "I got here.\n")
            (cc "This string was passed to the continuation.\n")
            (display "But not here.\n")))

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

(define (co-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue)))) ; returns the control to the caller, return the result of the evaluation resumed (car of queue)

(define (do-something e max)
  (λ ()
    (let loop ([n 0])
      (display e)(display " ")(display n)(newline)
      (yield)
      (if (< n max)
          (loop (+ 1 n))
          (co-exit)))))

(define (coroutine-test)
  (begin
    (make-coroutine (do-something "Procedure A\n" 5))
    (make-coroutine (do-something "Procedure B\n" 5))
    (make-coroutine (do-something "Procedure C\n" 5))
    (co-exit)))

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

;Function which creates a function representing the composition of two functions.

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

(define-syntax repeat
  (syntax-rules (until)
    ((_ body ... until cond)
     (let loop ()
       (begin
         body ...
         (when cond
           (loop)))))))

#|
(let ((x 5))
  (repeat
   (display x)
   (newline)
   (set! x (- x 1))
   until (positive? x)))
|#                                                                              