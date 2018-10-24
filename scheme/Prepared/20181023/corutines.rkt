#lang racket
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
     (enqueue cc) ; save the current continuation (appends)
     ((dequeue))))) ; returns the control to the caller, return the result of the evaluation resumed pops

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
    (co-exit "test")))

(define ((do-stuff-n-print str max))
  (let loop ((n 0))
    (display str)
    (display " ")
    (display n)
    (newline)
    (yield)
    (if (< n max)
        (loop (+ 1 n))
        (co-exit str))))

(define (main)
  (begin
    (make-coroutine (do-stuff-n-print "This is A" 1))
    (make-coroutine (do-stuff-n-print "This is B" 2))
    (displayln "End")
    (co-exit "main" )))
;(coroutine-test)