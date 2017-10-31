#lang racket

(define-syntax when
  (syntax-rules ()
    ((when condition head . body)
     (if condition (begin head . body) #f))))

(define (answer? x)
  (= x 42))

(when (answer? 42)
  (newline)
  (display "The answer!"))

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


(define-syntax ouror
  (syntax-rules ()
    [(ouror) #f]
    [(ouror e1 e2 ...)
     (cond [e1 #t]
           [else (ouror e2 ...)])]))


(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (fork proc)
  (call/cc
   (λ(cc)
     (enqueue cc)
     (proc))))

(define (yield by)
  (call/cc
   (λ(cc)
     (enqueue cc)
     (begin
       (displayln (string-append "Yield by " by))
       ((dequeue))))))

(define (cexit prov)
  (if (empty-queue?)
      (exit)
      (begin
        (displayln prov)
        ((dequeue)))))

(define ((do-stuff str max))
  (let loop ((n 0))
    (display str)
    (display "  ")
    (display n)
    (newline)
    (yield (string-append str (number->string n)))
    (if (< n max)
        (loop (+ 1 n))
        (cexit (string-append "do-stuff " str)))))
          
(define (main)
  (begin
    (fork (do-stuff "A" 3))
    (fork (do-stuff "B" 4))
    (displayln "End")
    (cexit "Main")))