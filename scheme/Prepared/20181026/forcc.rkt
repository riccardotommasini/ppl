#lang racket

(define break #f)
(define continue #f)



(define-syntax for
  (syntax-rules (in)
    ((_ v in l body ...)
    (call/cc (lambda (b)
       (set! break b)
       (let loop ((l1 l))
         (unless (null? l1)
          (call/cc (lambda (c)   
                      (set! v (car l1))
                      (set! continue c)
                      (begin
                        body ...)))
           (loop (cdr l1)))))))))


(define x #f)

(for x in '(1 2 3)
  (begin
    (display "initial")
    (display x)
    (newline))
  (if (= (remainder x 2) 0)
           (continue "continue")
           (begin
             (display "mid")
             (display (+ x 1))
             (newline)))
  (begin
    (display "end")
    (display (- x 1))
    (newline)))