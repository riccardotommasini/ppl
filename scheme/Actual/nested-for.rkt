#lang racket

(define break #f)
(define continue #f)
(define break-list '())
(define continue-list '())

;; Simple FIFO queue
(define-syntax pop
  (syntax-rules ()
    ((_ q)
     (let ([h (car q)])
       (begin (set! q (cdr q))
              h)))))

(define (front q)
  (car q))

(define-syntax push
  (syntax-rules (in)
    ((_ e in q)
     (set! q (cons e q)))))

;; Each time we create a new nested loop, we save continuations of the
;; outer loop in the queue and set the break and continue functions to
;; the continuations of the inner loop. Once the loop has ended we
;; take the continuations of the outer loop from the queue and use
;; them again.
(define-syntax for
  (syntax-rules (in)
    ((_ v in l code ...)
     (call/cc
      (lambda (break-cont)
        (push break-cont in break-list)
        (set! break break-cont)
          (for-each
           (lambda (i)
             (let ([v i])
               (call/cc
                (lambda (continue-cont)
                  (push continue-cont in continue-list)
                  (set! continue continue-cont)
                  code ...
                  (pop continue-list)
                  (set! continue (if (empty? continue-list)
                                     #f
                                     (front continue-list)))
                  ))))
           l)
          (pop break-list)
          (set! break (if (empty? break-list)
                          #f
                          (front break-list)))
          )))))