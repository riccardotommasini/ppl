#lang racket

(define (hello-world) (display "hello world"))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (tsil l)
  (cond [(null? l) l]
        [(not (list? l)) (list l)]
        [else (append (tsil (cdr l)) (tsil (car l)))]))

;; Given a number n the function return the range
;; list [0,...,n-1] 
(define (class1 n)
  (define (help low high acc)
    (if (= low high)
        acc
        (help (+ low 1) high (cons low acc))))
  (tsil (help 0 n '())))

(define (class1b n)
  (if (= n 1)
      (list 0)
      (let ([p (- n 1)])
        (append (class1b p) (list p)))))

;; '(0 1 '(2 3) 4 '(5 6 7) '(8 9 10)) -> (class1b 11)

(define (di ls)
  (define (help l acc)
    (cond [(null? l) acc]
          [(not (list? (car l)))
           (append (append '() (list (car l))) (help (cdr l) '()))] 
          [else (append (help (car l) '()) (help (cdr l) '()))]))
  (help ls '()))


(define (cp l1 l2)
  (define (help l1 l2 acc)
    (cond [(null? (cdr l1))
           (let loop ([p1 (car l1)]
                       [p2 l2]
                       [acc2 acc])
                      (if (null? p2)
                          acc2
                          (loop p1 (cdr p2) (append acc2 (list (* p1 (car p2)))))))]
          [else (append (help (list (car l1)) l2 acc)
         
                        (help (cdr l1) l2 acc))]))
  (help l1 l2 '()))
 
                      



