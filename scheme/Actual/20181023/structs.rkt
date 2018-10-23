#lang racket


;; Zip (zip list1 list2) -> list of pairs
;; (zip '(1 2 3) '("a" "b" "c")) -> '((1 . "a")..)

(define (zip l1 l2)
  (define (helper l r acc)
    (if (or (null? l) (null? r))
        acc
        (helper (cdr l) (cdr r)
                (append acc
                        (list (cons (car l) (car r)))))))
  (helper l1 l2 '()))

(define (iter l)
  (let ((ll l))
    (lambda ()
      (if (null? ll)
        '()
        (let ((v (car ll)))
          (set! ll (cdr ll))
          v)))))

(define (zip2 l1 l2)
  (let ((i (iter l2)))
    (map (lambda (x) (cons x (i))) l1)))

;; STRUCTS


(struct leaf
  ((content #:mutable)
   empty))
  
(struct node leaf
  (left right))

(define (create-leaf v)
  (leaf v #f))

(define (create-node v n1 n2)
  (node v #f n1 n2))


(define l (create-leaf 1))
(define r (create-leaf 2))

(define n1 (create-node 3 l r))
(define n2 (create-node 4 l r))

(define n3 (create-node 5 n1 n2))

(define empty (leaf -1 #t))

(define n4 (create-node 5 l empty))

(define n5 (create-node 4 n4 empty ))

(define root (create-node "root" n4 n5))

(define (fanout t)
  (define (helper t acc)
    (if (not (node? t))
        (+ 1 acc)
        (let [(l (helper (node-left t) (+ 1 acc)))
              (r (helper (node-right t) (+ 1 acc)))]
          (if (<= l r)
              r
              l))))
  (helper t 0))