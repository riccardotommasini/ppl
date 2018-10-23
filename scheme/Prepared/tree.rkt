#lang racket

(struct leaf
  (( content #:mutable) empty))

;; inheritance 
(struct node leaf
  (left right))

(define (display-leaf l)
  (if (leaf? l)
      (begin
        (display "\t Leaf ")
        (display (leaf-content l)))

      (display "Not a leaf")))

;; Helper function
(define (create-empty)
  (leaf -1 #t))

(define (create-node v t1 t2)
  (node v #f t1 t2))

(define (display-tree t)
  (if (not (node? t))
      (display-leaf t)
      (begin
        (display "\t\t Node ")
        (display (leaf-content t))
        (newline)
        (display-tree (node-left t))
        (newline)
        (display-tree (node-right t))))
        (newline))

(define (create-leaf v)
  (leaf v #f))

(define (nodes t)
  (define (helper t acc)
    (if (not (node? t))
        (+ acc 1)
        (+ (helper (node-left t) (+ 1 acc)) (helper (node-right t) (+ 1 acc)))))
  (helper t 0))


(define (fanout t)
  (define (helper t acc)
    (if (not (node? t))
        (+ acc 1)
        (let [
               (l (helper (node-left t) (+ 1 acc)))
               (r (helper (node-right t) (+ 1 acc)))]
        (if (<  l r)
            r
            l))))
  (helper t 0))

   
(define l1 (create-leaf 11))
(define l2 (create-leaf 12))

(define l3 (create-leaf 13))
(define l4 (create-leaf 14))

(define n1 (create-node 21 l1 l2))

(define n2 l3)

(define t (create-node 200 n1 n2))


(nodes t)
(fanout t)
(fanout n1)
(fanout n2)