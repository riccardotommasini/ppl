#lang racket

;; Zip function gets two lists and create a list of pairs

(define (zip l r)
  (define (helper l r res)
   (if (or (null? l) (null? r))
       res
       (helper (cdr l) (cdr r) (append res (list (cons (car l) (car r)))))))
  (helper l r '()))


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


(define (zip3 l1 l2)
  (let ((i (iter (reverse l2))))
    (foldr (lambda (x y) (cons (cons x (i)) y)) '() l1)))

(define (zip4 l1 l2)
  (let ((i (iter l2)))
    (foldl (lambda (x y) (append y (list (cons x (i))))) '() l1)))

(zip '(1 2 3) '(4 5 6))
(zip2 '(1 2 3) '(4 5 6))
(zip3 '(1 2 3) '(4 5 6))
(zip4 '(1 2 3) '(4 5 6))
