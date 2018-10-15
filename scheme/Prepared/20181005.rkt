#lang racket

;;this is a comment

(define (hello-world) (display "Hello World"))


(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


;; reverse pairs

(define (riap p)
  (if (not (pair? p))
      (error "not a pair")
      (let ([l (cdr p)]
            [r (car p)])
        (cons l r))))

(define (tsil l)
  (cond [(not (list? l)) (error "not a list")]
        [(null? l) l]
        [else (append (tsil (cdr l)) (list (car l)))]))


(define (range low high)
  (define (rec low high acc)
    (if (>= low high)
        acc
        (rec (+ low 1) high (cons low acc))))
  (tsil (rec low high '())))

(define (range2 low high)
  (define (rec low high acc)
    (if (= low high)
        acc
        (rec (+ low 1) high (append acc (list low)))))
  (rec low high '()))

;; list flattens


(define (flatten lol)
  (define (help lol acc)
    (if (null? lol)
        acc ;; I finished
        (help (cdr lol) (append acc (car lol)))))
  (help lol '()))


;; Homework Abitrary nesting

;; cartesian product of two lists

(define (cp l1 l2)
  (define (help l1 l2 acc)
    (cond [(null? l1) acc]
          [(null? (cdr l1))
           (let loop ([p1 (car l1)]
                      [p2 l2]
                      [acc2 acc])
             (if (null? p2)
                 acc2
                 (loop p1 (cdr p2) (append acc2 (list (* p1 (car p2)))))))]
          [else (append (help (list (car l1)) l2 acc) (help (cdr l1) l2 acc))]))
  (help l1 l2 '()))


(cp '(1 2 3) '(4 5 6))

