#lang racket
;; This is a comment
(define (hello-world) (display "Hello world")) ; this is an inline comment

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Write a function that reverses a pair
(define (rev-pair p)
  (if (not (pair? p))
      (error "Please provide pair...")
      (let ([first (car p)]
            [second (cdr p)])
        (cons second first))))

(define (pal s)
  (let* ([l (string->list s)]
         [reverse (reverse l)])
      (equal? l reverse)))

;; Write a function that returns the sum of squares of a list l
;; '(1 | 2 3 4 5) -> '(x | xs...)
(define (sos l)
  ;; enclosed f
  (define (rec l acc)
    (if (null? l)
        acc
        (let ([x (car l)]
              [xs (cdr l)])
          (rec xs (+ acc (* x x))))))
  ;; end rec
  (rec l 0))

;; Range function. That provided lo and hi provides the range [lo, hi)
(define (range lo hi)
  (define (rec lo hi acc)
    (if (>= lo hi)
        acc
        (rec (+ lo 1) hi (cons lo acc))))
  (reverse (rec lo hi '())))

(define (range-let lo hi)
  (let loop ([l lo]
             [acc '()])
    (if (>= l hi)
        acc
        (loop (+ l 1) (append acc (list l))))))

;; Define a function that flattens lists
;; e.g. '(1 2) -> '(1 2)
;;      '(1 (1) 2) -> '(1 1 2)
;;      '(1 (1 2 3 4) 1 3 (8 (9 7 4) 1)) -> '(1 1 2 3 4 1 3 8 9 7 4 1)
(define (flatten l)
  (define (rec l acc)
    (if (null? l)
        acc
        (let ([x (car l)]
              [xs (cdr l)])
              (if (not (list? x))
                  (rec xs (cons x acc))
                  (rec xs (rec x acc))))))
  (reverse (rec l '())))

;; Implement quick-sort
(define (qs l)
  (if (null? l)
      l
      (let ([x (car l)]
            [xs (cdr l)])
        (if (null? xs)
            (list x)
            (let* ([pivot x]
                   [smaller (filter (lambda (x) (<= x pivot)) xs)]
                   [bigger (filter (lambda (x) (> x pivot)) xs)])
              (append (qs smaller) (list x) (qs bigger)))))))

;; A function that packs elements
;; '(1 1 1 2 1 1 3 3 4 4 4) -> '((1 1 1) (2) (1 1) (3 3) (4 4 4))
(define (pack l)
  (define (rec l sub acc)
    (cond [(null? l) (cons sub acc)]
          [(or (null? sub) (equal? (car l) (car sub)))
           (rec (cdr l) (cons (car l) sub) acc)]
          [else (rec (cdr l) (list (car l)) (cons sub acc))]))
  (reverse (rec l '() '())))

    
        

    




  




