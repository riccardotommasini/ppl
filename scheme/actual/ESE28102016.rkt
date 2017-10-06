#lang racket

;; riccardo.tommasini@polimi.it

;; Style Guide
;; Startline comments call for two semicolons
;; Put all closing parentheses on one line, the last line of your code.
;; Each definition and each local definition deserves at least one line.
;; The Lisp convention is to use full English words separated by dashe
;; Prefix a function name with the data type of the main argument
;; Suffix a variable name with its type
;; Some suffix concentions:
;;    ?, predicates and boolean-valued functions, e.g. boolean?
;;    !, setters and field mutators, e.g. set!

(define (hello-world)
  (display "Hello World")) ; inline comments require only one semicolon
  
;; First exercise is a factorial. It allows us to see a couple of simple concepts
;; - Function definition
;; - Contional Expressions
;; - Recursion (Not Tail Recursion Yet)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

;; Handy Functions

;; Comparison
;; - eq? (does not ensure termination, e.g. recursive data structure) eqv? equal?
;; - even? odd? zero? list?
;; - null? pair?

;; quote, eval

;; Scheme basics Variable binding:

;; Define
(define TA-email "riccardo.tommasini@polimi.it")

;; Let
(define (reverse-pair p)
  (if (pair? p)
      (let ((x (car p))
            (y (cdr p)))
        (cons y x))
      (error "Not a pair")))

;; Let*
(define (life-milestones y)
  (let* ((age (- 2016 y))
        (adult-year (- 2016 (- age 18))))
    (display "You are ")
    (display age)
    (display " years old")
    (newline)
    (display "You got 18 in ")
    (display adult-year)))

;; Sum of squares: take a list of numbers as its argument and returns the sum of the squares of the elements of the list.
;; simple optimization is reduce car access by using a let 

(define (sum-of-squares l)
    (if (null? l)
        0
        (if (null? (cdr l))
              (* (car l) (car l))
              (+ (* (car l) (car l)) (sum-of-squares (cdr l))))))

;; Sum of squares tail recursive. NB recursion call is the lastest thing to call

(define (sum-of-squares-tail l)
  (define (sos l acc)
    (if (null? l)
        acc
        (sos (cdr l) (+ acc (* (car l) (car l))))))
    (sos l 0))

;; Sum of squares tail recursive with named let
;; named let locally binds a procedure to an id
;; it allows us to use labdas

(define (sum-of-squares-named-let l)
  (let sos ((ls l)
             (acc 0))
    (if (null? ls)
        acc
        (sos (cdr ls) (+ acc (* (car ls) (car ls)))))))

;; Playing with Lists

;; Handy Functions
;; - cons
;; - list
;; - member
;; - length
;; - append
;; - reverse
;; - filter
;; - partions (Notably, this function return two values)

;; Range function
(define (range lo hi)
  (if (> lo hi)
      (error "lo > hi")
      (if (< lo hi)
          (cons lo (range (+ 1 lo) hi))
          (list lo))))

;; Range step
(define (rangestep lo hi step)
  (if (< lo hi)
      (cons lo (rangestep (+ step lo) hi step))
      '()))

;; Fibonacci
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

;; Flatten (tail recursive)
(define (flatten l)
  (define (flatten-helper in acc)
    (if (null? in)
        acc
        (if (list? (car in))
            (flatten-helper (cdr in) (flatten-helper (car in) acc))
            (flatten-helper (cdr in) (cons (car in) acc)))))
  (reverse (flatten-helper l '())))


;; Scheme Basics My-Map: takes a function f and a list l and applies f to each elements of l; NB returns a list

(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map f (cdr l)))))

;; My-Map Named Let

(define (my-map-named-let f l)
  (let helper ((ff f)
               (ll l))
    (if (null? ll)
        '()
        (cons (ff (car ll)) (helper ff (cdr ll))))))

;; Check if a number is prime
;; conditional with when
;; Loop

;; Prime check

(define (prime? n)
  (cond [(<= n 3) #t]
        [(= 0 (modulo n 2)) #f]
        [else
         (let label ((i 3))
            (if (>= i n)
              #t
              (if (= 0 (modulo n i)) 
                  #f
                  (label (+ i 1)))))]))

;; Quick Sort with let*

(define (quick-sort s)
  (cond [(or (empty? s) (empty? (cdr s))) s]
        [else (let* ([p (car s)]
                           [s<  (filter (λ (x) (< x p)) (cdr s))]
                           [s>  (filter (λ (x) (> x p)) (cdr s))])
                (append (quick-sort s<) (list p) (quick-sort s>)))]))
            
;; Pack (put together in sublists successions of equal elements)
;; e.g., (1 1 1 2 3 3 4) -> ((1 1 1) (2) (3 3) (4))
(define (pack l)
  (define (pack-helper in acc sub)
    (cond ((null? in) (cons sub acc))
          ((null? sub) (pack-helper (cdr in) acc (cons (car in) sub)))
          ((eqv? (car in) (car sub)) (pack-helper (cdr in) acc (cons (car in) sub)))
          (else (pack-helper (cdr in) (cons sub acc) (list (car in))))))
  (reverse (pack-helper l '() '())))