#lang racket

(define (right-now)
  (call/cc
   (λ (cc)
     (cc cc))))

(define (right-now2)
  (call/cc
   (λ (cc)
     (cc))))

(define (call/cc-test)
  (call/cc (λ (escape)
             (display "Before escaping\n")
             (escape "escaping\n")
             (display "After escaping\n"))))

;;Coroutines

(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue e)
  (set! *queue* (append *queue* (list e))))

(define (dequeue)
  (let ((e (car *queue*)))
    (set! *queue* (cdr *queue*))
    e))


(define (make-coroutine proc)
  (call/cc (λ (cc)
             (enqueue cc) ; save the state
             (proc)))) ; evaluate proc

(define (yield)
  (call/cc (λ (cc)
           (enqueue cc)
           ((dequeue)))))

(define (coroutine-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue))))


(define (do-something str reps)
  (λ ()
    (let loop ((i 0))
      (display str) (display " ") (display i)(newline)
      (yield)
      (if (< i reps)
          (loop (+ i 1))
          (coroutine-exit)))))
   
(define (coroutine-test)
  (begin
    (make-coroutine (do-something "Proc A\n" 5))
    (make-coroutine (do-something "Proc B\n" 3))
    (coroutine-exit)))

;; Choose

(define *paths* '())

(define (choose choices)
  (if (null? choices)
      (fail)
      (call/cc
       (λ (cc)
         (set! *paths* (cons (λ ()
                               (cc (choose (cdr choices))))
                              *paths*))
         (car choices)))))

(define fail #f)
(call/cc (λ (cc)
           (set! fail
                 (λ ()
                   (if (null? *paths*)
                        (cc '!!FAILURE!!)
                        (let ((option (car *paths*)))
                          (set! *paths* (cdr *paths*))
                          (option)))))))

(define (is-the-sum-of sum)
  (unless (and (>= sum 0) (<= sum 10))
    (error "Out of range" sum))
  (let ((x (choose (reverse (range 0 5))))
        (y (choose '(0 1 2 3 4 5))))
        (if (= sum (+ x y))
            (list x y)
            (fail))))

;; Higher oder functions

;; interval check

(define (int-check min max L)
  (foldl (λ (x xs)
           (if (and (> x min) (< x max))
               (append xs (list x))
               xs)) '() L))

(int-check 0 9 (range 0 11))

(define (^ base exp)
  (let loop ((n 0) (res 1))
    (if (< n exp)
        (loop (+ 1 n) (* base res))
        res)))

(define (conv2 base l)
  (foldr (λ (exp dig res) (+ res (* dig (^ base exp))))
         0 (reverse (range 0 (length l))) l))

  
(conv2 2 '(0))
(conv2 2 '(0 1))
(conv2 5 '(1 3 2))

(define (o f g)
  (if (and (procedure? f) (procedure? g))
      (λ (a . rest) (f (g a rest)))
      (error "Not a function")))

(define fg (o list (λ (x . rest) (+ 1 x))))
(fg 1)

(define (isort lst)
  (define (min lst)
    (foldl (λ (x y) (if (< x y) x y)) (car lst) (cdr lst)))
  (if (null? lst)
      '()
      (let ((m (min lst)))
        (cons m (isort (remove m lst))))))

(isort '(42 24 4 2 132 0))

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
