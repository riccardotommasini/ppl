#lang racket

(define-syntax ++
  (syntax-rules ()
   ((_ i)
    (begin
      (set! i (+ i 1))))))

(define (f+* i . rest)
  (if (null? rest)
      (list (+ i 1))
      (append (list (+ 1 i)) (apply f+* rest))))

(define-syntax +*
  (syntax-rules ()
    ((_ i) (set! i (add1 i)))
    ((_ i ...)(begin (set! i (add1 i)) ...))))

(define (fact n)
    (foldl (λ (i x) (* i x)) 1 (range 1 (add1 n))))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define choose-arg
  (λ (i . args)
    (if (zero? i)
        (car args)
    (apply choose-arg (- i 1) (cdr args)))))

;(time (choose-arg 1 (fact 5) (+ 1 2) (fib 42)))
;(time ((choose-arg 1 (λ () (fact 5)) (λ () (+ 1 2)) (λ () (fib 42)))))

(define-syntax cargs
  (syntax-rules ()
    ((_ i f) f)
    ((_ i f f1 ...)(if (zero? i) f (cargs (sub1 i) f1 ...)))))

;(my-for x in '(1 2 3) (display x) (newline))

(define-syntax my-for
  (syntax-rules (in)
    ((_ var in l body ...)
     (for-each (λ (var)
                 body ...) l))))


(display
 (call/cc (λ (cc)
            (display "I got here\n")
            (cc "This string is passed to the continuation\n")
            (display "I won't see this\n"))))


(define (cont-fact n)
  (letrec
      ((f (λ (n cc)
            (if (zero? n)
                (cc 1)
                (f (sub1 n)
                   (λ (a)
                     (cc (* n a))))))))
    (call/cc
     (λ (cc)
       (f n cc)))))


(define (break-test)
  (call/cc (λ (break)
             (for-each (λ (i)
                         (if (= i 42)
                             (break)
                             (begin
                               (display i)
                               (newline))))
                       '(1 2 3 42 5 6 7)))))

(define (continue-test)
  (for-each
   (λ (i)
     (call/cc (λ (continue)
                (if (= i 42)
                    (continue)
                    (begin
                      (display i)
                      (newline))))))
   '(1 2 3 42 4 5 6)))

(define (right-now)
  (call/cc
   (λ (cc)
     (cc cc))))

(define (go-when then)
  (then then))


      
                     