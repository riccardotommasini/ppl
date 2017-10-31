#lang racket

(struct leaf
  (( content #:mutable) empty))

;; inheritance 
(struct node leaf
  (left right))


(define (display-leaf l)
  (if (leaf? l)
      (display (leaf-content l))
      (display "Not a leaf")))

;; Helper function
(define (create-empty)
  (leaf -1 #t))

(define (create-node v t1 t2)
  (node v #f t1 t2))

(define (display-tree t)
  (if (not (node? t))
      (if (leaf-empty t)
          (display "--\n")
          (begin 
                 (display "Leaf \n")
                 (display (leaf-content t))
                 (newline)))
      (begin
        (display "Node \n")
        (display (leaf-content t))
        (newline)
        (display-tree (node-left t))
        (display-tree (node-right t)))))

(define (create-leaf v)
  (node v #f (create-empty) (create-empty)))

(define l1 (create-leaf 10))
(define l2 (create-leaf 12))

(define n1 (create-node 20 l1 l2))

;;Playing with macros

;; (++ 1) => 2

(define-syntax ++
  (syntax-rules ()
    ((_ i)
     (let ((a 0))
       (set! i (+ 1 i))))))

;; (**

(define-syntax **
  (syntax-rules ()
    ((_ i)
     (let ((a i))
       (set! i (* a i))))))

;; Yes, custom syntax sounds interesting
;; but let's see a more practical reason.

(define (fact n)
  (foldl (λ (i x) (* i x)) 1 (range 1 (+ 1 n))))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 0]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define choose
         (λ (i . args)
           (if (zero? i)
               (car args)
               (apply choose (- i 1) (cdr args)))))

;; very inefficient, choose has to evaluate all the elements

(define-syntax choose-m
  (syntax-rules ()
    ((_ i f) f)
    ((_ i f f1 ...) ( if (zero? i)
                         f
                         (choose-m (sub1 i) f1 ...)))))

(define (right-now)
  (call/cc
   (λ (cc)
    (cc (display "ciao")))))


(define (test-cc)
 (display (call/cc (λ (escape)
             (display "before escaping\n")
             (escape  "escaping!!1!!\n" 1)
             (display "after escaping\n")))))


(define (break-test)
  (call/cc (λ (break)
             (for-each
              (λ (i)
                (if (= (remainder i 2) 0)
                    (display (break))
                    (display i)))
              '( 1 2 3 4 5)))))

                    
(define (continue-test)
       (for-each
              (λ (i)
                (call/cc (λ (continue)
                           (if (= (remainder i 2) 0)
                               (begin 
                                      (display "skip"))
                               (begin
                                 (display i)
                                 (newline))))))
  '(1 2 3 4 5)))



                