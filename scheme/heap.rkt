#lang racket

;; Heap Priority Queue with Vectors

(struct heap [size (length #:mutable) data pos])

(define (display-heap h)
  (begin
    (display "Length: ")
    (display (heap-length h))
   
    (display " Data: ")
    (display (heap-data h))
    (newline)))

(define (full-heap? h)
  (= (heap-length h) (heap-size h)))

(define (empty-heap? h)
  (zero? (heap-length h)))

(define (make-heap size)
  (heap size 0 (make-vector size -1) (make-vector size)))

(define (heap-ref h i)
  (vector-ref (heap-data h) i))

(define (heap-set! h i e)
  (vector-set! (heap-data h) i e))

(define (heap-leaf? h i)
  (= i (heap-length h)))

(define (key e)
  (car e))

(define (val e)
  (cdr e))

(define (heapify-up! h i)
  (if [<= i 0]
      h
      (let* ((j (quotient i 2)) ; / does not integer division Check racket numbers documentation
            (v (heap-ref h i))
            (p (heap-ref h j)))
        (if (> (key v) (key p))
            h
            (begin
              (heap-set! h i p)
              (heap-set! h j v)
              (heapify-up! h j))))))

(define (heap-insert! h e)
    (cond ([not (pair? e)] (error "Input is not a pair!\n"))
          ([full-heap? h] (error "Heap out of length!\n"))
           (else
            (begin
              (let ((i (vector-member -1 (heap-data h))))
                (heap-set! h i e)
                (set-heap-length! h (add1 (heap-length h)))
                (heapify-up! h i))))))


(define (heapify-down! h i)
  (cond ([not (pair? (heap-ref h i))] (error "Input is not a pair!\n"))
        ([empty-heap? h]  (error "Heap is empty, cannot delete!\n"))
        ([(heap-leaf? h i)] #f)
        (else
         (begin
          (let* ((switch (λ (r l) (if  [> (key (heap-ref h r)) (key (heap-ref h l))] r l)))
                 (v (heap-ref h i))
                 (r_index (* 2 (add1 i)))
                 (l_index (add1 (* 2 i)))
                 (j (switch r_index l_index))
                 (w (heap-ref h j)))
            (if (< (key w) (key v))
                (begin
                  (heap-set! h j w)
                  (heap-set! h i v)
                  (heapify-down! h j))
                 #f))))))
             
(define (heap-delete! h i)
  (let ((len (heap-length h))
        (rem (heap-ref h i)))
    (heap-set! h i -1)
    (set-heap-length! h (sub1 len))
    (if  [= (sub1 len) i]
         h
         (begin
            (heap-set! h i (heap-ref h (sub1 len)))
            (heap-set! h (sub1 len) -1)
            (let ((r_index (* 2 (add1 i)))
                  (l_index (add1 (* 2 i)))
                  (k (key (heap-ref h i))))
              (cond  ([< k (key (heap-ref h (quotient i 2)))] (heapify-up! h i))
                     ([and (< l_index len) (> k (key (heap-ref h l_index)))] (heapify-down! h i))
                     ([and (< r_index len) (> k (key (heap-ref h r_index)))] (heapify-down! h i))
                     (else h)))))))
                     
(define h (make-heap 5))
(display-heap (heap-insert! h (cons 9 "c")))
(display-heap (heap-insert! h (cons 5 "b")))
(display-heap (heap-insert! h (cons 21 "d")))
(display-heap (heap-insert! h (cons 42 "e")))
(display-heap (heap-insert! h (cons 0 "a")))
(display-heap (heap-delete! h 2))