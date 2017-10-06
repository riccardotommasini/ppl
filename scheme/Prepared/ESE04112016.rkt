if#lang racket

;; Named Let Translations [Sum of squares]

;; Sum of squares tail recursive (from 28/10/2016 class)

(define (sum-of-squares-tail l)
  (define (sos l acc)
    (if (null? l)
        acc
        (sos (cdr l) (+ acc (* (car l) (car l))))))
    (sos l 0))

;; NON-Idiomatic translation

(define (sum-of-squares-low l)
  (define xl l)
  (define acc 0)
  (let loop () ;; "loop" label as in class code
    (if (null? xl)
        acc
        (begin
          (set! acc (+ acc (* (car xl) (car xl))))
          (set! xl (cdr xl))
          (loop)))))

;; Idiomatic translation (Named let is translated into a local recursive function
;; if tail recursive it becomes a single loop (Quote Slides)

(define (sum-of-squares-low-idiomatic l)
  (let loop ((xl l)
             (acc 0))
    (if (null? xl)
        acc
        (loop (cdr xl) (+ acc (* (car xl) (car xl)))))))


;; Argument Passing

; Argument-passing
(define (greet name)
  (string-append "Hello, " name))

; The previous definition is syntactic sugar for the following
(define lgreet (lambda (name) (string-append "Hello, " name)))

(define (optgreet name #:hi [hi "Hello"])
  (string-append hi ", " name))

(define (restgreet #:hi [hi "Hello"] . others)
  (if (null? others)
      ""
      (string-append hi ", " (car others) (apply restgreet #:hi "" (cdr others)))))

;;Vectors

;; A vector is generally mutable (list are not), but vectors written directly as expressions are immutable.
;; The provide constant time access

;; Handy functions

(vector->list (vector 1 2 3))
(list->vector (list 1 2 3))

;; Quick Sort

;; Helper function tha swaps two elements in the array
(define (vector-swap v i j)
  (let ([t (vector-ref v i)])
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

;; Helper function that splits the array in two parts given the pivot index

(define (par v left right p)
  (define pivot (vector-ref v p))
  (vector-swap v p right)
  (define border left)
  (for ([i (range left right)] ; for loop code that allows to use
        #:when (<= (vector-ref v i) pivot)) ; "WHEN" keyword representing a conditional call 
        (vector-swap v i border)
        (set! border (+ 1 border)))
  (vector-swap v border right)
  border)

;; Quick Sort code

(define (quick-sort-vec v)
  (define (sort left right)
    (when (< left right)
      (let* ([p (+ left (random (- right left)))]
             [p* (par v left right p)])
        (sort left (- p* 1))
        (sort (+ p* 1) right))))
  (sort 0 (- (vector-length v) 1)))
               
(define v (vector 4 10 3 2 1 47 32 42 33 123))


;; Structs 


;; Last year Trees

(struct leaf
  ((content #:mutable) empty))

(struct node leaf
  (left right))

(define (display-tree t)
  (if (not (node? t))
      (if (leaf-empty t)
          (display "---\n")
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

(define (create-empty)
  (leaf -1 #t))

(define (create-node v t1 t2)
  (node v #f t1 t2))

(define (create-leaf v)
  (node v #f (create-empty) (create-empty)))

;; Alternative implementation

;; Content
(struct cnt
  ((k #:mutable) ; key is mutable
   v)) ; value is immutable

;; Leaf
(struct leaf
  ((c #:mutable)))

;; Node
(struct node leaf
  (left
  right))

;; Examples
(define leafR (leaf (cnt 1 "Right")))
(define leafL (leaf (cnt 10 "Left")))
(define parent (node (cnt 32 "Node") leafL leafR))

;; Comparison functions

(define (=node n1 n2)
  (if (= (cnt-k (leaf-c n1)) (cnt-k (leaf-c n2)))
      #t
      #f))

(define (<node n1 n2)
  (if (< (cnt-k (leaf-c n1)) (cnt-k (leaf-c n2)))
      #t
      #f))

(define (>node n1 n2)
  (if (> (cnt-k (leaf-c n1)) (cnt-k (leaf-c n2)))
      #t
      #f))

;; Homework try to display only the node with odd content

;; Homework implement a new struct Heap (priority queue) using vectors of pairs (key . value)
;; Homework implement Dijkstra's algorithm using heap (check the complexity with time function)
