#lang racket
Step four: write a function to swap vector elements.

(define (vector-swap vec i j)
  (let ([t (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j t)))
Step five: write a function to move elements that are less/greater than pivot to the left/right from said pivot.

(define (part vec left right p)
  (define pivot (vector-ref vec p))
  (vector-swap vec p right)            ; move pivot to end
  (define border left)
  (for ([i (range left right)]         ; (range left right) is the same
        #:when (<= (vector-ref vec i)  ;   as left..right-1
                   pivot))
    (vector-swap vec i border)
    (set! border (add1 border)))
  (vector-swap vec border right)       ; move pivot back
  border)                              ; return position of pivot
Step six: write the goddamned Quicksort.

(define (qsort2 vec)
  (define (sort left right)             ; subarray sort
    (when (< left right)                ; don't sort arrays of one element
      (let* ([p  (+ left (random (- right left)))]
             [p* (part vec left right p)])
        (sort left (sub1 p*))           ; sorting lesser elements
        (sort (add1 p*) right))))       ; sorting greater elements
  (sort 0 (sub1 (vector-length vec))))
