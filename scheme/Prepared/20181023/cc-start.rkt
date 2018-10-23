#lang racket
(define saved-cont #f) ; place to save k

(define (test-cont) (let ((x 0))
                      (call/cc
                       (lambda (k) ; k contains the continuation
                         (set! saved-cont k))) ; here is saved
                      ;; this *is* the continuation
                      (set! x (+ x 1)) (display x) (newline)))


(test-cont) ;; => 1
(saved-cont) ;; => 2
(define other-cont saved-cont)
(test-cont) ;; => 1 (here we reset saved-cont)
(test-cont) ;; => 1
(saved-cont) ;; => 2
(other-cont) ;; => 3



(define (right-now)
  (call/cc
   (λ (cc)
    (cc (display "ciao")))))


(define (test-cc)
 (display (call/cc (λ (escape)
             (display "before escaping\n")
             (escape  "escaping!!1!!\n" 1)
             (display "after escaping\n")))))


; "break" in a loop
(define (break-test)
  (call/cc (lambda (break)
             (for-each
              (lambda (i)
                (if (= (remainder i 2) 0)
                    (break)
                    (begin
                      (display i)
                      (newline))))
              '(1 2 3 4 5 6)))))

                    
; "continue" in a loop
(define (continue-test)
  (for-each
   (lambda (i)
     (call/cc (lambda (continue)
                (if (= (remainder i 2) 0)
                    (continue)
                    (begin
                      (display i)
                      (newline))))))
   '(1 2 3 4 5 6)))



