#lang racket

(define *handlers* (list))

(define (push-handler proc)
  (set! *handlers* (cons proc *handlers*)))

(define (pop-handler)
  (let ((h (car *handlers*)))
    (set! *handlers* (cdr *handlers*)) h))

(define (throw x)
  (if (pair? *handlers*)
      ((pop-handler) x)
      (apply error x)))


(define-syntax try
  (syntax-rules (catch)
    ((_ exp1 ...
        (catch what hand ...))
     (call/cc (λ (exit)
                ;: intall the handler
                (push-handler (λ (x)
                                (if (equal? x what)
                                    (exit
                                     (begin
                                       hand ...))
                                    (throw x))))
                (let ((res ;; evaluate the body
                       (begin exp1 ...)))
                  ;; ok: discard the handler
                  (pop-handler)
                  res))))))

(define (foo x) (display x) (newline) (throw "hello"))

(try
 (display "Before foo ")
 (newline)
 (foo "hi!")
 (display "After foo") ; unreached code
 (catch "hello"
   ; this is the handler block
   (display "I caught a throw.")
   (newline) #f))
                         