;; MACRO

;;   Here are the rules for pattern matching:
;;
;;  - A constant pattern will only match against an EQUAL? constant.
;;    We'll exploit this later on.
;;
;;  - A symbol that is one of the `literals' can only match against the
;;    exact same symbol in the form, and then only if the macro user
;;    hasn't shadowed it.
;;
;;  - A symbol that is *not* one of the literals can match against *any*
;;    complete form.  (Forgetting this can lead to surprising bugs.)
;;
;;  - A proper list of patterns can only match against a list form of
;;    the same length and only if the subpatterns match.
;;
;;  - An improper list of patterns can only match against a list form of
;;    the same or greater length and only if the subpatterns match.  The
;;    `dotted tail' of the pattern will be matched against the remaining
;;    elements of the form.  It rarely makes sense to use anything but a
;;    symbol in the dotted tail of the pattern.
;;
;;  - The ... token is special and will be discussed a bit later.

;; Simple examples


(define-syntax ++
  (syntax-rules ()
    ((_ i)
     (let ((a 0))
       (set! i (+ 1 i))))))

(let ((i 0))
  (begin
    (display i)
    (newline)
    (++ i)
    (display i)
    (newline)))

(define-syntax **
  (syntax-rules ()
    ((_ i)
     (let ((a i))
       (set! i (* a i))))))

(let ((i 5))
  (begin
    (display i)
    (newline)
    (** i)
    (display i)
    (newline)))

(define (fun+* i . rest)
  (if (null? rest)
      (list (+ i 1))
      (append (list (+ 1 i)) (apply fun+* rest))))

(fun+* 1)
(fun+* 1 2 4)

(let ((i 0)) (++ i) (display i))

;; Fun +* a s MACRO
;

(define-syntax +*
  (syntax-rules ()
    ((_ i) (begin (set! i (+ 1 i))))
    ((_ i ...) (begin (set! i (+ 1 i)) ...))))

(let ((i 0) (j 2)) (+* i j) (display i) (newline) (display j))
    
; Macro to compute the max between two values
(define-syntax my-max
  (syntax-rules ()
    (( _ a b)
     (if (> a b) a b))))

;;WHY WRITING MACROS

(define (fact n)
    (foldl (λ (i x) (* i x)) 1 (range 1 (add1 n))))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [(= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

;; CHOOSE-ARG takes a number and some arguments. The whole expression should evaluate to the argument selected by the number argument. 
(define choose-arg
  (λ (i . args)
    (if (zero? i)
        (car args)
        (apply choose-arg (- i 1) (cdr args)))))

;; (time (choose-arg 1 (factorial 5) (+ 1 2) (fib 40))) 
;; Very slow because Scheme must fully evaluate all procedure arguments before applying a procedure
;; We can speed up using thunks (functional wrappers, i.e., lambdas)

;(time ((choose-arg 1 (λ () (fact 5)) (λ () (+ 1 2)) (λ () (fib 40)))))

;; UNREADABLE!

(define-syntax choose-arg-m
  (syntax-rules ()
    ((_ i f) f)
    ((_ i f f1 ...) (if (zero? i) f (choose-arg-m (sub1 i) f1 ...)))))

;; (time (choose-arg-m 1 (fact 5) (+ 1 2) (fib 40)))
;; EXPANSION
; (choose-arg-m 1 (fact 5) (+ 1 2) (fib 40)) =>
;    (if (zero? 1)
;      (fact 5)
;      (choose-arg-m (+ 1 2) (fib 40))
;
; (choose-arg-m (sub1 1) (+ 1 2) (fib 40)) =>
;    (if (zero? (sub1 1))
;        (+ 1 2)
;        (chose-arg-m (sub1 (sub1 1)) (fib 40)))
;              ... -> single function case


;; N-ary macros

;;Repeat Until

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

;; Ellipses ...
;; The ... operator modifies how the PREVIOUS form is interpreted by the macro language.
;; In a macro pattern, ellipses can only appear as the last token in a
;; LIST or VECTOR pattern