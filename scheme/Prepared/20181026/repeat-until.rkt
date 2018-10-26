#lang racket
;; N-ary macros

;; Fun +* a s MACRO
;

(define-syntax +*
  (syntax-rules ()
    ((_ i) (begin (set! i (+ 1 i))))
    ((_ i ...) (begin (set! i (+ 1 i)) ...))))

(let ((i 0) (j 2)) (+* i j) (display i) (newline) (display j))

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