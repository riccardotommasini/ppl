#lang racket
(define-syntax list-of-aux
  (syntax-rules (in is)
    (sub (list-of-aux expr acc)
     #'(cons expr acc))

    (sub (list-of-aux expr acc (var in lst) rest ...)
     #'(let loop ((ls lst))
         (if (null? ls) acc
             (let+ (var (car ls))
               (list-of-aux expr (loop (cdr ls)) rest ...)))))

    (sub (list-of-aux expr acc (var is exp) rest ...)
     #'(let+ (var exp) (list-of-aux expr acc rest ...)))

    (sub (list-of-aux expr acc pred? rest ...)
     #'(if pred? (list-of-aux expr acc rest ...) acc))
  ))

(def-syntax (list-of expr rest ...)
  #'(list-of-aux expr '() rest ...))