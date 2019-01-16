#lang racket
; (string-from-strings '(1 "hello" ", " 2 "world")) => "hello, world"

(define (sfstl l)
  (define (helper l str)
    (if (null? l)
        str
        (let ((head (car l)))
              (helper (cdr l)
                      (if (string? head)
                          (string-append str head)
                          str)))))
  (helper l ""))

(define (sfs l)
  (if (null? l)
      ""
      (let ((head (car l)))
        (if (string? head)
            (string-append head (sfs (cdr l)))
            (sfs (cdr l))))))

(define (sfsho l)
  (foldr string-append "" (filter string? l)))