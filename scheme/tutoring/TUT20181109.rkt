#lang racket


(struct player
  ( id (score #:mutable) (game #:mutable) (exit #:mutable)))

(define ric (player "Riccardo" -1 #f #f))

(struct game
  ( (players #:mutable)  (winners #:mutable)   (losers #:mutable)))

(define thegame (game '() '() '()))

(define (start-play p)
  (set-game-players! thegame (append (game-players thegame) (list p))))

(start-play ric)


(define (players)
  (map  player-id (game-players thegame)))

(define (active-games)
  (map  player-game (game-players thegame)))

(define clubs  (append (range 1 11) (list 10 10 10)))
(define hearts  (append (range 1 11) (list 10 10 10)))
(define diamonds (append (range 1 11) (list 10 10 10)))
(define spades (append (range 1 11) (list 10 10 10)))

(define deck (append (append (append clubs hearts) diamonds) spades))

(define (rand-bool x y)
  (let ((v (random 2)))
    (= 0 v)))

(define (shuffle)
  (set! deck (sort deck rand-bool)))


;)
;; SHUFFLE THE DECK
;; proposed algorithm: generate one random interger and get the 4 elements
;; from the vector above, add them to a new vector
;; NB: remove the element from the original vectors
