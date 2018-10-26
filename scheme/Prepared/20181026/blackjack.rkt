#lang racket
;; BlackJack
;; A player consists of two elements
;;  1) A datastructure that contains the turn in hand (continuation), name and current score 
;;  2) a function/macro encapsulating the player behavior (why a macro, because I may want to standardize it as (play actions ... until condtions ... pass (yield) )


;; When a player reaches 21 set its status to winner
;; When a player goes beyond 21 throws a loosing exception
;; A player can yield the next turn
;; The status of the game correspond to several co-routines representing the playes
;; the game stops, i.e., it must be re-evaluated (but cards remain the same) when a player wins


;; Interesting, implements the split when a player has two or more cards can split his/her game and proceed as it if what two players