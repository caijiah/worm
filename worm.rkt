;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Ian Kampine
; Form VIIB
; CPD II

; ----------------
; | WORM PROJECT |
; ----------------

; REQUIRES...

(require 2htdp/universe)
(require 2htdp/image)


; NUMERICAL CONSTANTS

(define GRID-SIZE 10)
(define CELL-DIAMETER 10)
(define WORLD-SIZE (* GRID-SIZE CELL-DIAMETER))


; GRAPHICAL CONSTANTS

; (define HEAD (circle CELL-DIAMETER "solid" "pink")) <--- work out more advanced render system for later
(define SEGMENT (circle CELL-DIAMETER "solid" "red"))
(define FOOD (circle CELL-DIAMETER "solid" "green"))
(define WORLD (empty-scene (* GRID-SIZE CELL-DIAMETER) (* GRID-SIZE CELL-DIAMETER)))


; STRUCTURES

(define-struct head (posn direction))
(define-struct food (posn))
(define-struct world (food head))


; DATA DEFINITIONS

; A direction is one of the following
; - up
; - right
; - left
; - down


; FUNCTIONS

; World state -> image
; renders an image containing the head from a given world state
; TODO: Render food, render additional segments
(define (render-world statein)
  (let
    ([x (posn-x (head-posn (world-head statein)))]
     [y (posn-y (head-posn (world-head statein)))])
    (place-image SEGMENT x y WORLD)))

; World state -> world state
; moves the worm one cell in the direction it is facing
(define (move-worm statein)
  (let*
      ([x (posn-x (head-posn (world-head statein)))]
       [y (posn-y (head-posn (world-head statein)))]
       [worm-dir (head-direction (world-head))])
    (cond
      [(string=? worm-dir "up") (make-world (world-food statein)
                                            (make-head (make-posn x (- y CELL-DIAMETER)) worm-dir))]
      [(string=? worm-dir "down") (make-world (world-food statein)
                                              (make-head (make-posn x (+ Y CELL-DIAMETER)) worm-dir))]
      [(string=? worm-dir "left") (make-world (world-food statein)
                                              (make-head (make-posn (- x CELL-DIAMETER) y) worm-dir))]
      [(string=? worm-dir "right") (make-world (world-food statein)
                                               (make-head (make-posn (+ x CELL-DIAMETER) y) worm-dir))])))
