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

;-----------------------------------------------------------------

; NUMERICAL CONSTANTS

(define GRID-SIZE 20)
(define CELL-DIAMETER 40)
(define WORLD-SIZE (* GRID-SIZE CELL-DIAMETER))
(define CENTER (/ WORLD-SIZE 2))
(define TICK-INTERVAL 0.25)

;------------------------------------------------------------------

; GRAPHICAL CONSTANTS

; (define HEAD (circle CELL-DIAMETER "solid" "pink")) <--- work out more advanced render system for later
(define SEGMENT (circle (/ CELL-DIAMETER 2) "solid" "red"))
(define FOOD (circle (/ CELL-DIAMETER 2) "solid" "green"))
(define WORLD (empty-scene (* GRID-SIZE CELL-DIAMETER) (* GRID-SIZE CELL-DIAMETER)))

;--------------------------------------------------------------------

; STRUCTURES

(define-struct head (posn direction))
(define-struct food (posn))
(define-struct world (food head))

;-------------------------------------------------------------------

; INITIAL GAME STATE
(define INITIAL-STATE (make-world (make-food (make-posn 20 20))
                                  (make-head (make-posn CENTER CENTER) "down")))

;--------------------------------------------------------------------

; DATA DEFINITIONS

; A direction is one of the following
; - up
; - right
; - left
; - down

;--------------------------------------------------------------------

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
       [worm-dir (head-direction (world-head statein))])
    (cond
      [(string=? worm-dir "up") (make-world (world-food statein)
                                            (make-head (make-posn x (- y CELL-DIAMETER)) worm-dir))]
      [(string=? worm-dir "down") (make-world (world-food statein)
                                              (make-head (make-posn x (+ y CELL-DIAMETER)) worm-dir))]
      [(string=? worm-dir "left") (make-world (world-food statein)
                                              (make-head (make-posn (- x CELL-DIAMETER) y) worm-dir))]
      [(string=? worm-dir "right") (make-world (world-food statein)
                                               (make-head (make-posn (+ x CELL-DIAMETER) y) worm-dir))])))


; World state -> world state
; Determines what key has been pressed and changes the worm's direction accordingly.
(define (check-keys statein key)
  (let*
      ([x (posn-x (head-posn (world-head statein)))]
       [y (posn-y (head-posn (world-head statein)))]
       [foodin (world-food statein)])
    (cond
      [(key=? key "up") (make-world foodin
                                    (make-head (make-posn x y) "up"))]
      [(key=? key "down") (make-world foodin
                                      (make-head (make-posn x y) "down"))]
      [(key=? key "right") (make-world foodin
                                       (make-head (make-posn x y) "right"))]
      [(key=? key "left") (make-world foodin
                                      (make-head (make-posn x y) "left"))]
      [else statein])))


; World state -> world state
; Determines whether the worm has collided with the walls of the environment
(define (check-collision statein)
  (let*
      ([x (posn-x (head-posn (world-head statein)))]
       [y (posn-y (head-posn (world-head statein)))])
    (cond
      [(> x WORLD-SIZE) true]
      [(< x 0) true]
      [(> y WORLD-SIZE) true]
      [(< y 0) true]
      [else false])))


; Create the world
(big-bang INITIAL-STATE
          (on-tick move-worm TICK-INTERVAL)
          (on-key check-keys)
          (to-draw render-world)
          (stop-when check-collision))


; TO DO
; IMPLEMENT MULTIPLE SEGMENTS
; WORK ON RENDERING THEM
; WORK ON MOVING THEM (LIST APPROACH THAT MR. GRAHAM SUGGESTED)