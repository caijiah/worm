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

(define-struct segment (posn direction))
(define-struct food (posn))
(define-struct world (food worm))

;-------------------------------------------------------------------

; INITIAL GAME STATE
(define INITIAL-STATE (make-world (make-food (make-posn 20 20))
                                  (list (make-segment (make-posn CENTER CENTER) "down"))))

; TEST GAME STATES
(define TEST-STATE-1 (make-world (make-food (make-posn 20 20)) 
                                               (list (make-segment (make-posn CENTER CENTER) "down") 
                                                     (make-segment (make-posn CENTER (+ CELL-DIAMETER CENTER)) "down")
                                                     (make-segment (make-posn CENTER (+ (* CELL-DIAMETER 2) CENTER)) "down"))))

;--------------------------------------------------------------------

; DATA DEFINITIONS

; A direction is one of the following
; - up
; - right
; - left
; - down

; A worm is one of the following:
; - (cons segment empty)
; - (cons segment worm)

;--------------------------------------------------------------------

; FUNCTIONS


; segment, image -> image
; renders an image containing the segment from a given segment and the
; rendered world image
; TODO: Render food
(define (render-segment segin worldin)
  (let
    ([x (posn-x (segment-posn segin))]
     [y (posn-y (segment-posn segin))])
    (place-image SEGMENT x y worldin)))


; worm -> image
; renders a world containing the worm from a given worm
(define (render-worm wormin)
    (cond
      [(empty? (rest wormin)) (render-segment (first wormin) WORLD)]
      [else (render-segment (first wormin)
                            (render-worm (rest wormin)))]))


; world state -> image
; renders the whole world from a given world state
(define (render-world worldin)
  (let*
      ([worm (world-worm worldin)]
       [food (world-food worldin)])
    (render-worm worm)))


; list -> list
; Returns a list without the last element
(define (worm-ebl wormin)
  (reverse (rest (reverse wormin))))

; testing worm-ebl
(check-expect (worm-ebl (list 1 2 3 4)) (list 1 2 3))


; worm -> segment
; Returns the last segment of a worm
(define (worm-last wormin)
  (cond
    [(empty? (rest wormin)) wormin]
    [else (worm-last (rest wormin))]))


; worm -> worm
; update's the worm's position within the world
(define (update-worm wormin)
  (cond
    [(empty? (rest wormin)) (list (update-head (first wormin)))] 
    [else (let*
      ([worm-head (first wormin)]
       [worm-tail (rest wormin)]
       [worm-middle (worm-ebl worm-tail)])
            (cons (update-head worm-head) worm-middle))]))
  

; world -> world
; update the world
(define (update-world worldin)
  (let*
    ([worm (world-worm worldin)]
     [food (world-food worldin)])
  (make-world food (update-worm worm))))


; segment -> segment
; updates the head segment's position
(define (update-head segin)
  (let*
      ([x (posn-x (segment-posn segin))]
       [y (posn-y (segment-posn segin))]
       [segment-dir (segment-direction segin)])
    (cond
      [(string=? segment-dir "up") (make-segment (make-posn x (- y CELL-DIAMETER)) segment-dir)]
      [(string=? segment-dir "down") (make-segment (make-posn x (+ y CELL-DIAMETER)) segment-dir)]
      [(string=? segment-dir "left") (make-segment (make-posn (- x CELL-DIAMETER) y) segment-dir)]
      [(string=? segment-dir "right") (make-segment (make-posn (+ x CELL-DIAMETER) y) segment-dir)])))


; World state -> world state
; Determines what key has been pressed and changes the worm's direction accordingly.
;(define (check-keys statein key)
;  (let*
;      ([x (posn-x (head-posn (world-head statein)))]
;       [y (posn-y (head-posn (world-head statein)))]
;       [foodin (world-food statein)])
;    (cond
;      [(key=? key "up") (make-world foodin
;                                    (make-head (make-posn x y) "up"))]
;      [(key=? key "down") (make-world foodin
;                                      (make-head (make-posn x y) "down"))]
;      [(key=? key "right") (make-world foodin
;                                       (make-head (make-posn x y) "right"))]
;      [(key=? key "left") (make-world foodin
;                                      (make-head (make-posn x y) "left"))]
;      [else statein])))


; World state -> world state
; Determines whether the worm has collided with the walls of the environment
;(define (check-collision statein)
;  (let*
;      ([x (posn-x (head-posn (world-head statein)))]
;       [y (posn-y (head-posn (world-head statein)))])
;    (cond
;      [(> x WORLD-SIZE) true]
;      [(< x 0) true]
;      [(> y WORLD-SIZE) true]
;      [(< y 0) true]
;      [else false])))


; Create the world
(big-bang TEST-STATE-1
          (on-tick update-world TICK-INTERVAL)
;          (on-key check-keys)
          (to-draw render-world))
;          (stop-when check-collision))


; TO DO (IN THIS ORDER)
; GET MULTIPLE SEGMENT MOVING WORKING
; GET MULTIPLE SEGMENT STEERING WORKING
; GET COLLISION DETECTION WORKING
; GET ϟƘƦƖןןΣ✘
; ALSO USE GITHUB SO YOU DON'T END UP REDOING EVERYTHING AGAIN