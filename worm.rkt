;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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


; MAPPINGS

; acceptable keystrokes include:
; up arrow -> makes the worm face upward
; down arrow -> makes the worm face downward
; right arrow -> makes the worm face right
; left arrow -> makes the worm face left
