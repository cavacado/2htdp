;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 3-abstractions-17.5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A Shape is a funciton:
; [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p)
; produces #t if p is in of s, #f o.w

; Shape Posn -> Boolean
(define (inside? s p)
  (s p))

; Number Number Number -> Shape
; creates a representation for a circle of radius r
; located at (center-x, center-y)
(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

(check-expect
 (inside? (mk-circle 3 4 5) (make-posn 0 0)) #t)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn 0 9)) #f)
(check-expect
 (inside? (mk-circle 3 4 5) (make-posn -1 3)) #t)

(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p))) (sqr (- y (posn-y p))))))

; Number Number Number Number -> Shape
; represent a width by heigh rectangle whose
; upper-left corner is located at (ul-x, ul-y)

(check-expect (inside? (mk-rect 0 0 10 3)
                       (make-posn 0 0)) #t)
(check-expect (inside? (mk-rect 2 3 10 3)
                       (make-posn 4 5)) #t)
(check-expect (inside? (mk-rect 0 0 10 10)
                       (make-posn -1 -1)) #f)

(define (mk-rect ul-x ul-y width height)
  (lambda (p)
    (and (<= ul-x (posn-x p) (+ ul-x width))
         (<= ul-y (posn-y p) (+ ul-y height)))))

; Shape Shape -> Shape
; combines 2 shapes into 1
(define (mk-combination s1 s2)
  ; Posn -> Boolean
  (lambda (p)
    (or (inside? s1 p) (inside? s2 p))))

(define circle1 (mk-circle 3 4 5))
(define rectangle1 (mk-rect 0 3 10 3))
(define union1 (mk-combination circle1 rectangle1))

(check-expect (inside? union1 (make-posn 0 0)) #t)
(check-expect (inside? union1 (make-posn 0 9)) #f)
(check-expect (inside? union1 (make-posn -1 -3)) #f)

