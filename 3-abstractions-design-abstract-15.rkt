;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 3-abstractions-design-abstract-15) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; ex 250
; Operator Number -> [List-of Number[
; tabulates the operator between n
; and 0 (incl.) in a list

(define (tabulate op n)
  (cond
    [(= n 0) (list (op 0))]
    [else
     (cons (op n) (tabulate op (sub1 n)))]))

; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; ex 251
; Operator [List-of Number] -> Number
; computes the operation of
; the numbers on l
(define (fold1 op l)
  (cond
    [(empty? l) (cond
                  [(or (equal? op *) (equal? op /)) 1]
                  [else 0])]
    [else (op (first l) (fold1 op (rest l)))]))

; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

(define (fold2 op l)
  (cond
    [(empty? l) (cond
                  [(equal? op *) 1]
                  [else emt])]
    [else (op (first l) (fold2 op (rest l)))]))

; ex 253
; [Number -> Boolean] >
; [Boolean String -> Boolean]
; [Number Number Number -> Number] +
; [Number -> [List-of Number]] range?
; [[List-of Number] -> Boolean] empty?

; ex 254

; sort-n
; [[List-of Number] [Number Number -> Boolean] -> [List-of Number]]
; sort-s
; [[List-of String] [String String -> Boolean] -> [List of String]]
; sort-abs
; [[List-of X] [X Y -> Z] -> [List of Y]]

; map-n
; [[List-of Number] [Number -> Number] -> [List-of-Number]]
; map-s
; [[List-of String] [String -> String] -> [List-of-String]]
; map-abs
; [[List-of X] [X -> Y] -> [List-of Y]]


