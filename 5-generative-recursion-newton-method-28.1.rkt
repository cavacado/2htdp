;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-generative-recursion-newton-method-28.1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 455
(define (poly x)
  (* (- x 2) (- x 4)))

(define epsilon 0.01)

(check-expect (slope poly 1) -4)

(define (slope fn r1)
  (* (/ 1 (* 2 epsilon)) (- (fn (+ r1 epsilon)) (fn (- r1 epsilon)))))

; [Number -> Number] Number -> Number
; interpretation: fn maps f and r1 to the root
; of tangent through (r1, (f r1))

(define (root-of-tangent fn r1)
  (- r1 (/ (fn r1) (slope fn r1))))

; [Number -> Number] Number -> Number
; finds a number r such that (f r) is small
; generative, repeatedly generate improved guesses
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) epsilon) r1]
    [else (newton f (root-of-tangent f r1))]))
    
;; ex 457
; Number Number -> Number
; interpretation: computes how many mths it takes
; to double a given amt with a fixed interest rate

(define RATE 0.05)
(define BASE 1000)

;(check-expect (double-amount BASE RATE) ...)

(define (double-amount int)
  (/ (log 2) (log (+ 1 int))))
    
