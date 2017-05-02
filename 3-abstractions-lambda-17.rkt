;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 3-abstractions-lambda-17) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; ex 280
(check-expect ((lambda (x y) (+ x (* x y))) 1 2) 3)

(check-expect ((lambda (x y)
                 (+ x
                    (local ((define z (* y y)))
                      (+ (* 3 z) (/ 1 x)))))
               1 2) 14)

(check-expect ((lambda (x y)
                 (+ x
                    ((lambda (z)
                       (+ (* 3 z) (/ 1 z)))
                     (* y y))))
               1 2) 13.25)

;; ex 281
(check-expect ((lambda (n)
                 (< n 10))
               14) #f)

(check-expect ((lambda (x y)
                 (number->string (* x y)))
               6 7) "42")

(define-struct IR [name price])

(check-expect ((lambda (x y)
                 (< x y))
               (IR-price (make-IR "doll" 50))
               (IR-price (make-IR "ball" 100))) #t)

(check-expect ((lambda (n)
                 (cond
                   [(even? n) 0]
                   [else 1]))
               14) 0)

(define DOT (circle 1 "solid" "red"))
(define MT (empty-scene 100 100))

((lambda (p img)
   (place-image DOT (posn-x p) (posn-y p) img))
 (make-posn 10 10) MT)

