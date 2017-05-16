;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-finger-ex-pt5-26.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 398
; [List-of Number] [List-of Number] -> [List-of Number]
; interpretation: consumes 2 equally long lists
; produces a list of numbers

(check-expect (value '(1 2 3) '(1 2 3)) '(1 4 9))
(check-expect (value '(5 6 7) '(1 2 3)) '(5 12 21))
(check-expect (value '(10 20 30) '(2 4 6)) '(20 80 180))

(define (value l1 l2)
  (cond
    [(empty? l1) '()]
    [else
     (cons (* (first l1) (first l2)) (value (rest l1) (rest l2)))]))
         
