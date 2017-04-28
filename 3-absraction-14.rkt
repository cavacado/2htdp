;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 3-absraction-14) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #f]
    [else (or (string=? (first l) s)
              (contains? (rest l)))]))

; ex 235
(define (contains-atom? l)
  (contains? "atom" l))

(define (contains-basic? l)
  (contains? "baisc" l))

(define (contains-zoo? l)
  (contains? "zoo" l))

; ex 236 a
; Lon -> Lon
; add 1 to each item on l
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else (cons (add1 (first l)) (add1* (rest l)))]))

(check-expect (add1* (list 1 2 3 4 5)) (list 2 3 4 5 6))

; ex 236 b
; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) 5) (plus5 (rest l)))]))

(check-expect (plus5 (list 1 2 3 4 5)) (list 6 7 8 9 10))

; ex 236 c
; Lon -> Lon
; adds n to each item on l
(define (addn n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (addn n (rest l)))]))

(check-expect (addn 1 (list 1 2 3 4 5)) (list 2 3 4 5 6))
(check-expect (addn 5 (list 1 2 3 4 5)) (list 6 7 8 9 10))

(check-expect (addn -2 (list 5 6 7 8 9)) (list 3 4 5 6 7))

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l) (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

(check-expect (extract < '() 5) (small '() 5))
(check-expect (extract < '(3) 5