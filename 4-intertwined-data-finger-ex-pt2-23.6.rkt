;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-finger-ex-pt2-23.6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 394
; [List-of Number] [List-of Number] -> [List-of Number]
; interpretation: fn consumes 2 lists of numbers
; gives a sorted list of numbers containing all numbers from both list

(check-expect (merge '(1 2 3 4 5 6 7) '(2 4 6 8 10)) '(1 2 2 3 4 4 5 6 6 7 8 10))
(check-expect (merge '(5 6 7 8 9) '(1 2 3 4 5 6 7)) '(1 2 3 4 5 5 6 6 7 7 8 9))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(or (> (first l1) (first l2)) (= (first l1) (first l2))) 
     (cons (first l2) (merge l1 (rest l2)))]
    [(< (first l1) (first l2))
     (cons (first l1) (merge (rest l1) l2))]))

;; ex 395
; [List] Number -> [List]
; interpretation: fn consumes a list l and a natural num n
; produces the first n items from l
; or all of l if n > (length l)

(check-expect (take '(1 2 34 5 6 7) 3) '(1 2 34))
(check-expect (take '(1 2 3 4 5) 10) '(1 2 3 4 5))

(define (take lx num)
  (cond
    [(< (length lx) num) lx]
    [(equal? num 0) '()]
    [(not (equal? num 0))
     (cons (first lx) (take (rest lx) (sub1 num)))]))

;; ex 395 b
; [List] Number -> [List]
; interpretation: fn consumes a list l and a natural num n
; produces the list with first n items removed
; if n > (length l) produces '()

(check-expect (drop '(1 2 34 5 6 7) 3) '(5 6 7))
(check-expect (drop '(1 2 3 4 5) 10) '())

(define (drop lx num)
  (cond
    [(< (length lx) num) '()]
    [(equal? num 0) lx]
    [(not (equal? num 0))
     (drop (rest lx) (sub1 num))]))
