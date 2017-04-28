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

; Lon Number -> Lon
; select those numbers on l
; that are below t
(define (small l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(< (first l) t)
        (cons (first l)
          (small
            (rest l) t))]
       [else
        (small
          (rest l) t)])]))

     
; Lon Number -> Lon
; select those numbers on l
; that are above t
(define (large l t)
  (cond
    [(empty? l) '()]
    [else
     (cond
       [(> (first l) t)
        (cons (first l)
          (large
            (rest l) t))]
       [else
        (large
          (rest l) t)])]))

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l) (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

(check-expect (extract < '() 5) (small '() 5))
(check-expect (extract < '(3) 5) (small '(3) 5))
(check-expect (extract < '(1 6 4) 5) (small '(1 6 4) 5))

; Lon Number -> Lon
(define (small-1 l t)
  (extract < l t))

; Lon Number -> Lon
(define (large-1 l t)
  (extract > l t))

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

; ex 237
(check-expect (squared>? 3 10) #f)
(check-expect (squared>? 4 10) #t)
(check-expect (squared>? 5 10) #t)

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
    
; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; ex 238
; Nelon -> Number
; abstraction of previous 2 functions
(define (abstractor op l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (if (op (first l) (abstractor op (rest l)))
         (first l) (abstractor op (rest l)))]))

(check-expect (abstractor < (list 1 3 5 0)) 0)

(define (inf-1 l)
  (abstractor < l))

(define (sup-1 l)
  (abstractor > l))

;(check-expect (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
;(check-expect (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)

;(check-expect (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 1)
;(check-expect (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 25)

(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (min (first l) (inf.v2 (rest l)))]))

(define (sup.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (max (first l) (sup.v2 (rest l)))]))

(define (abstractor-2 op l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (op (first l) (abstractor-2 op (rest l)))]))

(define (inf-2 l)
  (abstractor-2 min l))

(define (sup-2 l)
  (abstractor-2 max l))

(check-expect (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(check-expect (sup-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)) 25)

(check-expect (inf-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 1)
(check-expect (sup-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)) 25)
 