;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-finger-ex-23.6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 393

; [Set] [Set] -> [Set]
; interpretation: fn consumes 2 sets
; returns the union of the 2 sets
; ie produces a set that contains the elements of both

(define set1 (list 1 (list 2 (list 3))))
(define set2 (list 2 (list 3 (list 4 (list 5)))))
(define set3 (list 3 (list 4)))

;(check-expect (union set1 set2)
;              (list 1 (list 2 (list 3 (list 2 (list 3 (list 4 (list 5))))))))

(define (union s1 s2)
  (cond
    [(empty? (rest s2)) (add-to-end s1 (first s2))]
    [else (union (add-to-end s1 (first s2)) (first (rest s2)))]))


(define (add-to-end set num)
  (cond
    [(empty? (rest set))
     (list (first set) (list num))]
    [else (list (first set) (add-to-end (first (rest set)) num))]))

;(check-expect (add-to-end set1 2)
;              (list 1 (list 2 (list 3 (list 2)))))

;; ex 393 b

; [Set] [Set] -> [Set]
; interpretation: fn consumes 2 sets
; returns the intersect of the 2 sets
; ie produces a set that contains elements that occur in both

(check-expect (intersect set1 set2)
              (list 2 (list 3)))

(define (aux-intersect s1 s2)
  (cond
    [(empty? (rest s2)) '()]
    [else
     (if
      (inside? s1 (first s2))
      (list (cons (first s2) (aux-intersect s1 (first (rest s2)))))
      (aux-intersect s1 (first (rest s2))))]))

(define (intersect s1 s2)
  (first (aux-intersect s1 s2)))

(define (inside? set num)
  (cond
    [(and (empty? (rest set)) (not (equal? (first set) num))) #f]
    [(equal? (first set) num) #t]
    [else (inside? (first (rest set)) num)]))


       