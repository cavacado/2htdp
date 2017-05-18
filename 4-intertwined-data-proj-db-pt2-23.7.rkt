;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-proj-db-pt2-23.7) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 404
; [[Any Any] -> Boolean] [List-of Any] [List-of Any] -> Boolean
; interpretation: consumes a fn with 2 values that produces a boolean;
; and two other lists of same length
; produces a boolean

(define lx1 '(1 2 3 4 5 6))
(define lx2 '(7 8 9 10 11 12))
(define (cus-fn v1 v2)
  (> v2 v1))

(check-expect (andmap2 cus-fn lx1 lx2) #t)

(define (andmap2 fn l1 l2)
  (local ((define (aux-lx fn l1 l2)
            (cond
              [(empty? l1) '()]
              [else
               (cons (fn (first l1) (first l2)) (aux-lx fn (rest l1) (rest l2)))])))
    (andmap (lambda (x)
              (not (false? x)))
            (aux-lx fn l1 l2))))