;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arbitary-design-by-composition) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; List-of-numbers -> List-of-numbers
; rearrange alon in desc order

(check-expect (sort> '()) '())
(check-expect (sort> (cons 12 (cons 20 (cons -5 '())))) (cons 20 (cons 12 (cons -5 '()))))
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon) (insert (first alon) (sort> (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(define (insert n alon)
  (cond
    [(empty? alon) (cons n '())]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))

(define-struct gp [name score])
; A GamePlayer is a structure:
; (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points

; List-of-gp -> List-of-gp
; sort by desc order

(check-expect (sort-gp (list (make-gp "John" 15) (make-gp "Tom" 20))) (list (make-gp "Tom" 20) (make-gp "John" 15)))

(define (sort-gp alogp)
  (cond
    [(empty? alogp) '()]
    [else (insert-gp (first alogp) (sort-gp (rest alogp)))]))

(define (insert-gp struct alogp)
  (cond
    [(empty? alogp) (cons struct '())]
    [else (if (compare-struct struct (first alogp))
              (cons struct alogp)
              (cons (first alogp) (insert-gp struct (rest alogp))))]))

(define (compare-struct s1 s2)
  (cond
    [(>= (gp-score s1) (gp-score s2)) #t]
    [else #f]))

(define-struct email [from date message])
; A Email Message is a structure:
; (make-email String Number String)
; interpretation (make-email f d m) represents
; text m, sent by f, d seconds after the beginning of time

(check-expect (sort-email (list (make-email "J" 1000 "h") (make-email "T" 2500 "k") (make-email "H" 1250 "i")))
              (list (make-email "T" 2500 "k") (make-email "H" 1250 "i") (make-email "J" 1000 "h")))

(define (sort-email aloe)
  (cond
    [(empty? aloe) '()]
    [else (insert-email (first aloe) (sort-email (rest aloe)))]))

(define (insert-email e aloe)
  (cond
    [(empty? aloe) (cons e '())]
    [else (if (compare-email e (first aloe))
              (cons e aloe)
              (cons (first aloe) (insert-email e (rest aloe))))]))

(define (compare-email e1 e2)
  (cond
    [(>= (email-date e1) (email-date e2)) #t]
    [else #f]))

;; v2

(check-expect (sort-email.v2 (list (make-email "J" 1000 "h") (make-email "T" 2500 "k") (make-email "H" 1250 "i")))
              (list (make-email "T" 2500 "k") (make-email "J" 1000 "h") (make-email "H" 1250 "i")))

(define (sort-email.v2 aloe)
  (cond
    [(empty? aloe) '()]
    [else (insert-email.v2 (first aloe) (sort-email.v2 (rest aloe)))]))

(define (insert-email.v2 e aloe)
  (cond
    [(empty? aloe) (cons e '())]
    [else (if (compare-email.v2 e (first aloe))
              (cons e aloe)
              (cons (first aloe) (insert-email.v2 e (rest aloe))))]))

(define (compare-email.v2 e1 e2)
  (cond
    [(string<? (email-from e1) (email-from e2)) #f]
    [else #t]))
              
;; ex 189

; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #f]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

(define (search-sorted n alon)
  (cond
    [(> n (first alon)) #f]
    [else (or (= (first alon) n) (search-sorted n (rest alon)))]))

(check-expect (search-sorted 4 (sort> (list 2 5 3 4 12 6))) #t)
(check-expect (search-sorted 3 (sort> (list 1 2 10 4 12 6))) #f)
