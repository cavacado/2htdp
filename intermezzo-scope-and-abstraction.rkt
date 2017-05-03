;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo-scope-and-abstraction) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))
 
(define (p2 x)
  (+ (* 55 x) (+ x 11)))
 
(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))

(define (insertion-sort alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else
               (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (add an (rest alon)))])])))
    (sort alon)))

;(define (sort alon)
;  (local ((define (sort alon)
;            (cond
;              [(empty? alon) '()]
;              [else
;               (add (first alon) (sort (rest alon)))]))
;          (define (add an alon)
;            (cond
;              [(empty? alon) (list an)]
;              [else
;                (cond
;                  [(> an (first alon)) (cons an alon)]
;                  [else (cons (first alon)
;                              (add an (rest alon)))])])))
;    (sort alon)))

; [List-of X] -> [List-of [List N X]]
; pair each item in lx with its index

(check-expect (enumerate '(a b c)) '((1 a) (2 b) (3 c)))

(define (enumerate l)
  (for/list ([i (length l)] [j l])
    (list (+ i 1) j)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generate all pairs from the lists provided

(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))

(define (cross l1 l2)
  (for*/list ([i l1] [j l2])
    (list i j)))

; N -> sequence?
; construct the infinite sequence of natural numbers, 
; starting from n
;(define (in-naturals n) ...)
 
; N N N -> sequence?
; construct the following finite sequence of natural numbers:
;   start
;   (+ start step)
;   (+ start step step)
;   ...
;  until the number exceeds end
;(define (in-range start end step) ...)

(define (enumerate.v2 lx)
  (for/list ([item lx] [ith (in-naturals 1)])
    (list ith item)))

; N -> Number
; add the even numbers between 0 and n (exclusive)
(check-expect (sum-events 2) 0)
(check-expect (sum-events 4) 2)
(define (sum-events n)
  (for/sum ([i (in-range 0 n 2)]) i))

;; ex 306

(check-expect (build-list-norm 5) '(0 1 2 3 4))
(define (build-list-norm n)
  (for/list ([i n])
    i))

(check-expect (build-list-norm-plus-1 5) '(1 2 3 4 5))
(define (build-list-norm-plus-1 n)
  (for/list ([i n])
    (add1 i)))

(check-expect (build-list-1-over 5) '(1/5 2/5 3/5 4/5 5/5))
(define (build-list-1-over n)
  (for/list ([i n])
    (/ (add1 i) n)))

(check-expect (build-list-even-for 5) '(0 2 4 6 8))
(define (build-list-even-for n)
  (for/list ([i n])
    (* 2 i)))

; A [Non-empty-list X] is one of:
; - (cons X '())
; - (cons X [Non-empty-list X])

; [Non-empty-list X] -> X
; retrieve the last item of ne-l
(check-expect (last-item '(a b c)) 'c)
(check-error (last-item'()))

(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))

(define-struct layer [color doll])
; An RD (short for Russian doll) is one of:
; - "doll"
; - (make-layer String RD)

; layer -> Number
; measures the depth of layers of the RD

(check-expect (depth (make-layer "red" (make-layer "yellow" (make-layer "blue" "doll")))) 3)
(define (depth a-doll)
  (match a-doll
    ["doll" 0]
    [(layer c inside) (+ (depth inside) 1)]))

;; ex 308
(define-struct phone-record [area-code num1 num2])

; [List-of phone-record] -> [List-of phone-record]
; takes an existing list of phone records and replaces the area code with 281 from 713

(check-expect (replace (list (make-phone-record 713 284 5553) (make-phone-record 222 255 8888) (make-phone-record 713 554 0988)))
              (list (make-phone-record 281 284 5553) (make-phone-record 222 255 8888) (make-phone-record 281 554 0988)))

(define (replace lor)
  (map (lambda (x)
         (match x
           [(phone-record 713 num1 num2) (make-phone-record 281 num1 num2)]
           [(phone-record area-code num1 num2) (make-phone-record area-code num1 num2)]))
       lor))

;; ex 309
; [List-of [List-of String]] -> [List-of Number]
; determines the number of words on each line

(check-expect (words-on-line '(("word1" "word2") ("word3" "word4") ("word5" "word6"))) '(10 10 10))

(define (words-on-line lls)
  (map count lls))

(define (count los)
  (match los
    [(cons fst '()) (string-length fst)]
    [(cons fst rst) (+ (string-length fst) (count rst))]))


