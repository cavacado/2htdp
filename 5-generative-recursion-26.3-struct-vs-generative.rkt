;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-generative-recursion-26.3-struct-vs-generative) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 437
(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special (rest P)))]))

(define (solve lx)
  1)

(define (combine-solutions lx n)
  (cond
    [(empty? lx) 0]
    [(number? (first lx)) (combine-solutions (rest lx) n)]
    [else
     (+ 1 (combine-solutions (rest lx) n))]))

; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)

(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

;; ex 438
; greatest-divisor first finds the min of the 2 numbers provided
; it then sets it to i
; then it compares the remainder of both numbers when they are
; divided by i
; if the remainders are fully divisible by i; this means that
; there is a shared common divider
; and since a greatest divisor cannot be more than the min of the 2 numbers
; and because the fn is sub1 with each recursion, therefore
; the greatest common divisor would be found

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S))
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

(define (smallers lon n)
  (filter (lambda (x)
            (if (<= x n)
                #t
                #f))
          lon))

(define (largers lon n)
  (filter (lambda (x)
            (if (> x n)
                #t
                #f))
          lon))

(define (quick-sort< alon)
  (cond
    ;; to optimise the fn, saves around 60 odd steps
    ;[(< (length alon) 2) (sort< alon)]
    [(empty? alon) '()]
    [else
     (local ((define pivot (first alon)))
       (append (quick-sort< (smallers (rest alon) pivot))
               (list pivot)
               (quick-sort< (largers (rest alon) pivot))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(define create-tests
  (build-list 999 (lambda (x)
                             (random 999))))

; roughly after the length of the list goes above 15, that is when quick sort
; is faster than regular sort

(define (clever-sort< lx)
  (cond
    [(< (length lx) 15) (sort< lx)]
    [else
     (quick-sort< lx)]))

(time (sort< create-tests))
(time (quick-sort< create-tests))
(time (clever-sort< create-tests))

;; ex 443
; hard to terminate??