;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arbitary-large-data-part2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;sample ex
(define (contains-flatt? alon)
  (cond
    ((empty? alon) #false)
    ((cons? alon)
     (or (string=? (first alon) "Flatt")
      (contains-flatt? (rest alon))))))

;;tests
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '())))) #true)

;;ex 134
(define (contains? alos str)
  (cond
    ((empty? alos) #false)
    ((cons? alos)
     (cond
       ((string=? (first alos) str) #true)
       (else (contains? (rest alos) str))))))


(check-expect (contains? '() "samurai") #false)
(check-expect (contains? (cons "Find" '()) "samurai") #false)
(check-expect (contains? (cons "Flatt" '()) "samurai") #false)
(check-expect (contains? (cons "String" (cons "AnotherString" (cons "samurai" '()))) "samurai") #true)

(define (how-many alos)
  (cond
    ((empty? alos) 0)
    (else (+ (how-many (rest alos)) 1))))

(check-expect (how-many (cons "1" (cons "another string" (cons "alala" '())))) 3)

;;ex 138
(define (list-of-amounts alon)
  (cond
    ((empty? alon) 0)
    (else (+ (list-of-amounts (rest alon)) (first alon)))))

(check-expect (list-of-amounts (cons 1 (cons 2 (cons 3 (cons 4 '()))))) 10)

;;ex 139
(define (list-of-numbers alon)
  (cond
    ((empty? alon) #t)
    (else
     (cond
       ((number? (first alon)) (list-of-numbers (rest alon)))
       (else #f)
       ))))

(define (pos? alon)
  (list-of-numbers alon))

(check-expect (pos? (cons 1 (cons 2 (cons 3 (cons 4 '()))))) #t)
(check-expect (pos? (cons 1 (cons 'symbol(cons 2 (cons 3 (cons 4(cons "string" '()))))))) #f)

(define (checked-sum alon)
  (cond
    ((false? (list-of-numbers alon)) #f)
    (else (list-of-amounts alon))))

(check-expect (checked-sum (cons 1 (cons 2 (cons 3 (cons 4 '()))))) 10)
(check-expect (checked-sum (cons 1 (cons 'symbol(cons 2 (cons 3 (cons 4 '())))))) #f)

;; ex 140

(define (all-true alob)
  (cond
    ((empty? alob) #t)
    (else
     (cond
       ((boolean? (first alob)) (all-true (rest alob)))
       (else #f)))))

(check-expect (all-true (cons #t(cons #f(cons #f(cons #t '()))))) #t)
(check-expect (all-true (cons #t(cons #f(cons "string"(cons #f(cons #t '())))))) #f)

(define (one-true alob)
  (cond
    ((empty? alob) #f)
    (else
     (cond
       ((equal? (first alob) #t) #t)
       (else
        (one-true (rest alob)))))))

(check-expect (one-true (cons #f(cons #f(cons #f(cons #t(cons #f '())))))) #t)
(check-expect (one-true (cons #f(cons #f(cons #f(cons #f(cons #f '())))))) #f)
(check-expect (one-true (cons #t '())) #t)

;;ex 141
(define (cat alos)
  (cond
    ((empty? alos) "")
    (else (string-append (first alos) (cat (rest alos))))))

(check-expect (cat (cons "string"(cons "anothestring"(cons "a"(cons "b" '()))))) "stringanothestringab")

;;ex 142
(define (ill-sized? aloi n)
  (cond
    ((empty? aloi) #f)
    (else
     (cond
       ((and (image? (first aloi)) (= (image-width (first aloi)) n) (= (image-height (first aloi)) n)) (first aloi))
       (else (ill-sized? (rest aloi) n))))))

(check-expect (ill-sized? (cons (rectangle 20 40 "solid" "slateblue")(cons (square 40 "solid" "slateblue") '())) 40)
(square 40 "solid" "slateblue"))
(check-expect (ill-sized? (cons (square 40 "solid" "slateblue") '()) 40)
(square 40 "solid" "slateblue"))

(define (average alot)
  (/ (sum alot) (how-many alot)))

(define (sum alot)
  (cond
    ((empty? alot) 0)
    (else
     (+ (first alot) (sum (rest alot))))))

;; ex 143

(define (checked-average alot)
  (cond
    ((empty? alot) "empty list")
    (else (average alot))))

(check-expect (checked-average '()) "empty list")

(define (sum-ne-l ne-l)
  (cond
    ((empty? (rest ne-l)) (first ne-l))
    (else (+ (first ne-l) (sum-ne-l (rest ne-l))))))

(check-expect (sum-ne-l (cons 1 '())) 1)

(define (sorted>? nelt)
  (cond
    [(empty? (rest nelt)) #t]
    [(false? (rest nelt)) #f]
    [(cons? (rest nelt)) (and (> (first nelt) (first (rest nelt))) (sorted>? (rest nelt)))]))

(check-expect (sorted>? (cons 3(cons 2(cons 1 '())))) #t)
  