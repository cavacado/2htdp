;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arbitary-12.1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
(define DICTIONARY-LOCATION "/usr/share/dict/words")

(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

; A Letter is on of the following 1Strings:
; - "a"
; ...
; - "z"
; or, equivaently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; equivalent
; 1String List -> Number

; ex 195
(check-expect (starts-with# "a" (list "avvadark" "atrocious" "bad" "horrid" "non-efficient")) 2)

(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [(equal? l (first (explode (first d)))) (+ 1 (starts-with# l (rest d)))]
    [else (+ 0 (starts-with# l (rest d)))]))

; ex 196

; count-by-letter
; Dictionary -> List-of-numbers
; takes a dict and then counts the number of times the first letter appears in the word produces a list in the end

; times-in-word
; Letter String -> Number
; takes a string then counts the number of time the first letter appears in that string

;; did not read question properly...
(define (times-in-word l str)
  (cond
    [(empty? (explode str)) 0]
    [(equal? (first (explode str)) l) (+ 1 (times-in-word l (implode (rest (explode str)))))]
    [else (+ 0 (times-in-word l (implode (rest (explode str)))))]))

(check-expect (times-in-word "a" "abracadabra") 5)

(define-struct letter-count [letter number])

(define (count-by-letter lol d)
  (cond
    [(empty? lol) '()]
    [else (cons (make-letter-count (first lol) (starts-with# (first lol) d)) (count-by-letter (rest lol) d))]))



(check-expect (count-by-letter LETTERS (list "avvadark" "atrocious" "bad" "horrid" "non-efficient"))
              (list
 (make-letter-count "a" 2)
 (make-letter-count "b" 1)
 (make-letter-count "c" 0)
 (make-letter-count "d" 0)
 (make-letter-count "e" 0)
 (make-letter-count "f" 0)
 (make-letter-count "g" 0)
 (make-letter-count "h" 1)
 (make-letter-count "i" 0)
 (make-letter-count "j" 0)
 (make-letter-count "k" 0)
 (make-letter-count "l" 0)
 (make-letter-count "m" 0)
 (make-letter-count "n" 1)
 (make-letter-count "o" 0)
 (make-letter-count "p" 0)
 (make-letter-count "q" 0)
 (make-letter-count "r" 0)
 (make-letter-count "s" 0)
 (make-letter-count "t" 0)
 (make-letter-count "u" 0)
 (make-letter-count "v" 0)
 (make-letter-count "w" 0)
 (make-letter-count "x" 0)
 (make-letter-count "y" 0)
 (make-letter-count "z" 0)))

(define (sort> l)
  (cond
    [(empty? l) '()]
    [else
     (insert (first l) (sort> (rest l)))]))

(define (insert n l)
  (cond
    [(empty? l) (list n)]
    [else
     (cond
       [(>= n (first l)) (cons n l)]
       [else (cons (first l) (insert n (rest l)))])]))

(check-expect (sort> (list 3 4 5)) (list 5 4 3))

(define (sort-letter-count alolc)
  (cond
    [(empty? alolc) '()]
    [else (insert-letter-count (first alolc) (sort-letter-count (rest alolc)))]))

(define (insert-letter-count struct alolc)
  (cond
    [(empty? alolc) (cons struct '())]
    [else (if (compare-struct struct (first alolc))
              (cons struct alolc)
              (cons (first alolc) (insert-letter-count struct (rest alolc))))]))

(define (compare-struct s1 s2)
  (cond
    [(>= (letter-count-number s1) (letter-count-number s2)) #t]
    [else #f]))

(check-expect (sort-letter-count (list (make-letter-count "a" 4)
                                       (make-letter-count "b" 10)
                                       (make-letter-count "c" 20)))
                                 (list (make-letter-count "c" 20)
                                       (make-letter-count "b" 10)
                                       (make-letter-count "a" 4)))

(define (most-frequent lol d)
  (first (sort-letter-count (count-by-letter lol d))))

;to check pls uncomment
;(check-expect (most-frequent LETTERS DICTIONARY-AS-LIST) (make-letter-count "s" 22759))
  


