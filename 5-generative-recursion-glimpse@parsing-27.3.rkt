;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-generative-recursion-glimpse@parsing-27.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; A File is one of: 
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; interpretation represent the content of a file 
; "\n" is the newline character

; A Line is a [List-of 1String]

; File -> [List-of Line]
; converts a file into a list of lines

(check-expect (file->list-of-lines
                (list "a" "b" "c" "\n"
                      "d" "e" "\n"
                      "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))

; File -> Line
; interpretation: takes a file (list of lines)
; when it encounters a new line; break and form a list
; consisting of all elements before the new line character
(define (first-line lx)
  (cond
    [(empty? lx) '()]
    [(equal? "\n" (first lx)) '()]
    [else
     (cons (first lx) (first-line (rest lx)))]))

; File -> File
; interpretation: takes a file
; when it encounters a new line; break and
; return the rest of the elements in the file,
; discarding the first few elements

(define (remove-first-line lx)
  (cond
    [(empty? lx) '()]
    [(equal? "\n" (first lx)) (rest lx)]
    [else
     (remove-first-line (rest lx))]))

;; ex 453
; Line -> [List-of Token]
; takes a line and returns a list of tokens

(check-expect (tokenize (list "a" "b" "c" " " "d" "e" "f" " "))
                        (list "abc" "def"))
(check-expect (tokenize (list "/" 5 3 "a" "t" "t" "a" "c" "k"))
                        (list "/" 5 3 "attack"))

(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

(define (tokenize lx)
  (cond
    [(empty? lx) '()]
    [else
     (cond
       [(equal? (chunk lx) "")
        (cons (first lx) (tokenize (remove-chunk lx)))]
       [else
        (cons (chunk lx) (tokenize (remove-chunk lx)))])]))

(define (chunk lx)
  (cond
    [(empty? lx) ""]
    [(member? (first lx) LETTERS)
     (string-append (first lx) (chunk (rest lx)))]
    [(not (member? (first lx) LETTERS)) ""]))

(define (remove-chunk lx)
  (cond
    [(empty? lx) '()]
    [(not (member? (first lx) LETTERS)) (rest lx)]
    [else
     (remove-chunk (rest lx))]))
    