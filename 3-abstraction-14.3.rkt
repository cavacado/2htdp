;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 3-abstraction-14.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; a parametric data definition
; A [List-of ITEM] is one of:
; - '()
; - (cons ITEM [List-of ITEM])

; ex 239
; A [List X Y] is a structure:
;   (cons X (cons Y '())

; A [List Number Number is a structure:
;   (cons Number (cons Number '())
; eg (cons 5 (cons 6 '()))

; A [List Number 1String is a structure:
;   (cons Number (cons 1String '())
; eg (cons 5 (cons "a" '()))

; A [List String Boolean is a structure:
;   (cons String (cons Boolean '())
; eg (cons "astring" (cons #f '()))

;  ex 240
; A LStr is one of:
; - String
; - (make-layer LStr)
; eg (make-layer "layer 3" (make-layer "layer 2" (make-layer "layer 1")))

; A LNum is one of:
; - Number
; - (make-layer LNum)

(define-struct layer [stuff])

; ex 242
; A [Maybe X] is one of:
; - #f
; - X

; String [List-of String] -> [Maybe [List-og String]]
; returns the remainder of los starting with s
; #f o.w
;(check-expect (occurs "a" (list "b" "a" "d" "e"))
              ;(list "d" "e"))
;(check-expect (occurs "a" (list "b" "c" "d")) #f)

(define (occurs s los)
  (cond
    [(empty? los) #f]
    [else
     (cond
       [(equal? s (first los)) (rest los)]
       [else (occurs s (rest los))])]))

; ex 243

; (define (f x) x)
; (list f)
; f
; (cons f (cons 10 (cons 10 '()))

; ex 244
; (define (f x) (x 10))
; this is legal because by passing in a function in x; the expression would evaluate

; (define (f x) (x f))
; this is legal because by passing in a function, the expression would evauluate to a function

; (define (f x y) (x 'a y 'b))
; this is legal because by passing a function into x, the expression would require 3 arguments to evaluate

; ex 245
(define (function=at-1.2-3-and-5.775? f1 f2)
  (cond
    [(and (equal? (f1 1.2) (f2 1.2))
          (equal? (f1 3) (f2 3))
          (equal? (f1 5.775) (f2 5.775))) #t]
    [else #f]))

(define (f n) 0)
(define (g n) (- n n))

;(check-expect (function=at-1.2-3-and-5.775? f g) #t)

; not possible because test case can be so varied i think

; from 14.2 extract function
(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l) (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

;(check-expect (extract < '() 5) '())
;(check-expect (extract < (cons 4 '()) 5) (cons 4 '()))

;(check-expect (extract < (cons 6 (cons 4 '())) 5) (extract < (cons 4 '()) 5))

