;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-forests-19.2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define-struct child [father mother name date eyes])
(define-struct no-parent [])

; A FT (short for family tree) is one of:
; - (make-no-parent) [replaced with NP]
; - (make-child FT FT String N String)

; A Child is a structure:
; (make-child Child Child String N String)

;(define Adam
;  (make-child Carl Bettina "Adam" 1950 "hazel"))

;(make-child (make-no-parent)
;            (make-no-parent)
;            "Bettina" 1926 "green")

(define NP (make-no-parent))

;(make-child (make-child NP NP "Carl" 1926 "green")
;            (make-child NP NP "Bettina" 1926 "green")
;            "Adam"
;            1950
;            "hazel")

; Oldest Generation
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Generation
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field

(check-expect (blue-eyed-child? Carl) #f)
(check-expect (blue-eyed-child? Gustav) #t)

(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #f]
    [else (or (string=? (child-eyes a-ftree) "blue")
              (blue-eyed-child? (child-father a-ftree))
              (blue-eyed-child? (child-mother a-ftree)))]))
;; ex 310 
; FT -> Number
; counts the number of child-structure in the tree

(define (count-persons a-ftree)
  (cond
    [(no-parent? a-ftree) 0]
    [else
     (cond
       [(string? (child-name a-ftree)) (+ 1
                                          (count-persons (child-father a-ftree))
                                          (count-persons (child-mother a-ftree)))]
       [else 0])]))
       
(define (total-age a-ftree yr)
  (cond
    [(no-parent? a-ftree) 0]
    [else
     (cond
       [(number? (child-date a-ftree)) (+ (- yr (child-date a-ftree))
                                          (total-age (child-father a-ftree) yr)
                                          (total-age (child-mother a-ftree) yr))])]))

(define (average-age a-ftree yr)
  (/ (total-age a-ftree yr) (count-persons a-ftree)))


(define (eye-colors a-ftree)
  (cond
    [(no-parent? a-ftree) '()]
    [else
     (cond
       [(string? (child-eyes a-ftree)) (append (list (child-eyes a-ftree))
                                               (eye-colors (child-father a-ftree))
                                               (eye-colors (child-mother a-ftree)))])]))

;; ex 313
; FT -> Boolean
(define (blue-eyed-ancestor? a-ftree)
  (cond
    [(no-parent? a-ftree) #f]
    [else
     (cond
       [(or (string=? "blue" (child-eyes (child-father a-ftree)))
            (string=? "blue" (child-eyes (child-mother a-ftree)))) #t]
       [else (or (blue-eyed-ancestor? (child-father a-ftree))
                 (blue-eyed-ancestor? (child-mother a-ftree)))])]))
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; start of 19.2

; A FF (short for family forest) is one of:
; - '()
; - (cons FT FF)
; interpretation a family forest represents several
; families (say a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; FF -> Boolean
; does the forest contain any child with "blue" eyes

(check-expect (blue-eyed-child-in-forest? ff1) #f)
(check-expect (blue-eyed-child-in-forest? ff2) #t)
(check-expect (blue-eyed-child-in-forest? ff3) #t)

(define (blue-eyed-child-in-forest? a-forest)
  (cond
    [(empty? a-forest) #f]
    [else
     (or (blue-eyed-child? (first a-forest))
         (blue-eyed-child-in-forest? (rest a-forest)))]))

;; ex 314
(check-expect (blue-eyed-child-in-forest?.v2 ff1) #f)
(check-expect (blue-eyed-child-in-forest?.v2 ff2) #t)
(check-expect (blue-eyed-child-in-forest?.v2 ff3) #t)

(define (blue-eyed-child-in-forest?.v2 a-forest)
  (ormap (lambda (x)
         (blue-eyed-child? x))
         a-forest))

;; ex 315

; FF -> Number
; interpretation; takes a family forest (which is a [List-of Families])
; and calculates the average age of the family forest

(check-expect (average-age-ff ff1 2017) 91)
(check-expect (average-age-ff ff2 2017) 71.25)
(check-expect (average-age-ff ff3 2017) 75.2)

(define (total-age-ff a-forest yr)
  (foldr + 0 (map (lambda (x)
                    (total-age x yr))
                  a-forest)))

(define (total-count-ff a-forest)
  (foldr + 0 (map (lambda (x)
                    (count-persons x))
                  a-forest)))

(define (average-age-ff a-forest yr)
  (/ (total-age-ff a-forest yr) (total-count-ff a-forest)))