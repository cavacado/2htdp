;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-generative-recursion-gaussian-elimination-28.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; An SOE is a non-empty Matrix.
; constraint for  (list r1... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear eqns

; An Equation is a [List-of Number].
; constraint an Equation contains at least 2 numbers.
; interpretation if (list a1 ... an b) is an Equation,
; a1, ... , an are the left-hand side variable coefficents
; and b is the right hand side

; A Solution is a [List-of Number]

(define M ; an SOE
  (list (list 2 2 3 10); an Equation
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))

(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

;; ex 462

; Equation Solution -> Number
; interpretation: consumes lhs of Equation and Solution
; calculates the value of the lhs when numbers from solution are
; plugged in

(check-expect (plug-in '(2 2 3 10) '(1 1 2)) 10)
(check-expect (plug-in '(2 5 12 31) '(1 1 2)) 31)
(check-expect (plug-in '(4 1 -2 1) '(1 1 2)) 1)

(define (plug-in eqn sol)
  (local ((define LHS (lhs eqn)))
    (cond
      [(empty? LHS) 0]
      [else
       (+ (* (first LHS) (first sol))
          (plug-in (rest eqn) (rest sol)))])))

; SOE Solution -> Boolean
; interpretation: consumes SOE and Solution and produces
; a Boolean if plugging in Solution
; produces equal lhs and rhs values
(check-expect (check-solution M S) #true) 

(define (check-solution soe sol)
  (cond
    [(empty? soe) #true]
    [else
     (and (equal? (rhs (first soe)) (plug-in (first soe) sol))
          (check-solution (rest soe) sol))]))

    