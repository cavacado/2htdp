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
;(check-expect (lhs (first M)) '(2 2 3))

(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right hand side from a row in a matrix
;(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

;; ex 462

; Equation Solution -> Number
; interpretation: consumes lhs of Equation and Solution
; calculates the value of the lhs when numbers from solution are
; plugged in

;(check-expect (plug-in '(2 2 3 10) '(1 1 2)) 10)
;(check-expect (plug-in '(2 5 12 31) '(1 1 2)) 31)
;(check-expect (plug-in '(4 1 -2 1) '(1 1 2)) 1)

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
;(check-expect (check-solution M S) #true) 

(define (check-solution soe sol)
  (cond
    [(empty? soe) #true]
    [else
     (and (equal? (rhs (first soe)) (plug-in (first soe) sol))
          (check-solution (rest soe) sol))]))

;; ex 465
; Equation Equation -> Equation
; interpretation: takes 2 eqns of equal length,
; 'subtracts' them such that the first coefficient is 0
; thus returning the rest of the 'subtracted' eqn

;(check-expect (subtract-soe '(2 2 3 10) '(2 5 12 31) 0) '(-3 -9 -21))
;(check-expect (subtract-soe '(-3 -9 -21) '(-3 -8 -19) 0) '(-1 -2))
;(check-expect (subtract-soe '(3 9 21) '(-3 -8 -19) 0) '(1 2))

(define (subtract-soe eqn1 eqn2 count)
  (local (
          (define (aux-subtract-soe eq1 eq2 count)
            (cond
              [(not (equal? (length eq1) (length eq2))) (error "eqns are of different lengths")]
              [else
               (cond
                 [(empty? eq1) '()]
                 [(or (zero? (- (first eq1) (first eq2))) (not (equal? 0 count)))
                  (cons (- (first eq1) (first eq2))
                        (aux-subtract-soe (rest eq1) (rest eq2) (add1 count)))]
                 [else
                  (local ((define (find-multiple x y)
                            (/ x y))
                          (define multiple-temp (find-multiple (first eq1) (first eq2)))
                          (define new-aux-eq (map (lambda (x)
                                                    (* x multiple-temp)) eq2)))
                    (aux-subtract-soe eq1 new-aux-eq count))])])))
    (rest (aux-subtract-soe eqn1 eqn2 count))))

;; ex 466
; A TM is a [NEList-of Equation]
; such that the Equantions are of decreasing length:
; n + 1, n, n - 1, ..., 2.
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations.

;(check-expect (aux-triangulate M) (list '(2 2 3 10) '(-3 -9 -21) '(-1 -2)))


(define (triangulate soe)
  (local ((define (aux2-triangulate soe)
           (local ((define (aux-triangulate soe)
                     (local ((define (remv-2nd soe)
                               (cond
                                 [(>= (length soe) 2) (cons (first soe) (rest (rest soe)))]
                                 [else soe])))
                       (cond
                         [(empty? (rest soe)) '()]
                         [else
                          (cons (subtract-soe (first soe) (second soe) 0) (aux-triangulate (remv-2nd soe)))])))
                   (define temp-res (aux-triangulate soe)))
             (cond
               [(empty? temp-res) temp-res]
               [else
                (cons (first temp-res) (aux2-triangulate temp-res))]))))
    (cons (first soe) (aux2-triangulate soe))))
      
    


 


  
    



    