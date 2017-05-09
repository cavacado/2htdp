;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-interpreting-exp-21.1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define-struct add [left right])
(define-struct mul [left right])

;; ex 345
; BSL-exp is one of:
; - Number
; - BSL

; SL is one of:
; - '()
; - (cons BSL-exp BSL)

; (+ 10 -10)
; -> (make-add 10 -10)

; (+ (* 20 3) 33)
; -> (make-add (make-mul 20 3) 33)

; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
; -> (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9)))

; (make-add -1 2)
; -> (+ -1 2)

; (make-add (make-mul -2 -3) 33)
; -> (+ (* -2 -3) 33)

; (make-mul (make-add 1 (make-mul 2 3)) 3.14)
; -> (* (+ 1 (* 2 3)) 3.14)

; BSV is one of:
; Number

; BSL-exp -> BSV
; interpretation: fn consumes a BSL exp and produces a BSV
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)

(define (eval-expression bslexp)
  (cond
    [(number? bslexp) bslexp]
    [else
     (cond
       [(add? bslexp) (+ (eval-expression (add-left bslexp))
                         (eval-expression (add-right bslexp)))]
       [(mul? bslexp) (* (eval-expression (mul-left bslexp))
                         (eval-expression (mul-right bslexp)))])]))

;; ex 348

(define-struct cus& [left right])
(define-struct cus|| [left right])
(define-struct cus! [exp])

; BSL-bool-exp is one of:
; - Boolean
; - BSL-bool

; BSL-bool is one of:
; '()
; (cons BSL-bool-exp BSL-bool)

(check-expect (eval-bool-expression #t) #t)
(check-expect (eval-bool-expression #f) #f)
(check-expect (eval-bool-expression (make-cus& #t #f)) #f)
(check-expect (eval-bool-expression (make-cus& #t (make-cus& #t #t))) #t)
(check-expect (eval-bool-expression (make-cus|| #t #f)) #t)
(check-expect (eval-bool-expression (make-cus! #f)) #t)
(check-expect (eval-bool-expression (make-cus! (make-cus& #t (make-cus|| #t #f)))) #f)

(define (eval-bool-expression bslboolexp)
  (cond
    [(boolean? bslboolexp) bslboolexp]
    [else
     (cond
       [(cus&? bslboolexp) (and (eval-bool-expression (cus&-left bslboolexp))
                                (eval-bool-expression (cus&-right bslboolexp)))]
       [(cus||? bslboolexp) (or (eval-bool-expression (cus||-left bslboolexp))
                                (eval-bool-expression (cus||-right bslboolexp)))]
       [(cus!? bslboolexp) (not (eval-bool-expression (cus!-exp bslboolexp)))])]))

; An Atom is one of:
; - Number
; - String
; - Symbol

(check-expect (atom? 10) #t)
(check-expect (atom? "hello") #t)
(check-expect (atom? 'symbol) #t)
(check-expect (atom? (make-posn 5 10)) #f)

(define WRONG "error")

(define (atom? x)
  (cond
    [(string? x) #t]
    [(symbol? x) #t]
    [(number? x) #t]
    [else #f]))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

;; ex 349

(check-expect (parse 5) 5)
(check-error (parse "string") "error")
(check-error (parse 'symbol) "error")
(check-error (parse (list "oneexponly")) "error")
(check-error (parse (list "notasymbol" "but" "3 length")) "error")
(check-error (parse (list '- 10 20)) "error")

;; ex 351
; S-exp -> BSL-exp or Error
; interpretation: fn accepts S-exps and checks if they are
; valid BSL-exprs
; if so produce their value
; else return an error

(check-expect (interpreter-expr 5) 5)
(check-expect (interpreter-expr (cons '+ (cons 5 (cons 10 '())))) 15)
(check-error (interpreter-expr (list '- 10 20)) "error")
(check-error (interpreter-expr (list "oneexponly")) "error")

(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))



