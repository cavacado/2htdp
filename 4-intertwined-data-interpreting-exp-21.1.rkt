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


; A BSL-var-expr is one of :
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

;; ex 352
; BSL-var-expr Symbol Number -> BSL-var-expr
; interpretation: consumes a BSL-var-exp, symbol and number
; produces a BSL-var-exp with the symbol replaced by the number

(check-expect (subst (make-add 'x 3) 'x 10) (make-add 10 3))
(check-expect (subst (make-mul 1/2 (make-mul 'x 3)) 'x 10) (make-mul 1/2 (make-mul 10 3)))
(check-expect (subst 'x 'x 10) 10)

(define (subst bslvarexp sym num)
  (cond
    [(number? bslvarexp) bslvarexp]
    [(and (symbol? bslvarexp) (equal? sym bslvarexp)) num]
    [(symbol? bslvarexp) bslvarexp]
    [(add? bslvarexp) (make-add (subst (add-left bslvarexp) sym num)
                                (subst (add-right bslvarexp) sym num))]
    [(mul? bslvarexp) (make-mul (subst (mul-left bslvarexp) sym num)
                                (subst (mul-right bslvarexp) sym num))]))

(define (BSL-exp? exp)
  (cond
    [(number? exp) #t]
    [(add? exp) #t]
    [(mul? exp) #t]
    [else #f]))

;; ex 353
; BSL-var-exp -> Boolean
; interpretaton: this fn checks whther a BSL-var-exp
; is also a BSL-exp

(check-expect (numeric? 5) #t)
(check-expect (numeric? (make-add 5 10)) #t)
(check-expect (numeric? 'x) #f)
(check-expect (numeric? (make-add 5 (make-mul 10 20))) #t)

(define (numeric? bslvarexp)
  (BSL-exp? bslvarexp))
  
;; ex 354
; BSL-var-exp -> Number or Error
; interpretation: this fn evaluates the BSL-var-exp
; if it is a BSL-expr
; else produces an Error

(check-expect (eval-variable 5) 5)
(check-expect (eval-variable (make-add 5 10)) 15)
(check-error (eval-variable 'x) "error")
(check-expect (eval-variable (make-add 5 (make-mul 10 20))) 205)

(define (eval-variable bslvarexp)
  (cond
    [(numeric? bslvarexp) (eval-expression bslvarexp)]
    [else (error "error")]))

; An AL (short for association list) is [List-of Association]
; An Association is a list of two items:
; (cons Symbol (cons Number '()))

; ex of AL
; (list (list 'x 5) (list 'y 10) (list 'z 15))

; BSL-var-exp AL -> Number
; interpretation: fn takes a bslvarexp and evaluates it
; using the appropriate substition as indicated by
; the association list

(check-expect (eval-variable* (make-add 'x 3) (list (list 'x 10))) 13)
(check-expect (eval-variable* (make-mul 1/2 (make-mul 'x 3)) (list (list 'x 10))) 15)
(check-expect (eval-variable* 'x (list (list 'x 10))) 10)
(check-expect (eval-variable* (make-add (make-mul 'x 'x) (make-mul 'y 'y))
                              (list
                               (list 'x 2)
                               (list 'y 5))) 29)

(define (eval-variable* bslvarexp al)
  (eval-variable (sub* bslvarexp al)))

(define (sub* bslvarexp al)
  (cond
    [(empty? al) bslvarexp]
    [else (sub* (subst bslvarexp (first (first al)) (second (first al))) (rest al))]))

;; ex 355
; BSL-var-expr AL -> Number

(check-expect (eval-var-lookup (make-add 'x 3) (list (list 'x 10))) 13)
(check-expect (eval-var-lookup (make-mul 1/2 (make-mul 'x 3)) (list (list 'x 10))) 15)
(check-expect (eval-var-lookup 'x (list (list 'x 10))) 10)
(check-expect (eval-var-lookup (make-add (make-mul 'x 'x) (make-mul 'y 'y))
                              (list
                               (list 'x 2)
                               (list 'y 5))) 29)

(define (eval-var-lookup bslvarexp al)
  (cond
    [(number? bslvarexp) bslvarexp]
    [(symbol? bslvarexp) (second (assq bslvarexp al))]
    [(add? bslvarexp) (+ (eval-var-lookup (add-left bslvarexp) al) (eval-var-lookup (add-right bslvarexp) al))]
    [(mul? bslvarexp) (* (eval-var-lookup (mul-left bslvarexp) al) (eval-var-lookup (mul-right bslvarexp) al))]
    [else (error "error")]))


;; ex 356

; A BSL-var-expr is one of :
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)
; - (list Symbol BSL-var-expr)

; (k (+ 1 1)) -> (list 'k (make-add 1 1))
; (* 5 (k (+ 1 1))) -> (make-mul 5 (list 'k (make-add 1 1)))
; (* (i 5) (k (+ 1 1))) -> (make-mul (list 'i 5) (list 'k (make-add 1 1))))

;; ex 357
; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Value
(define fn (list 'k (make-add 1 1)))

(check-expect (eval-definition1 fn 'f (first fn) (second fn) 10)
              2)

(define (eval-definition1 ex f x b arg)
  (eval-expression (subst b x arg)))

;; ex 358
(define-struct fn-def [name param body])
(define-struct fn-def* [name param body])

; a BSL-fun-def is one of:
; (make-fn-def Symbol Symbol BSL-var-exp)
; (make-fun-def Symbol Symbol BSL-fun-def)

; (define (f x) (+ 3 x)) -> (make-fn-def 'f 'x (make-add 3 'x))
; (define (g y) (f (* 2 y))) -> (make-fn-def 'g 'y (make-fn-def 'f 'y (make-mul 2 'y)))
; (define (h v) (+ (f v) (g v))) -> (make-fn-def 'h 'v (make-add (make-fn-def 'f 'v 'v) (make-fn-def 'g 'v 'v))))

; a BSL-fun-def* is:
; '()
; (cons BSL-fun-def '())

(define da-fgh
  (list (make-fn-def 'f 'x (make-add 3 'x))
        (make-fn-def 'g 'y (make-fn-def 'f 'y (make-mul 2 'y)))
        (make-fn-def 'h 'v (make-add (make-fn-def 'f 'v 'v) (make-fn-def 'g 'v 'v)))))

;; ex 359

  
  