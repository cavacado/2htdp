;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-finger-ex-pt7-26.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 400
; [List of DNA-describ] [List-of DNA-describ] -> Boolean
; interpretation: fn consumes 2 lists of symbols
; produces a boolean if the pattern (first list)
; is identical to the initial part of search str (2nd list)

(check-expect (DNAprefix '(a c g t a c g t) '(a c g t a c g t)) #t)
(check-expect (DNAprefix '(g c t a g t g) '(a c g t a)) #f)

(define (DNAprefix l1 l2)
  (equal? l1 l2))

; [List of DNA-describ] [List-of DNA-describ] -> Boolean
; interpretation: fn consumes 2 lists of symbols
; produces an error if the pattern (first list)
; is identical to the initial part of search str (2nd list) and if there isnt
; any DNA letter beyond the pattern
; if pattern does not match string, returns #f

(check-error (DNAdelta '(a c g t a c g t) '(a c g t a c g t)) "no more letters beyond pattern")
(check-expect (DNAdelta '(g c t a g t g) '(a c g t a)) #f)
(check-expect (DNAdelta '(a c g t a c g t a c g t) '(a c g t a c g t)) #t)

(define (DNAdelta l1 l2)
  (cond
    [(and (equal? l1 l2) (= (length l1) (length l2))) (error "no more letters beyond pattern")]
    [else
     (local ((define (aux-fn l1 l2)
               (cond
                 [(and (not (empty? l1)) (empty? l2)) #t]
                 [(equal? (first l1) (first l2)) (aux-fn (rest l1) (rest l2))]
                 [else #f])))
       (aux-fn l1 l2))]))

;; ex 401

; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

(check-expect (sexp=? '(1 2 3 (a b c ("str1" "str2" "str3"))) '(1 2 3 (a b c ("str1" "str2" "str3")))) #t)
(check-expect (sexp=? '(2 "str1" a (b v d ("str2" 1 1))) '(1 2 3 (a b c ("str1" "str2" "str3")))) #f)

(define (sexp=? sexp1 sexp2)
  (cond
    [(and (empty? sexp1) (empty? sexp2)) #t]
    [(equal? (first sexp1) (first sexp2))
     (sexp=? (rest sexp1) (rest sexp2))]
    [else #f]))