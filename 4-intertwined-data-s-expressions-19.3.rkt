;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-s-expressions-19.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; An S-expr is one of:
; - Atom
; - SL

;; ex 320
; An S-expr-revised is one of:
; - Number
; - String
; - Symbol
; - [List-of Number String Symbol] 

; An SL is one of:
; - '()
; - (cons S-expr SL)

; An Atom is one of:
; - Number
; - String
; - Symbol

(check-expect (atom? 10) #t)
(check-expect (atom? "hello") #t)
(check-expect (atom? 'symbol) #t)
(check-expect (atom? (make-posn 5 10)) #f)

(define (atom? x)
  (cond
    [(string? x) #t]
    [(symbol? x) #t]
    [(number? x) #t]
    [else #f]))

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(define (count sexp sy)
  (cond
    [(atom? sexp) (count-atom sexp sy)]
    [else (count-sl sexp sy)]))

; SL Symbol -> N
; counts all occurrences of sy in sl
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+ (count (first sl) sy) (count-sl (rest sl) sy))]))

; Atom Symbol -> N
; counts all occurrences of sy in at
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

(check-expect (count 'world 'world) 1)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

;; ex 317

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
; a rehash of the previous guided ex;
; using local functions inorder not to
; pollute global space

(check-expect (count.v2 'world 'world) 1)
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)

(define (count.v2 sexp sy)
  (local ((define (count-atom.v2 at sy)
           (cond
             [(number? at) 0]
             [(string? at) 0]
             [(symbol? at) (if (symbol=? at sy) 1 0)]))
          (define (count-sl.v2 sl sy)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl.v2 (rest sl) sy))])))
    (cond
      [(atom? sexp) (count-atom.v2 sexp sy)]
      [else (count-sl.v2 sexp sy)])))

;; ex 318

; S-expr -> N
; determines the depth of an S-expr

(check-expect (fin-depth 'atom) 1)
(check-expect (fin-depth '(world hello)) 2)
(check-expect (fin-depth '(((world) hello) hello)) 4)

(define (depth sexpr)
  (cond
    [(atom? sexpr) 1]
    [else (+ 2 (count-lx sexpr))]))

(define (count-lx sexpr)
  (cond
    [(empty? sexpr) 0]
    [else
     (cond
       [(list? (first sexpr)) (+ 1 (count-lx (first sexpr)) (count-lx (rest sexpr)))]
       [else (+ 0 (count-lx (rest sexpr)))])]))

(define (fin-depth sexpr)
  (cond
    [(atom? sexpr) 1]
    [else (apply max (map depth (list sexpr)))]))

;; ex 319

; S-expr Symbol Symbol -> S-expr
; interpretation: function replaces all instances of old symbol
; with new symbol

(define (substitute sexpr sym1 sym2)
  (cond
    [(and (atom? sexpr) (equal? sym1 sexpr)) sym2]
    [(atom? sexpr) sexpr]
    [else (aux-fn sexpr sym1 sym2)]))

(define (aux-fn sexpr sym1 sym2)
  (cond
    [(empty? sexpr) '()]
    [else (cons (substitute (first sexpr) sym1 sym2) (aux-fn (rest sexpr) sym1 sym2))]))
     


