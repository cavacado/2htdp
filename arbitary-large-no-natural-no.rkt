;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arbitary-large-no-natural-no) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))

;;alt def

(define (copier2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier2 (sub1 n) s))]))

;;ex 150

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

(check-within (add-to-pi 3) (+ 3 pi) 0.001)

(define (gen-add n x)
  (cond
    [(zero? n) x]
    [else (add1 (gen-add (sub1 n) x))]))

;;ex 151

(define (gen-mult n x)
  (cond
    [(zero? x) 0]
    [else (+ n (gen-mult n (sub1 x)))]))

;;ex 152
;;(square 40 "outline" "slateblue")
(define (col n img)
  (cond
    [(equal? n 1) img]
    [else (above img
                 (col (sub1 n) img))]))

(define (row n img)
  (cond
    [(equal? n 1) img]
    [else (beside img
                 (row (sub1 n) img))]))

;;russian dolls

(define-struct layer [color doll])

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else (+ (depth (layer-doll an-rd)) 1)]))

(check-expect (depth "red") 1)
(check-expect (depth (make-layer "yellow" (make-layer "green" "red"))) 3)

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))

(check-expect (colors (make-layer "yellow" (make-layer "green" "red"))) "yellow, green, red")

;;ex 155

(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (inner (layer-doll an-rd))]))

;;sets LHS
(define es '())

(define s1.L
  (cons 1(cons 1 '())))

(define (set-.L x s)
  (remove-all x s))

(check-expect (set-.L 1 s1.L) es)

;;sets RHS
(define s1.R
  (cons 1 '()))

(define (set-.R x s)
  (remove x s))

(check-expect (set-.R 1 s1.R) es)

;;ex 160
(define (set+.L x s)
  (append (cons x '()) s))

(check-expect (set+.L 1 s1.L) (cons 1(cons 1(cons 1 '()))))
