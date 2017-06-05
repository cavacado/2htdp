;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-generative-recursion-numeric-integration-28.2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define (constant x) 20)

;(check-expect (integrate-kepler constant 12 22) 200)

(define (linear x) (* 2 x))

;(check-expect (integrate-kepler linear 0 10) 100)

(define (square-cus x) (* 3 (sqr x)))

;(check-expect (integrate-kepler square-cus 0 10)
;              (- (expt 10 3) (expt 0 3)))

(define epsilon 0.1)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds

;(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 epsilon)
;(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 epsilon)
;(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              epsilon)

;; ex 458
(define (integrate-kepler f a b)
  (local ((define R-L (- b a)))
    (* 0.5 R-L (+ (f a) (f b)))))

;; 1 out of 3 failed, the last one failed.

(check-within (integrate-kepler2 (lambda (x) 20) 12 22 10000) 200 epsilon)
(check-within (integrate-kepler2 (lambda (x) (* 2 x)) 0 10 10000) 100 epsilon)
(check-within (integrate-kepler2 (lambda (x) (* 3 (sqr x))) 0 10 10000)
              1000
              epsilon)

;; ex 459
(define (integrate-kepler2 f a b R)
  (local ((define width (/ (- b a) 10000))
          (define S (/ width 2)))
    (cond
      [(zero? R) (+ (* width (f (+ a (* 0 width) S))))]
      [else
       (+ (* width (f (+ a (* (sub1 R) width) S))) (integrate-kepler2 f a b (sub1 R)))])))

          
;; ex 460

(check-within (integrate-kepler3 (lambda (x) 20) 12 22) 200 epsilon)
(check-within (integrate-kepler3 (lambda (x) (* 2 x)) 0 10) 100 epsilon)
;(check-within (integrate-kepler3 (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              epsilon)

(define (integrate-kepler3 f a b)
  (cond
    [(<= (abs (- b a)) 1)
     (+ (* 0.5 (- b a) (+ (f a) (f b))))]
    [else
     (+ (integrate-kepler3 f a (+ a (/ (- b a) 2))) (integrate-kepler3 f (+ a (/ (- b a)  2)) b))]))
  
