;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 5-generative-recursion-binary-search-27.2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; [Number -> Number] Number Number -> Number
; determine R such that f has a root in [R,(+ R epsilon)]
; assume f is continuous
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
(define epsilon 0.5)
; generative divide interval in half, the root is in
; one of the 2 halves, pick according to (2)
(define (find-root f left right)
  (cond
    [(<= (- right left) epsilon) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))

;; ex 445
; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-expect (find-root poly 3 6) 3.75)