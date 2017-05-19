;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo-nature-of-numbers) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; make an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; convert an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
       10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

;; ex 412
; Inex Inex -> Inex
; adds 2 Inex together
; the result must be another instance of an Inex

(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0)) (create-inex 3 1 0))
(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0)) (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0)) (create-inex 11 1 1))

(define (inex+ i1 i2)
  (cond
    [(> (+ (inex-mantissa i1) (inex-mantissa i2)) 99)
     (local ((define temp
               (make-inex (round (/ (+ (inex-mantissa i1) (inex-mantissa i2)) 10)) (inex-sign i1) (+ 1 (inex-exponent i1)))))
       (cond
         [(and (<= 0 (inex-mantissa temp) 99) (<= 0 (inex-exponent temp) 99) (or (= (inex-sign temp) 1) (= (inex-sign temp) -1)))
          temp]
         [else (error "out of range")]))]
    [else
     (make-inex (+ (inex-mantissa i1) (inex-mantissa i2)) (inex-sign i1) (inex-exponent i1))]))
