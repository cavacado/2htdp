;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 3-abstractions-compute-local-16.4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; ex 263- 264
(define (inf.v3 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest
               (inf.v3 (rest l))))
       (cond
         [(< (first l) smallest-in-rest) (first l)]
         [else smallest-in-rest]))]))

;(inf.v3 (list 2 1 3))

(define (sup.v3 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define largest-in-rest
               (sup.v3 (rest l))))
       (cond
         [(> (first l) largest-in-rest) (first l)]
         [else largest-in-rest]))]))

(sup.v3 (list 2 1 3))


; [List-of Posn] -> [List-of Posn]
; adds 3 to each x-coordinate on the given list

(check-expect
 (add-3-to-all
  (list (make-posn 30 10) (make-posn 0 0)))
 (list (make-posn 33 10) (make-posn 3 0)))

(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; adds 3 to the x-coord of p
          (define (add-3 p)
            (make-posn (+ (posn-x p) 3) (posn-y p))))
    (map add-3 lop)))

; [List-of Posn] -> [List-of Posn]
; eliminates Posns whose y-coordinate is > 100

(check-expect
 (keep-good (list (make-posn 0 110) (make-posn 0 60)))
 (list (make-posn 0 60)))

(define (keep-good lop)
  (local (; Posn -> Boolean
          ; checks if y-coordinate is < 100
          (define (<y-100? p)
            (cond
              [(< (posn-y p) 100) #t]
              [else #f])))
    (filter <y-100? lop)))

; Posn Posn Number -> Boolean
; is the distance between p and q less than d

(check-expect (close-to (make-posn 5 10) (make-posn 10 20) 1500) #t)

(define (close-to p q d)
  (local (; Posn Posn -> Number
          ; takes 2 posns and computes the distance between them
          (define (cal p q)
            (sqrt (+ (sqr (- (posn-x q) (posn-x p))) (sqr (- (posn-y q) (posn-y p))))))
          (define distance
            (cal p q)))     
    (cond
      [(< distance d) #t]
      [else #f])))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt?

(check-expect
 (close? (list (make-posn 47 54) (make-posn 0 60))
         (make-posn 50 50))
 #f)

(define (close? lop pt)
  (local (; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))

(define CLOSENESS 5)
  