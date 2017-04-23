;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chp3-6) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (distance-to-0 a-posn)
    (sqrt
     (+ (sqrt (posn-x a-posn))
        (sqrt (posn-y a-posn)))))

(define-struct entry (name zip phone))

(define phonebook (make-entry 'PeterLee 15270 '606-7771))

(define-struct star (last first instrument sales))

(define (increment-sales a-star)
  (make-star (star-last a-star)
             (star-first a-star)
             (star-instrument a-star)
             (+ (star-sales a-star) 20000)))

(define-struct fighter (designation acceleration top-speed range))

(define (within-range a-fighter target-distance)
  (> (- (fighter-range a-fighter) target-distance) 0))

(define (reduce-range a-fighter)
  (make-fighter (fighter-designation a-fighter)
                (fighter-acceleration a-fighter)
                (fighter-top-speed a-fighter)
                (* 0.8 (fighter-range a-fighter))))

(define-struct time (hours minutes seconds))

(define (time-template a-time)
  (make-time (time-hours a-time)
             (time-minutes a-time)
             (time-seconds a-time)))

;; 6.5.2
(define (time->seconds a-time)
  (+ (* (time-hours a-time) 60 60) (* (time-minutes a-time) 60) (time-seconds a-time)))

;;6.6.1
(define-struct circle (center radius color))

(define (fun-for-circles a-circle)
  (make-circle (circle-center a-circle)
               (circle-radius a-circle)
               (circle-color a-circle)))

(define (translate-circle a-circle delta)
  (make-circle (make-posn
                (+ delta (posn-x (circle-center a-circle)))
                (posn-y (circle-center a-circle)))
               (circle-radius a-circle)
               (circle-color a-circle)))

(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-color c)))

(define (clear-a-circle c)
  (clear-circle (circle-center c)
               (circle-radius c)
               (circle-color c)))

(define (square x)
  (* x x))

(define (in-circle? c pos)
  (< (sqrt
      (+ (square (- (posn-x pos)
                    (posn-x (circle-center c))))
         (square (- (posn-y pos)
                    (posn-y (circle-center c))))))
     (circle-radius c))
  )

(define (draw-and-clear-circle c)
  (and (draw-a-circle c)
  (sleep-for-a-while 1)
  (clear-a-circle c)
  ))

(define (move-circle delta a-circle)
  (cond
    ((draw-and-clear-circle a-circle) (translate-circle a-circle delta))
    (else a-circle)))

(define-struct squarish (nw length))
(define-struct circlish (center radius))
(define-struct rectangularish (breadth length))

(define (perimeter a-shape)
  (cond
    ((squarish? a-shape) (* (squarish-length a-shape) 4))
    ((circlish? a-shape) (* (* 2 (circlish-radius a-shape)) pi))
    ((rectangularish? a-shape) (* 2 (+ (rectangularish-breadth a-shape) (rectangularish-length a-shape))))
    ))

(define (area a-shape)
  (cond
    ((squarish? a-shape) (square (squarish-length a-shape)))
    ((circlish? a-shape) (* (square (circlish-radius a-shape)) pi))
    ((rectangularish? a-shape) (* (rectangularish-breadth a-shape) (rectangularish-length a-shape)))
    ))

;; zoo definitions

(define-struct spider (num-legs-remaining space))
(define-struct elephant (space))
(define-struct monkey (intelligence space))

(define (fits? an-animal volume-cage)
  (cond
    ((spider? an-animal) (> volume-cage (spider-space an-animal)))
    ((elephant? an-animal) (> volume-cage (elephant-space an-animal)))
    ((monkey? an-animal) (> volume-cage (monkey-space an-animal)))
    ))

(define-struct bus (passengers cost))
(define-struct limo (passengers cost))
(define-struct four-wheel-drive (passengers cost))
(define-struct subway (passengers cost))

(define (helper4trip x y z)
  (* (/ x y) z))

(define (field-trip-cost? a-vehicle students)
  (cond
    ((bus? a-vehicle) (helper4trip students (bus-passengers a-vehicle) (bus-cost a-vehicle)))
    ((limo? a-vehicle) (helper4trip students (limo-passengers a-vehicle) (limo-cost a-vehicle)))
    ((four-wheel-drive? a-vehicle) (helper4trip students (four-wheel-drive-passengers a-vehicle) (four-wheel-drive-cost a-vehicle)))
    ((subway? a-vehicle) (helper4trip students (subway-passengers a-vehicle) (subway-cost a-vehicle)))
    ))

;; 7.5.1
;;(define (checked-area-of-disk v)
;;  (cond
;;    ((and (number? v) (> v 0)) (area-of-disk v))
;;    (else (error 'checked-area-of-disk "number expected"))
;;    ))

;; 7.5.3

(define-struct vec (x y))

(define (checked-make-vec a-vec)
  (cond
    ((and (number? (vec-x a-vec)) (number? (vec-y a-vec)) (> (vec-x a-vec) 0) (> (vec-y a-vec) 0)) (make-vec (vec-x a-vec) (vec-y a-vec)))
    (else (error 'checked-make-vec "positive number expected for both x and y"))
    ))



