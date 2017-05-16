;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-finger-ex-pt4-26.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
;; ex 397

(define-struct employee-record [name num pay-rate])
(define-struct time-card-record [num noh/wk])
(define-struct wage-record [name wkly-wage])

; [List-of Employee-record] [List-of Timecard-record] -> [List-of Wage-record]
; interpretation: fn consumes list of employee records and timecard record
; produces the list of wage records

(check-expect (wages*.v3 (list (make-employee-record "John" 1234 5)
                               (make-employee-record "Jane" 2345 6)
                               (make-employee-record "Justin" 4567 8))
                         (list (make-time-card-record 1234 20)
                               (make-time-card-record 4567 10)
                               (make-time-card-record 2345 30)
                               (make-time-card-record 1234 5)))
              (list (make-wage-record "John" 125)
                    (make-wage-record "Jane" 180)
                    (make-wage-record "Justin" 80)))

(define (wages*.v3 loe lot)
  (local ((define (wg-cal empl lot)
            (cond
              [(empty? lot) 0]
              [(equal? (employee-record-num empl) (time-card-record-num (first lot)))
               (+ (* (employee-record-pay-rate empl) (time-card-record-noh/wk (first lot)))
                  (wg-cal empl (rest lot)))]
              [else (wg-cal empl (rest lot))])))
    (cond
      [(empty? loe) '()]
      [else
       (cons (make-wage-record (employee-record-name (first loe)) (wg-cal (first loe) lot))
             (wages*.v3 (rest loe) lot))])))

