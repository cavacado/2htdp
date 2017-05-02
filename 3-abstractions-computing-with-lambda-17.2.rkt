;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 3-abstractions-computing-with-lambda-17.2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

(define (f-plain x)
  (* 10 x))

(define f-lambda
  (lambda (x)
    (* 10 x)))

(compare (random 100000))

;; ex 283

(map (lambda (x) (* 10 x))
     '(1 2 3))

(foldl (lambda (name rst)
         (string-append name ", " rst))
       "etc."
       '("Matthew" "Robby"))

(define-struct ir [name price])
(define th 15)

(filter (lambda (ir) (<= (ir-price ir) th))
        (list (make-ir "bear" 10)
              (make-ir "doll" 33)))

;; ex 284

; evaluates to (lambda (x) x)
((lambda (x) x) (lambda (x) x))

;; evaluates to (lambda (x) x)
((lambda (x) (x x)) (lambda (x) x))

;; inf recursion
;((lambda (x) (x x)) (lambda (x) (x x)))

;; ex 285

; [List-of Number] -> [List-of Number]

(check-expect (convert-euro (list 5 10 15 20)) (list (* 1.22 5) (* 1.22 10) (* 1.22 15) (* 1.22 20)))

(define (convert-euro lous)
  (map (lambda (us)
         (* 1.22 us))
       lous))

; [List-of Posn] -> [List-of Number Number]
(check-expect (translate (list
                          (make-posn 5 10)
                          (make-posn 10 20)
                          (make-posn 30 50)))
              (list
               (list 5 10)
               (list 10 20)
               (list 30 50)))

(define (translate lop)
  (map (lambda (x)
         (list (posn-x x) (posn-y x)))
       lop))

;; ex 286
; [List-of IR] -> [List-of IR]
(define-struct IR [name description acq-pxs rec-pxs])

(check-expect (sortIR (list (make-IR "ball" "a ball" 20 30)
                            (make-IR "doll" "a doll" 5 10)
                            (make-IR "stall" "a stall" 30 100)))
              (list (make-IR "doll" "a doll" 5 10)
                    (make-IR "ball" "a ball" 20 30)
                    (make-IR "stall" "a stall" 30 100)))

(define (sortIR loir)
  (sort loir (lambda (ir1 ir2)
               (< (- (IR-rec-pxs ir1) (IR-acq-pxs ir1))
                  (- (IR-rec-pxs ir2) (IR-acq-pxs ir2))))))

;; ex 287
; Number [List-of IR] -> [List-of IR]
(check-expect (eliminate-expensive 50 (list (make-IR "ball" "a ball" 20 30)
                                           (make-IR "doll" "a doll" 5 10)
                                           (make-IR "stall" "a stall" 30 100)))
              (list (make-IR "ball" "a ball" 20 30)
                    (make-IR "doll" "a doll" 5 10)))

(define (eliminate-expensive ua loir)
  (filter (lambda (x)
            (cond
              [(> ua (IR-rec-pxs x)) #t]
              [else #f]))
          loir))

; String [List-of IR] -> [List-of IR]
(check-expect (recall "doll" (list (make-IR "ball" "a ball" 20 30)
                                   (make-IR "doll" "a doll" 5 10)
                                   (make-IR "stall" "a stall" 30 100)))
              (list (make-IR "ball" "a ball" 20 30)
                    (make-IR "stall" "a stall" 30 100)))

(define (recall ty loir)
  (filter (lambda (x)
            (cond
              [(equal? ty (IR-name x)) #f]
              [else #t]))
          loir))

; [List-of String] [List-of String] -> [List-of String]
(check-expect (selection (list "name1" "name2" "name3") (list "name2" "name3" "name4"))
              (list "name2" "name3"))

(define (selection lon1 lon2)
  (filter (lambda (x)
            (cond
              [(member? x lon2) #t]
              [else #f]))
          lon1))

;; ex 288
(check-expect (norm-build-list 5) (list 0 1 2 3 4))

(define (norm-build-list n)
  (build-list n (lambda (x) x)))


(check-expect (build-list-sw1 5) (list 1 2 3 4 5))

(define (build-list-sw1 n)
  (build-list n (lambda (x) (add1 x))))

(check-expect (build-list-div-n 5) (list 1 1/2 1/3 1/4 1/5))

(define (build-list-div-n n)
  (build-list n (lambda (x) (/ 1 (add1 x)))))

(check-expect (build-list-even 5) (list 0 2 4 6 8))

(define (build-list-even n)
  (build-list n (lambda (x) (* 2 x))))

;; ex 289
; Name [List-of Name] -> Boolean

(check-expect (find-name "john" (list "peter" "jane" "mary" "bob")) #f)
(check-expect (find-name "spiderman" (list "joker" "batman" "superman" "spidermandoesthelaundry")) #t)

(define (find-name nm lon)
  (ormap (lambda (x)
           (string-contains? nm x))
         lon))

; 1String [List-of String] -> Boolean

(check-expect (start-with "a" (list "adonis" "adam" "ada" "arthur")) #t)
(check-expect (start-with "a" (list "adonis" "adam" "ada" "barthur")) #f)

(define (start-with 1str lon)
  (andmap (lambda (x)
            (equal? (first (explode x)) 1str))
          lon))

;; ex 290
; [List-of Number] [List-of Number] -> [List-of Number]

(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8)) (list 1 2 3 4 5 6 7 8))

(define (append-from-fold l1 l2)
  (foldr (lambda (x r)
           (cons x r)) l2 l1))

;; ex 291
; [List-of Number] -> [List-of Number]
(define (f x)
  (* 10 x))

(check-expect (map-from-fold f (list 1 2 3 4)) (list 10 20 30 40))

(define (map-from-fold fn l1)
  (foldr (lambda (x r)
           (cons (fn x) r))
         '() l1))