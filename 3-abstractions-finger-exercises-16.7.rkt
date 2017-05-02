;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 3-abstractions-finger-exercises-16.7) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; ex 267

; List-of Number -> List-of Number
; [X Y] [X -> Y] [List-of X] -> [List of Y]
; interpretation: consumes a function that consumes an X to produce a Y,
; and consumes a List of X to give Y

(check-expect (convert-euro (list 5 10 15 20)) (list (* 5 0.92) (* 10 0.92) (* 15 0.92) (* 20 0.92)))

(define (convert-euro lou)
  (local (;Number -> Number
          ;converts each usd into euro
          (define (usd->euro n)
            (* 0.92 n)))
    (map usd->euro lou)))

; List-of Posn -> LLoP (List of List of pairs)
; [X Y] [X -> Y] [List-of X] -> [List of Y]
; consumes List-of Posns and changes into a List of list of pairs of numbers

(check-expect (translate (list
                          (make-posn 5 10)
                          (make-posn 10 20)
                          (make-posn 30 50)))
              (list
               (list 5 10)
               (list 10 20)
               (list 30 50)))

(define (translate lop)
  (local (; Posn -> [List-of X Y]
          ; takes a posn and extracts the x and y and puts them in a list
          (define (extractXY p)
            (list (posn-x p) (posn-y p))))
    (map extractXY lop)))

;; ex 268
; [X Y] [X Y -> Boolean] [List-of X] -> [List-of X]
; consumes a function that compares the difference between 2 prices and sorts them accordingly
; produces a list of sorted inventory prices
(define-struct IR [name description acq-pxs rec-pxs])

(check-expect (sortIR (list (make-IR "ball" "a ball" 20 30)
                            (make-IR "doll" "a doll" 5 10)
                            (make-IR "stall" "a stall" 30 100)))
              (list (make-IR "doll" "a doll" 5 10)
                    (make-IR "ball" "a ball" 20 30)
                    (make-IR "stall" "a stall" 30 100)))

(define (sortIR loir)
  (local (; IR IR -> Boolean
          ; consumes 2 IR records, checks the difference in acq-pxs and rec-pxs in the first IR
          ; checks the difference in acq-pxs and rec-pxs in the second IR
          ; then produces a #t to indicate whether IR1 is < IR2
          ; else produces a #f
          (define (diff-in-pxs ir1 ir2)
            (< (- (IR-rec-pxs ir1) (IR-acq-pxs ir1))
               (- (IR-rec-pxs ir2) (IR-acq-pxs ir2)))))
    (sort loir diff-in-pxs)))

;; ex 269
; [X Y] [X Y -> Boolean] [List-of Y] -> List-of Y
; interpretation: function consumes another function that compares 2 values,
; returning t or f respectively
; also consumes a list of IR records and produces
; a list of filtered IR records

(check-expect (eliminate-expensive 50 (list (make-IR "ball" "a ball" 20 30)
                                           (make-IR "doll" "a doll" 5 10)
                                           (make-IR "stall" "a stall" 30 100)))
              (list (make-IR "ball" "a ball" 20 30)
                    (make-IR "doll" "a doll" 5 10)))

(define (eliminate-expensive ua loir)
  (local (; Number IR -> Boolean
          ; takes a number and an inventory record to
          ; produce a boolean
          (define (is-expensive? ir)
            (cond
              [(> ua (IR-rec-pxs ir)) #t]
              [else #f])))
    (filter is-expensive? loir)))

;; ex 269 b
; [X Y] X [Y -> Boolean] List-of Y -> List-of Y
; interpretation; function consumes a name of inventory item
; with list of IR records and produces a list of inventory records
; that do not use name

(check-expect (recall "doll" (list (make-IR "ball" "a ball" 20 30)
                                   (make-IR "doll" "a doll" 5 10)
                                   (make-IR "stall" "a stall" 30 100)))
              (list (make-IR "ball" "a ball" 20 30)
                    (make-IR "stall" "a stall" 30 100)))

(define (recall ty loir)
  (local (; IR -> Boolean
          ; takes a IR record and produces a boolean
          ; by checking the predicate
          (define (is-called? ir)
            (cond
              [(equal? ty (IR-name ir)) #f]
              [else #t])))
    (filter is-called? loir)))

;; ex 269 c
; [X Y] [List-of X] [List-of Y] [X Y -> Boolean] -> List-of X
; interpretation: takes 2 lists of names and selects all those
; from the second one that are also on the first

(check-expect (selection (list "name1" "name2" "name3") (list "name2" "name3" "name4"))
              (list "name2" "name3"))

(define (selection lon1 lon2)
  (local (; X -> Boolean
          ; takes a name and checks if its a member in lon2
          ; return #t or #f
          (define (is-member? n1)
            (cond
              [(member? n1 lon2) #t]
              [else #f])))
    (filter is-member? lon1)))

;; ex 270

(check-expect (norm-build-list 5) (list 0 1 2 3 4))

(define (norm-build-list n)
  (local ((define (f x) x))
    (build-list n f)))

(check-expect (build-list-sw1 5) (list 1 2 3 4 5))

(define (build-list-sw1 n)
  (local ((define (f x) (add1 x)))
    (build-list n f)))

(check-expect (build-list-div-n 5) (list 1 1/2 1/3 1/4 1/5))

(define (build-list-div-n n)
  (local ((define (f x)
            (/ 1 (add1 x))))
    (build-list n f)))

(check-expect (build-list-even 5) (list 0 2 4 6 8))

(define (build-list-even n)
  (local ((define (f x)
            (* 2 x)))
    (build-list n f)))

(define (f x)
  (* x 10))

(check-expect (tabulate f 5) (list 0 10 20 30 40))

(define (tabulate fn n)
  (build-list n fn))

;; ex 271
; [X] [X List-of X] -> Boolean
; The function consumes a name and a list of names.
; It determines whether any of the names on the latter
; are equal to or an extension of the former.

(check-expect (find-name "john" (list "peter" "jane" "mary" "bob")) #f)
(check-expect (find-name "spiderman" (list "joker" "batman" "superman" "spidermandoesthelaundry")) #t)

(define (find-name name lon)
  (local (; Name -> Boolean
          ; checks whether the name is in the List of Names
          (define (is-in-or-part-of? n)
            (string-contains? name n)))
    (ormap is-in-or-part-of? lon)))


;; ex 271 b
; [X Y] [X List-of Y] -> Boolean
; checks all names on a list of names
; that start with the letter "a"

(check-expect (start-with "a" (list "adonis" "adam" "ada" "arthur")) #t)
(check-expect (start-with "a" (list "adonis" "adam" "ada" "barthur")) #f)

(define (start-with l lon)
  (local (; 1String Name -> Boolean
          ; checks whether the name starts with the 1string provided
          (define (1st-letter n)
            (equal? (first (explode n)) l)))
    (andmap 1st-letter lon)))

;; you should use andmap to ensure that no name on some list exceeds some given width

;; ex 272
; [X] [List-of X List-of X] -> [List-of X]
; collapses both lists into one big list

(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8)) (list 1 2 3 4 5 6 7 8))

(define (append-from-fold l1 l2)
  (local ((define (f x r) (cons x r)))
    (foldr f l2 l1)))

(check-expect (reduce-sum (list 1 2 3 4 5 6)) 21)
(check-expect (reduce-product (list 1 2 3 4)) 24)

(define (reduce-sum lon)
  (foldl + 0 lon))

(define (reduce-product lon)
  (foldl * 1 lon))

(check-expect (map-from-fold f (list 1 2 3 4)) (list 10 20 30 40))

(define (map-from-fold fn l)
  (local ((define (improve-fn n r)
            (cons (fn n) r)))
    (foldr improve-fn '() l)))