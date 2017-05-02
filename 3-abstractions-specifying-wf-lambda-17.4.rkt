;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 3-abstractions-specifying-wf-lambda-17.4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; [X X -> Boolean] -> [ [List-of X] -> Boolean ]
; produces a function that determines whether
; some list is sorted according to cmp

(check-expect [(sorted string<?) '("b" "c")] #t)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #t)
              
(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond
                [(empty? (rest l)) #t]
                [else (and (cmp (first l) (second l))
                           (sorted/l (rest l)))])))
      (if (empty? l0) #t
          (sorted/l l0)))))

; [X X -> Boolean] [NEList-of X] -> Boolean
; determine whether l is sorted according to cmp

(check-expect (sorted? < '(1 2 3)) #t)
(check-expect (sorted? < '(2 1 3)) #f)

(define (sorted? fn lon)
  (cond
    [(empty? (rest lon)) #t]
    [else
     (cond
       [(fn (first lon) (second lon)) (sorted? fn (rest lon))]
       [else #f])]))

(check-expect [(sorted.v2 string<?) '("b" "c")] #t)
(check-expect [(sorted.v2 <) '(1 2 3 4 5 6)] #t)

(define (sorted.v2 cmp)
  (lambda (l0)
    (sorted? cmp l0))
    )

; [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of list l0

(check-expect [(sorted-variant-of '(3 2) <) '(2 3)]
              #t)
(check-expect [(sorted-variant-of '(3 2) <) '(3)]
              #f)

(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         (contains? l0 k))))

; [List-of X] [List-of X] -> Boolean
; are all items in list k members of list l

(check-expect (contains? '(1 2 3) '(1 4 3)) #f)
(check-expect (contains? '(1 2 3 4) '(1 3)) #t)

(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))

;; ex 293
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #f o.w

(define (find x l)
  (cond
    [(empty? l) #f]
    [else
     (if (equal? (first l) x) l
         (find x (rest l)))]))

(check-satisfied (find 1 '(2 3 4 5 1 2 3)) (found? '(2 3 4 5 1 2 3)))

(define (found? l)
  (lambda (l0)
    (cond
      [(empty? l0) #f]
      [else
       (cond
         [(andmap (lambda (y)
                    (member? y l))
                  l0) #t]
         [else #f])])))

;; ex 294
; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

(check-satisfied (index 1 (list 4 5 6 7 1 2)) (is-index? 1 (list 4 5 6 7 1 2)))

(define (is-index? x l)
  (lambda (n)
    (equal? x (list-ref l n))))
  