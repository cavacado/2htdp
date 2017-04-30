;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 3-abstractions-using-16) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; [List-of Addr] -> String 
; creates a string of first names, 
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define-struct address [first-name last-name])
(define (listing.v2 l)
  (local (; 1. extract names 
          (define names  (map address-first-name l))
          ; 2. sort the names 
          (define sorted (sort names string<?))
          ; 3. append them, add spaces 
          ; String String -> String
          ; appends two strings, prefix with " " 
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))


; [List-of Number] [Number Number -> Boolean] 
; -> [List-of Number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond
              [(empty? alon) '()]
              [else
               (insert (first alon) (isort (rest alon)))]))
 
          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon 
          (define (insert n alon)
            (cond
              [(empty? alon) (cons n '())]
              [else (if (cmp n (first alon))
                        (cons n alon)
                        (cons (first alon)
                              (insert n (rest alon))))])))
    (isort alon0)))

; ex 258
(define MT (empty-scene 50 50))
; Image Polygon -> Image 
; adds an image of p to MT
(define (render-polygon img p)
  (local (; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) MT]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))
          ; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
          ; Polygon -> Posn
           ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))]))
          (define result
            (render-line (connect-dots img p) (first p) (last p))))
    result))


(define triangle-p
  (list
    (make-posn 20 0)
    (make-posn 10 10)
    (make-posn 30 10)))

; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

; this guy is faster because (rest l) is only evaluated once;
; unlike the previous inf function which had to evaluate (rest l) many many times

; compared
; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

	
(define-struct ir
  [name price])
; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

(define (extract1.v2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define helper (extract1 (rest an-inv))))
     (cond
       [(<= (ir-price (first an-inv)) 1.0)
        (cons (first an-inv) helper)]
       [else helper]))]))

; ex 262
; Number -> LLoN (List of List of numbers)
; interpretation: this function consumes a number, then gives the identity matrix of size number
; the identity matrix can be defined as
; (list (list 1)) when number = 1
; and (list (list 1 0 0) (list 0 1 0) (list 0 0 1))

(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (identityM n)
  (local ((define (h x)
    (local ((define (k y)
             (cond
               [(equal? x y) 1]
               [else 0])))
      (build-list n k))))
    (build-list n h)))
  
