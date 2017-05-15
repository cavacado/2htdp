;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-processing-2-lists-simultaneously-23.1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; replace the final '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '(a b)) '(a b))

(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

; [List-of Symbol] [List-of Number] -> [List of Symbol Number]
; interpretation : fn consumes list of symbols and numbers
; then produces all possible ordered pairs of symbols
; and numbers

(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else
     (append (map (lambda (x)
                  (list (first los) x))
                lon)
           (cross (rest los) lon))]))


; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
      (weekly-wage (first hours) (first wages/h))
      (wages*.v2 (rest hours) (rest wages/h)))]))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

;; ex 388

(define-struct employee [name ssn pay-rate])
(define-struct wk-record [name noh])
(define-struct wage-record [name wkly-wage])

; [List-of Employee] [List-of Wk-record] -> [List-of Wage-record]

(check-expect (wages*.v3 '() '()) '())

(check-expect (wages*.v3 (list (make-employee "John" 12345 5.65))
                         (list (make-wk-record "John" 40)))
              (list (make-wage-record "John" 226.0)))

(check-expect (wages*.v3 (list (make-employee "John" 12345 5.65)
                               (make-employee "Jane" 54321 8.75))
                         (list (make-wk-record "John" 40)
                               (make-wk-record "Jane" 30.0)))
              (list (make-wage-record "John" 226.0)
                    (make-wage-record "Jane" 262.5)))

(define (wages*.v3 loe lowr)
  (cond
    [(empty? loe) '()]
    [else
     (cons
      (weekly-wage.v2 (employee-pay-rate (first loe)) (wk-record-noh (first lowr)) (employee-name (first loe)))
      (wages*.v3 (rest loe) (rest lowr)))]))


(define (weekly-wage.v2 pay-rate hours name)
  (make-wage-record name (* pay-rate hours)))

;; ex 389

; [List-of String] [List-of String] -> [List-of Phone-record]
; interpretation: consumes list of names and list of phone numbers
; to give a list of phone records

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
; (make-phone-record String String)

(check-expect (zip (list "John" "Mary" "Jane") (list "12345" "54321" "67890"))
              (list (make-phone-record "John" "12345")
                    (make-phone-record "Mary" "54321")
                    (make-phone-record "Jane" "67890")))

(define (zip lonam lonum)
  (cond
    [(empty? lonam) '()]
    [else
     (cons (make-phone-record (first lonam) (first lonum))
           (zip (rest lonam) (rest lonum)))]))

; N is one of:
; - 0
; - (add1 N)

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l;
; signals an error if there is no such symbol

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")

(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l))
     (error "list too short")]
    [(and (> n 0) (empty? l))
     (error "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))

;; ex 390
(define-struct branch [left right])

; A TOS is one of:
; - Symbol
; - (make-branch TOS TOS)

; A Direction is one of:
; - 'left
; - 'right

; A list of Directions is also called a path

(define tree1
  (make-branch
   (make-branch
    (make-branch 'a 'b)
    'c)
   'd))

(check-expect (tree-pick tree1 '(left left left)) 'a)
(check-expect (tree-pick tree1 '(left left right)) 'b)
(check-expect (tree-pick tree1 '(right)) 'd)
(check-error (tree-pick 'z '(right)) "tree does not have branches")

(define (tree-pick tree lod)
  (cond
    [(and (not (branch? tree)) (> (length lod) 0)) (error "tree does not have branches")]
    [(and (not (branch? tree)) (= (length lod) 0)) tree] 
    [(branch? tree)
     (cond
       [(equal? 'left (first lod)) (tree-pick (branch-left tree) (rest lod))]
       [(equal? 'right (first lod)) (tree-pick (branch-right tree) (rest lod))])]))


; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s.


  



