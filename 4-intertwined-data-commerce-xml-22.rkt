;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-commerce-xml-22) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; An Xexpr.v0 (short for X-expression) is one-item list:
; (cons Symbol '())

; An Xexpr.v1 is a list:
; (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; - (cons Symbol XL)
; An XL is one of:
; - [List-of Xexpr.v2]
; - (cons [List-of Attribute] [List-of Xexpr.v2])
;
; An Attribute is a list of 2 items:
; (cons Symbol (cons String '()))

;; ex 363

; An Xexpr.v2 is a list:
; - (cons Symbol XL)
; An XL is one of:
; - (cons (cons Symbol XL) '())
; - (cons [List-of Attribute] (cons (cons Symbol XL) '()))

;; ex 364

; <transition from="seen-e" to="seen-f" />
; -> '(transition ((from "seen-e") (to "seen-f")))

; <ul><li><word /><word /></li><li><word /></li></ul>
; -> '(ul
;      (li
;       (word)
;       (word))
;      (li
;       (word)))

;; ex 365
; '(server ((name "example.org")))
; -> <server name="example.org" />

; '(carcas (board (grass)) (player ((name "sam"))))
; -> <carcas><board><grass /></board><player name="sam" /></carcas>

; '(start)
; -> <start />

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe

(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))                 

; [List-of Attribute] -> Boolean
; Xexpr.v2 -> Boolean
; determine whether x is an element of [List-of Attribute]
; #f o.w
(define (list-of-attributes? x)
  (cond
    [(empty? x) #t]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

;; ex 366

; Xexpr.v2 -> Name
; retrieves the name of xe

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> Content
; retrieves the content of xe

(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '(((initial "X"))))
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '(() (action)))
(check-expect (xexpr-content e4) '(((initial "X")) (action) (action)))

(define (xexpr-content xe)
  (rest xe))

;; ex 369
; [List-of Attributes] Symbol -> [Maybe String]
; interpretation: fn consumes list of attributes
; and a symbol.
; if attributes list associates the symbol with a string
; return that string
; else return #f

(define attr-list '((attr1 "lalal") (attr2 "haihai") (attr3 3)))

(check-expect (find-attr attr-list 'attr1) "lalal")
(check-expect (find-attr attr-list 'attr2) "haihai")
(check-expect (find-attr attr-list 'attr3) #f)

(define (find-attr loa sym)
  (cond
    [(and (list? (assq sym loa)) (string? (second (assq sym loa)))) (second (assq sym loa))]
    [else #f]))