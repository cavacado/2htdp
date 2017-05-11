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

; An XWord is '(word ((text String)))

;; ex 370
(define xword1 '(word ((text "str1"))))
(define xword2 '(word ((text "str2"))))
(define xword3 '(word ((text "str3"))))

; expression -> Boolean
; interpretation: takes a ISL+ value and
; checks whether it is in XWord

(check-expect (word? xword1) #t)
(check-expect (word? 3) #f)
(check-expect (word? xword2) #t)

(define (word? w)
  (cond
    [(number? w) #f]
    [(list? w) #t]
    [(symbol? (first w)) #t]
    [(symbol? (first (second w))) #t]
    [else #f]))

; XWord -> String
; interpretation: extracts the text string from the xword

(check-expect (word-text xword1) "str1")
(check-expect (word-text 3) #f)
(check-expect (word-text xword2) "str2")

(define (word-text w)
  (cond
    [(word? w) (second (assq 'text (second w)))]
    [else #f]))

; An Xexpr is one of:
; - String
; - (cons Symbol XL)
; An XL is one of:
; - (cons (cons Symbol XL) '())
; - (cons [List-of Attribute] (cons (cons Symbol XL) '()))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define enum0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define BULLET1 (circle 1 "solid" "black"))

(define e0-rendered
  (above/align 'left
               (beside/align 'center BULLET1
                             (text "one" 12 'black))
               (beside/align 'center BULLET1
                             (text "two" 12 'black))))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BULLET1 item)))

; XEnum.v1 -> Image
; renders a simple enumeration as an image

;; ex 372
(check-expect (render-enum1 enum0) e0-rendered)

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left
                         (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BULLET ; a graphical constant 
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))
 
; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BULLET item))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image 
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (beside/align
     'center BULLET
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))

(define enum1
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define enum2
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))
    (li (ul
         (li (word ((text "three"))))
         (li (word ((text "four"))))))))

(define e1-rendered
  (above/align 'left
               (beside/align 'center BULLET
                             (text "one" 12 'black))
               (beside/align 'center BULLET
                             (text "two" 12 'black))))

(check-expect (render-enum enum1) e1-rendered)

;; ex 374

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))


;; ex 375
; revised cond
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
;(define (render-item an-item)
;  (local ((define content (first (xexpr-content an-item))))
;    (cond
;      [(word? content) (beside/align 'center BULLET (text (word-text content) SIZE 'black))]
;      [else (beside/align 'center BULLET (render-enum content))])))




