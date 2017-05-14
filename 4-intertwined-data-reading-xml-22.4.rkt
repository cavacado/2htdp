;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-reading-xml-22.4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define PREFIX "https://www.google.com/finance?q=")
(define SUFFIX "&btnG=Search")
(define SIZE 22)

(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co SUFFIX))
          ; [StockWorld ->StockWorld]
          ; interpretation: retrieves the data from the url
          ; converts it into a StockWorld struct
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          ; interpretation: renders the data recieved
          ; into an image
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    ; interpretation: converts the data in StockWorld
                    ; into a word struct for displaying later
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
              [on-tick retrieve-stock-data 15]
              [to-draw render-stock-data])))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

(define attr-list '((attr1 "lalal") (attr2 "haihai") (attr3 3)))

;(check-expect (find-attr attr-list 'attr1) "lalal")
;(check-expect (find-attr attr-list 'attr2) "haihai")
;(check-expect (find-attr attr-list 'attr3) #f)

(define (find-attr loa sym)
  (cond
    [(and (list? (assq sym loa)) (string? (second (assq sym loa)))) (second (assq sym loa))]
    [else #f]))

; Xexpr -> String
(define (get-xexpr x s)
  (cond
    [(equal? (first x) 'meta)
     (cond
       [(equal? (find-attr (first (rest x)) 'itemprop) s)
        (second (assq 'content (first (rest x))))]
       [else #f])]
    [else #f]))

(define fd (read-xexpr "racket/ford.htm"))

  
