;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname aribitary-11.4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Polygon is one of:
; - (list Posn Posn Posn) -> (cons Posn (cons Posn (cons Posn '())))
; - (cons Posn Polygon)

(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

(define triangle-p
  (list
    (make-posn 20 0)
    (make-posn 10 10)
    (make-posn 30 10)))

; a plain background image
(define MT (empty-scene 50 50))

; Image Polygon -> Image
; renders the given polygon p into img
(define (render-poly img p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
      (render-line
       (render-line MT (first p) (second p))
       (second p) (third p))
      (third p) (first p))]
    [else
     (render-line (render-poly img (rest p))
                  (first p)
                  (second p))]))
     

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
(define (render-line im p q)
  (scene+line
   im
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

(check-expect (render-line MT (make-posn 0 10) (make-posn 20 5))  (scene+line MT 0 10 20 5 "red"))

; A NELoP is one of:
; - (cons Posn '())
; - (cons Posn NELoP)

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else
     (render-line
      (connect-dots img (rest p))
      (first p)
      (second p))]))

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 0 10 10 "red")
               10 10 30 10 "red"))

; Image Polygon -> Image
; adds an image of p to img
(define (render-polygon.v2 img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))