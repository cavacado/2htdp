;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 3-abstractions-design-with-abstractions-16.6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; [List-of Posn] -> Image 
; adds the Posns on lop to the empty scene
(define MT-SCENE (empty-scene 200 200))
(define DOT (circle 1 "solid" "red"))
 
(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 MT-SCENE))
 
(define (dots lop)
  (local (;; Posn -> Image
          ;; takes the posn-x and posn-y and creates an image
          (define (place-image-cus p scene)
            (place-image DOT (posn-x p) (posn-y p) scene)))
    (foldl place-image-cus MT-SCENE lop)))
