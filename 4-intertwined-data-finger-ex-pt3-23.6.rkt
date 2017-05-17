;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-finger-ex-pt3-26.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define DICTIONARY-LOCATION "/usr/share/dict/words") ; on OS X
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(define SIZE (length DICTIONARY-AS-LIST))
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; A HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; run a simplistic Hangman game, produce the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

(require racket/list)
; HM-Word HM-Word Keyevent -> HM-Word
; interpretation: fn consumes a 2 HM-Words and a Keyevent
; produces a HM-Word with all "_" where the guess revealed a letter

(check-expect (compare-word '("w" "o" "r" "d") '("w" "_" "_" "_") "d") '("w" "_" "_" "d"))
(check-expect (compare-word '("w" "o" "r" "d") '("w" "_" "_" "_") "r") '("w" "_" "r" "_"))
(check-expect (compare-word '("w" "o" "r" "d") '("_" "o" "_" "_") "w") '("w" "o" "_" "_"))
(check-expect (compare-word '("w" "o" "r" "d") '("w" "_" "r" "d") "o") '("w" "o" "r" "d"))

(define (compare-word hmw1 hmw2 ke)
  (local ((define indx
            (cond
              [(member ke hmw1) (index-of hmw1 ke)]
              [else #f]))
          (define (make-word hmw num w)
            (cond
              [(equal? num 0) (cons w (rest hmw))]
              [else (cons (first hmw) (make-word (rest hmw) (sub1 num) w))])) 
          )
    (cond
      [(number? indx) (make-word hmw2 indx ke)]
      [(false? indx) hmw2])))

(play (list-ref DICTIONARY-AS-LIST (random SIZE)) 10)

