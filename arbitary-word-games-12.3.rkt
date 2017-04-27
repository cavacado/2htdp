;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arbitary-word-games-12.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; On OS X: 
(define DICTIONARY-LOCATION "/usr/share/dict/words")
 
; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

; String -> List-of-strings
; find all words that use the same letters as s

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a String as a list of 1Strings (letters)

; String -> Boolean
(define (all-words-from-rat? w)
  (and
   (member? "rat" w)
   (member? "art" w)
   (member? "tar" w)))

; String -> List-of-strings
; find all words that the letters of some given word spell

;(check-member-of (alternative-words "cat")
;                 (list "act" "cat")
;                 (list "cat" "act"))

;(check-satisfied (alternative-words "rat") all-words-from-rat?)

(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; ex 210
; List-of-words -> List-of-strings
; turn all Words in low into Strings
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))

; ex 211
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member? (first los) DICTIONARY-AS-LIST) (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))

; Word -> List-of-words
; find all re-arrangements of word

;ex 209
; String -> Word
; convert s to the chosen word representation
(define (string->word s)
  (string->list s))

; Word -> String
; convert w to a string
(define (word->string w)
  (list->string w))

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a String as a list of 1Strings (letters)

; List-of-words can be one of the following
; - '() or
; - (cons Word Low)
; interpretation a List-of-words as a list of words which are lists of 1Strings (letters)

(check-expect (list #\a #\b) (cons #\a (cons #\b '())))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w) (arrangements (rest w)))]))

; 1String List-of-words
; inserts 1String at front, inbetween a character and at end
(define (insert-everywhere/in-all-words char alow)
  (cond
    [(empty? alow) '()]
    [else (cons (permutate (explode char) (first alow)) (insert-everywhere/in-all-words char (rest alow)))]))

(define (permutate aloc w)
  (cond
    [(empty? w) '()]
    [else (cons (append (cons (first w) aloc) (rest w)) (permutate (cons (first w) aloc) (rest w)))]))

(define (jumble w)
  (insert-everywhere/in-all-words (first w) (permutate '() (rest w))))