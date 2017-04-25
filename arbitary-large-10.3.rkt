;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arbitary-large-10.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

;;LLS definition
; A LLS is one of:
; - '()
; - (cons Los LLS)
; interpretation a list of lines, each is a list of Strings

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

; LLS -> List-of-numbers
; determines the number of words on each line

(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1) (cons 2 (cons 0 '())))

(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else (cons (length (first lls))
                (words-on-line (rest lls)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
   (read-words/line file-name)))

;;ex 172
; List-of-lines -> String
; converts a list of lines into a string

(define (collapse alol)
  (cond
    [(empty? alol) ""]
    [else (string-append (line->string (first alol)) "\n" (collapse (rest alol)))]))

; List-of-lines -> String
; converts a list of lines into a string (no articles)

(define (collapse.v2 alol)
  (cond
    [(empty? alol) ""]
    [else (string-append (line->string-no-articles (first alol)) "\n" (collapse.v2 (rest alol)))]))

; List -> String
; converts a list into a string

(define (line->string line)
  (cond
    [(empty? line) ""]
    [else (string-append (first line) " " (line->string (rest line)))]))

; List -> String
; converts a list into a string and removes the articles

(define (line->string-no-articles line)
  (cond
    [(empty? line) ""]
    [else (string-append (no-articles (first line)) " " (line->string-no-articles (rest line)))]))

; String -> String
; checks whether string contains an article, if no then do nothing, else remove the string

(define (no-articles str)
  (cond
    [(or (equal? str "a") (equal? str "an") (equal? str "the")) ""]
    [else str]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String

(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t") (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a") (string-append "0" (code1 "a")))

(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10) (string-append "00" (code1 s))]
    [(< (string->int s) 100) (string-append "0" (code1 s))]))

; 1String -> String
; convert the give 1String into a String

(check-expect (code1 "z") "122")

(define (code1 c)
  (number->string (string->int c)))

; List-of-Lines -> String
; reads the file, splits it into list of lines within the lines, each string will be exploded
; and converted into an encoded string

(define (collapse-cust alol)
  (cond
    [(empty? alol) ""]
    [else (string-append (list->string-cust (first alol)) (collapse-cust (rest alol)))]))

(define (encode-list aloc)
  (cond
    [(empty? aloc) ""]
    [else (string-append (encode-letter (first aloc)) (encode-list (rest aloc)))]))

(check-expect (encode-list (explode (collapse-cust (read-words/line "ttt.txt"))))
              "084084084080117116117112105110097112108097099101119104101114101105116039115101097115121116111115101101116104101099114121112116105099097100109111110105115104109101110116084046084046084046087104101110121111117102101101108104111119100101112114101115115105110103108121115108111119108121121111117099108105109098044105116039115119101108108116111114101109101109098101114116104097116084104105110103115084097107101084105109101046080105101116072101105110")

; List -> String

(define (list->string-cust list)
  (cond
    [(empty? list) ""]
    [else (string-append (first list) (list->string-cust (rest list)))]))

; File -> List-of-numbers
; consumes a file, and produces a list of numbers, 1 being the no. of characters in the file
; 2 being the no. of lines in the file
; 3 being the no. of words in the file

(define (wc file)
  (cons (length (explode (read-file file))) (cons (length (read-lines file)) (cons (length (read-words file)) '()))))

; Lo1s -> Lo1s
; produces a reverse version of the given list

(check-expect (rev (cons "a" (cons "b" (cons "c" '()))))
              (cons "c" (cons "b" (cons "a" '()))))

(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else
     (cons (first l) (add-at-end (rest l) s))]))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)


;; editor portion is uncompleted (go back to original editor portion in the first part of book if want to proceed)
(define (create-editor pre post)
  (make-editor (rev (explode pre)) (explode post)))

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e)
  (place-image/align
    (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))

(define (editor-text s)
  (cond
    [(empty? s) (text "" FONT-SIZE FONT-COLOR)]
    [else (beside (text (first s) FONT-SIZE FONT-COLOR) (editor-text (rest s)))]))
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(define (editor-kh ed ke)
  ed)

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))