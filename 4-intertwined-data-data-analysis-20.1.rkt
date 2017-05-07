;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-data-analysis-20.1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
;; ex 329
; read! only occurs 1 time
; tt size of files in tree = (+ 10 99 52 7 8 2 19)
; tt size of files plus directories = (+ 10 99 52 7 8 2 19 (* 1 5))
; 3 levels

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String

;; ex 330
; A Tree.v1 is one of:
; - '()
; - (cons Dir.v1)

;; ex 331
; Dir.v1 -> Number
; interpretation : this function determines the number of
; files present in a given dir

;(check-expect (how-many (list "file1" (list "file2"))) 2)

(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [else (+ 1 (how-many (rest dir)))]))

(define-struct dir [name content])

; A Dir.v2 is a structure:
; (make-dir String LOFD)

; A LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

(define dir-tree (make-dir "TS" (list "read"
                                      (make-dir "Text" (list "part1" "part2" "part3"))
                                      (make-dir "Libs" (list (make-dir "Code"
                                                                       (list "hang" "draw"))
                                                             (make-dir "Docs"
                                                                       (list "read!"))))
                                      )))

;; ex 333
; Dir.v2 -> Number
; interpretation: consumes a dir and
; gives the total number of files in it

(check-expect (how-many.v2 dir-tree) 7)

(define (how-many.v2 dir)
  (cond
    [(dir? dir) (processor (dir-content dir))]
    [else 0]))

(define (processor lofd)
  (cond
    [(empty? lofd) 0]
    [(dir? (first lofd)) (+ (how-many.v2 (first lofd)) (processor (rest lofd)))]
    [else (+ 1 (processor (rest lofd)))]))


;; ex 334     
(define-struct dir.v2 [name content size readability])

(define-struct file [name size content])

; A File.v3 is a structure:
; (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure:
; (make-dir.v3 String Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

;; ex 335

(define dir-tree.v3 (make-dir.v3 "TS"
                                 (list
                                  (make-dir.v3 "Text"
                                            '()
                                            (list
                                             (make-file "part1" 99 "content1")
                                             (make-file "part2" 52 "content2")
                                             (make-file "part3" 17 "content3")))
                                  (make-dir.v3 "Libs"
                                            (list
                                             (make-dir.v3 "Code"
                                                       '()
                                                       (list
                                                        (make-file "hang" 8 "content4")
                                                        (make-file "draw" 2 "content5")))
                                             (make-dir.v3 "Docs"
                                                       '()
                                                       (list
                                                        (make-file "read!" 19 "content6"))))
                                            '()))
                                  (list
                                   (make-file "read!" 10 "content7"))))

;; ex 336

(check-expect (how-many.v3 dir-tree.v3) 7)

(define (how-many.v3 dir)
  (+ (length (dir.v3-files dir))
     (foldr + 0 (map how-many.v3 (dir.v3-dirs dir)))))

; why confident? because of the tests i guess

