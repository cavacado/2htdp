;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-refining-fns-20.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
(define dir1 (create-dir "/Users/zhaoloong/exercism"))
(define dir2 (create-dir "/Users/zhaoloong/racket"))

(define (how-many dir)
  (+ (length (dir-files dir))
     (foldr + 0 (map how-many (dir-dirs dir)))))

;; ex 338
; 6 files plus 2 .DStore
;(check-expect (how-many dir1) 8)

;; ex 339
; Dir String -> Boolean
(check-expect (find? dir2 "findme.txt") #t)
(check-expect (find? dir2 "tradfa.txt") #f)
(check-expect (find? dir1 "hello_world.exs") #t)
(check-expect (find? dir1 "tralalala.txt") #f)

(define (find? dir str)
  (cond
    [(list-finder (dir-files dir) str) #t]
    [else
     (ormap find? (dir-dirs dir) (make-list (length (dir-dirs dir)) str))]))

(define (list-finder lx str)
  (cond
    [(empty? lx) #f]
    [else
     (cond
       [(equal? str (file-name (first lx))) #t]
       [else (list-finder (rest lx) str)])]))

;; ex 340

; Dir -> [List-of String]
; interpretation: consumes a dir
; then produces the list of names of all files
; and sub-directories

(check-expect (ls dir2) (list '/Users/zhaoloong/racket "findme.txt" '/Users/zhaoloong/racket/dir1))

(define (ls dir)
  (append (list (dir-name dir))
          (file-list-append (dir-files dir))
          (foldr append '() (map ls (dir-dirs dir)))))

(define (file-list-append lx)
  (cond
    [(empty? lx) '()]
    [else
     (cons (file-name (first lx)) (file-list-append (rest lx)))]))


;; ex 341
; Dir -> Number
; consumes a dir and computes the total size
; of it

(check-expect (du dir2) 2)
(check-expect (du dir1) 18627)

(define (lx-file-size lx)
  (foldr (lambda (x r)
           (+ (file-size x) r)) 0 lx))

(define (du dir)
  (+ 1
     (lx-file-size (dir-files dir))
     (foldr + 0 (map du (dir-dirs dir)))))

