;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arbitary-itunes-12.2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; an LTracks is one of:
; - '()
; - (cons Track LTracks)

;(define-struct track
  ;[name artist album time track# added play# played])

; A Track is a structure:
; (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's
; title, its producing artist, to which album it belongs,
; its playing time in ms, its position with the album,
; the date it was added, how often it has been played,
; and the date when it was last played

;(define-struct date [year month day hour minute second])
; A Date is a structure:
; (make-date N N N N N N)
; interpretation: An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive),
; day (between 1 and 31), hour (between 0 and 23),
; minute (between 0 and 59), and
; second (also between 0 and 59).

;eg (make-date 2012 12 20 17 6 4) -> refers to the date 2012/12/20 @ 1706hrs and 4 secs)

; Any Any Any Any Any Any Any Any -> Track or #f
; creates an instance of Track for legitimate inputs
; o.w it produces #f
;(define (create-track name artist album time track# added play# played)
;  ...)

; Any Any Any Any Any Any -> Date or #f
; creates an instance of Date for legitimate inputs
; o.w it produces #f
;(define (create-date y mo day h m s)
;  ...)

; String -> LTracks
; creates a "list of tracks" representation from the
; text in file-name (an XML export from iTunes)
;(define (read-itunes-as-tracks file-name)
;  ...)

(define ITUNES-LOCATION "sample-itunes-lib.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; ex 200
; total-time
; LTracks -> Number

(define (total-time lot)
  (cond
    [(empty? lot) 0]
    [else (+ (track-time (first lot)) (total-time (rest lot)))]))

; ex 201
; select-all-album-titles
; LTracks -> LStrings

(define (select-all-album-titles lot)
  (cond
    [(empty? lot) '()]
    [else (cons (track-album (first lot)) (select-all-album-titles (rest lot)))]))

; create-set
; LStrings -> LStrings

(define (create-set los)
  (cond
    [(empty? los) '()]
    [else
     (cond
       [(member (first los) (rest los)) (create-set (rest los))]
       [else (cons (first los) (create-set (rest los)))])]))

; select-album-titles/unique
; LTracks -> LStrings

(define (select-album-titles/unique alot)
  (create-set (select-all-album-titles alot)))

; select-album
; String LTracks -> LTracks

(define (select-album title alot)
  (cond
    [(empty? alot) '()]
    [(equal? title (track-album (first alot))) (cons (track-name (first alot)) (select-album title (rest alot)))]
    [else (select-album title (rest alot))]))

; compare-dates
; Date Date -> Date
; takes 2 dates and compares them, returning the earlier one

;year month day hour minute second
(define (compare-dates d1 d2)
  (cond
    [(> (date-year d1) (date-year d2)) #t]
    [(< (date-year d1) (date-year d2)) #f]
    [(= (date-year d1) (date-year d2))
     (cond
       [(> (date-month d1) (date-month d2)) #t]
       [(< (date-month d1) (date-month d2)) #f]
       [(= (date-month d1) (date-month d2))
        (cond
          [(> (date-day d1) (date-day d2)) #t]
          [(< (date-day d1) (date-day d2)) #f]
          [(= (date-day d1) (date-day d2))
           (cond
             [(> (date-hour d1) (date-hour d2)) #t]
             [(< (date-hour d1) (date-hour d2)) #f]
             [(= (date-hour d1) (date-hour d2))
              (cond
                [(> (date-minute d1) (date-minute d2)) #t]
                [(< (date-minute d1) (date-minute d2)) #f]
                [(= (date-minute d1) (date-minute d2))
                 (cond
                   [(> (date-second d1) (date-second d2)) #t]
                   [(< (date-second d1) (date-second d2)) #f]
                   [(= (date-second d1) (date-second d2)) #t])])])])])]))

; ex 203
; select-album-date
; String Date LTracks -> LTracks

(define (select-album-date title date alot)
  (cond
    [(empty? alot) '()]
    [(and (equal? title (track-album (first alot))) (compare-dates (track-added (first alot)) date)) (cons (track-name (first alot)) (select-album-date title date (rest alot)))]
    [else (select-album-date title date (rest alot))]))

; ex 204
; select-albums
; LTracks -> List-of-structs

(define-struct album [title tracks])
(define unique-albums
  (select-album-titles/unique itunes-tracks))

(define (select-albums alot)
  (cond
    [(empty? alot) '()]
    [else (cons (make-album (first alot) (select-album (first alot) alot)) (select-albums (rest alot)))]))


; part 2

; An LLists is one of:
; - '()
; - (cons LAssoc LLists)

; An LAssoc is one of:
; - '()
; - (cons Association LAssoc)
;
; An Association is a list of two items:
; - (cons String (cons BSDN '()))

; A BSDN is one of:
; - Boolean
; - Number
; - String
; - Date

; String -> LLists
; creates a list of lists representation for all tracks in
; file-name,  which must be an XML export from iTunes
; (define (read-itunes-as-lists file-name)
; ...)

; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; ex 206
; String LAssoc Any -> Assoc

(define (find-association key loa default)
  (cond
    [(empty? loa) default]
    [else
     (cond
       [(equal? (first (first loa)) key) (first loa)]
       [else (find-association key (rest loa) default)])]))

(check-expect (find-association "Genre" (first list-tracks) #f) (list "Genre" "Mash-up"))

; ex 207
; LLists -> Number

(define (total-time/list alol)
  (cond
    [(empty? alol) 0]
    [else (+ (first (rest (find-association "Total Time" (first alol) 0))) (total-time/list (rest alol)))]))

; ex 208
; LLists -> String

(define (boolean-attributes alol)
  (cond
    [(empty? alol) '()]
    [else
     (cons (create-set (find-all-booleans (first alol))) (boolean-attributes (rest alol)))]))

(define (find-all-booleans aloa)
  (cond
    [(empty? aloa) '()]
    [else
     (cond
       [(boolean? (second (first aloa))) (cons (first aloa) (find-all-booleans (rest aloa)))]
       [else (find-all-booleans (rest aloa))])]))