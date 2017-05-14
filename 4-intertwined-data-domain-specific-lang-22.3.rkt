;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-domain-specific-lang-22.3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "dir.rkt" "teachpack" "htdp")) #f)))
; A FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; A FSM-State is a String that specifies a color
(define-struct transition [current next])
 
; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
; FSM FSM-State -> FSM-State 
; match the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay
         (text current 24 "black")
         (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

;; ex 379
(check-error (find fsm-traffic "blue") "not found")
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "yellow") "red")

;; ex 380
; change the key handler section
; on-key (specified keys will trigger event)
; else nothing happens

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))))

(define xm1
  '(machine ((initial "black"))
     (action ((state "black") (next "white")))
     (action ((state "white") (next "black")))))

; An XMachine is a nested list of this shape:
; `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
; `(action ((state ,FSM-State) (next ,FSM-State)))

;; ex 381

; An XMachine is a nested list of this shape:
; (list machine (list (list initial FSM-State)) [List-of X1T])
; (cons machine (cons (cons initial FSM-State) (cons X1T '())))

; An X1T is a nested list of this shape:
; (list action (list (list state FSM-State) (list next FSM-state)))
; (cons action (cons (cons state FSM-state) (cons next FSM-state)))

(define fsm-bw
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

(define xml-bw
  (list 'machine (list (list 'initial "black"))
        (list 'action (list (list 'state "black") (list 'next "white")))
        (list 'action (list (list 'state "white") (list 'next "black")))))

; XMachine -> FSM-State
; simulates an FSM via the given configuration 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

; XMachine -> FSM-State
; extracts and translates the transition table from xm0

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

(check-expect (xm-state0 xm0) "red")

(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))

; XMachine -> [List-of 1Transition]
; extracts the transition table from xm

(check-expect (xm->transitions xm0) fsm-traffic)

; Xexpr.v2 -> Content
; retrieves the content of xe

;(check-expect (xexpr-content e0) '())
;(check-expect (xexpr-content e1) '(((initial "X"))))
;(check-expect (xexpr-content e2) '((action)))
;(check-expect (xexpr-content e3) '(() (action)))
;(check-expect (xexpr-content e4) '(((initial "X")) (action) (action)))

(define (xexpr-content xe)
  (rest (rest xe)))

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))
