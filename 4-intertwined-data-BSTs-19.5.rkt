;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4-intertwined-data-BSTs-19.5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BinaryTree (short for BT) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

(define bt1 (make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE)))

(define bt2 (make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE))

(define bt3 (make-node 63 'a
                       (make-node 29 'b
                                  (make-node 15 'c
                                             (make-node 10 'd NONE NONE)
                                             (make-node 24 'e NONE NONE))
                                  NONE)
                       (make-node 89 'f
                                  (make-node 77 'g NONE NONE)
                                  (make-node 95 'h NONE
                                             (make-node 99 'i NONE NONE)))))

(define bt4 (make-node 63 'a
                       (make-node 29 'b
                                  (make-node 15 'c
                                             (make-node 87 'd NONE NONE)
                                             (make-node 24 'e NONE NONE))
                                  NONE)
                       (make-node 89 'f
                                  (make-node 33 'g NONE NONE)
                                  (make-node 95 'h NONE
                                             (make-node 99 'i NONE NONE)))))

(define bt5 (make-node 63 'a
                       (make-node 29 'b
                                  (make-node 15 'c
                                             (make-node 10 'd NONE NONE)
                                             (make-node 24 'e NONE NONE))
                                  NONE)
                       NONE))

(define bt6 (make-node 63 'a
                       (make-node 29 'b
                                  (make-node 15 'd
                                             (make-node 10 'h NONE NONE)
                                             (make-node 24 'i NONE NONE))
                                  NONE)
                       (make-node 89 'c
                                  (make-node 77 'l NONE NONE)
                                  (make-node 95 'g NONE
                                             (make-node 99 'o NONE NONE)))))
     

;; ex 323
; BT -> Boolean
; interpretation: function consumes a binary-tree
; and determines whether a given number occurs in it

(check-expect (contains-bt? 15 bt1) #t)
(check-expect (contains-bt? 15 bt2) #t)
(check-expect (contains-bt? 33 bt1) #f)
(check-expect (contains-bt? 87 bt2) #t)
(check-expect (contains-bt? 99 bt3) #t)
(check-expect (contains-bt? 100 bt3) #f)

(define (contains-bt? n a-bt)
  (cond
    [(and (node? a-bt) (equal? (node-ssn a-bt) n)) #t]
    [(and (node? (node-left a-bt)) (node? (node-right a-bt))) (or (contains-bt? n (node-left a-bt)) (contains-bt? n (node-right a-bt)))]
    [(node? (node-left a-bt)) (contains-bt? n (node-left a-bt))]
    [(node? (node-right a-bt)) (contains-bt? n (node-right a-bt))]
    [else #f]))


;; ex 324
; BT -> [List-of Number]
; interpretation: function consumes a binary tree
; produces the sequence of all ssn numbers in the tree
; from left to right

(check-expect (inorder bt3) (list 10 15 24 29 63 77 89 95 99))
(check-expect (inorder bt4) (list 87 15 24 29 63 33 89 95 99))

;(define (inorder a-bt)
;  (cond
;    [(not (node? a-bt)) '()]
;    [(and (not (node? (node-left a-bt)))
;          (not (node? (node-right a-bt))))
;          (list (node-ssn a-bt))]
;    [(and (not (node? (node-left (node-left a-bt))))
;          (not (node? (node-left (node-right a-bt))))
;          (not (node? (node-right (node-left a-bt))))
;          (not (node? (node-right (node-right a-bt)))))
;     (list (node-ssn (node-left a-bt)) (node-ssn a-bt) (node-ssn (node-right a-bt)))]
;    [else (append (inorder (node-left a-bt)) (list (node-ssn a-bt)) (inorder (node-right a-bt)))]))

(define (inorder a-bt)
  (cond
    [(and (not (node? (node-right a-bt)))
          (not (node? (node-left a-bt)))) (list (node-ssn a-bt))]
    [else
     (cond
       [(not (node? (node-right a-bt)))
        (append (inorder (node-left a-bt)) (list (node-ssn a-bt)))]
       [(not (node? (node-left a-bt)))
        (append (list (node-ssn a-bt)) (inorder (node-right a-bt)))]
       [else
        (append (inorder (node-left a-bt)) (list (node-ssn a-bt)) (inorder (node-right a-bt)))])]))

;; ex 325
; Number BST -> [Maybe Name NONE]
; interpretation: function consumes a bst
; inorder to determine whether a ssn exists
; produces the name if exists
; NONE o.w

(check-expect (search-bst 63 bt3) 'a)
(check-expect (search-bst 77 bt3) 'g)
(check-expect (search-bst 3 bt3) NONE)
(check-expect (search-bst 80 bt3) NONE)
(check-expect (search-bst 200 bt3) NONE)

(define (search-bst n a-bt)
  (cond
    [(equal? n (node-ssn a-bt)) (node-name a-bt)]
    [(> n (node-ssn a-bt))
     (cond
       [(not (node? (node-right a-bt))) NONE]
       [else (search-bst n (node-right a-bt))])]
    [(< n (node-ssn a-bt))
     (cond
       [(not (node? (node-left a-bt))) NONE]
       [else (search-bst n (node-left a-bt))])]
    [else NONE]))

;; ex 326

; B N S -> BST
; interpretation: consumes a BST Number and Symbol
; inserts the new node into the correct position

(check-expect (create-bst bt3 13 'z)
              (make-node 63 'a
                       (make-node 29 'b
                                  (make-node 15 'c
                                             (make-node 10 'd NONE
                                                        (make-node 13 'z NONE NONE))
                                             (make-node 24 'e NONE NONE))
                                  NONE)
                       (make-node 89 'f
                                  (make-node 77 'g NONE NONE)
                                  (make-node 95 'h NONE
                                             (make-node 99 'i NONE NONE)))))

(define (create-bst a-bst n s)
  (cond
    ; is a-bst a node? if no; then construct a bst from the inputted arguments
    [(not (node? a-bst)) (make-node n s NONE NONE)]
    ; is n < ssn of node? && node-left of node does not exist?
    ; then construct the bst with the inputted arguments on node-left of the last node..
    [(and (< n (node-ssn a-bst)) (not (node? (node-left a-bst))))
     (make-node (node-ssn a-bst) (node-name a-bst)
                (make-node n s NONE NONE) (node-right a-bst))]
    [(and (> n (node-ssn a-bst)) (not (node? (node-right a-bst))))
     (make-node (node-ssn a-bst) (node-name a-bst)
                (node-left a-bst) (make-node n s NONE NONE))]
    [(< n (node-ssn a-bst)) (make-node (node-ssn a-bst) (node-name a-bst)
                                       (create-bst (node-left a-bst) n s) (node-right a-bst))]
    [(> n (node-ssn a-bst)) (make-node (node-ssn a-bst) (node-name a-bst)
                                       (node-left a-bst) (create-bst (node-right a-bst) n s))]))

;; ex 327
; [List-of [List Number Symbol]] -> BST
; interpretation: consumes a list of numbers
; and names and produces a binary search tree
; by repeatedly applying create-bst

(check-expect (create-bst-from-list '((99 o)
                                      (77 l)
                                      (24 i)
                                      (10 h)
                                      (95 g)
                                      (15 d)
                                      (89 c)
                                      (29 b)
                                      (63 a))) bt6)

(define (create-bst-from-list lons)
  (cond
    [(empty? (rest lons)) (create-bst NONE
                                      (first (first lons))
                                      (second (first lons)))]
    [else (create-bst (create-bst-from-list (rest lons))
                      (first (first lons))
                      (second (first lons)))]))
  
              
                                    

                                

