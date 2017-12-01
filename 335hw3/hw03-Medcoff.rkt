#lang racket
(#%provide (all-defined))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
compilation of the accompanying test file. Changes that are almost certain to break
the above restriction are:
  - changing the names of any definitions that are explicitely used in the tests
    (e.g. function names, relevant constants)

If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions as you want.

When done, make sure that the accompanying test file compiles. 
|#
;======================================01=======================================
(define (foldl-335 op default-el lst) ; default-el "should" be the identity of
  (if (null? lst)                     ; op (i.e. 0 for +, 1 for *, etc.)
      default-el
      (foldl-335 op (op (car lst) default-el) (cdr lst))  
))

;---

(define (revcar lst)
  (car (reverse lst)))

(define (revcdr lst)
  (reverse (cdr (reverse lst))))

(define (foldr-335 op default-el lst)
   (if (null? lst)
       default-el
       (foldr-335 op (op (revcar lst) default-el) (revcdr lst)))
)

;======================================02=======================================

(define (jasonmap proc lst)
  (foldr (lambda (x l)
           (cons (proc x) l))
         '()
         lst))


(define (andmap-335 test-op lst)
  (foldl (lambda (x y) (and x y)) #t (jasonmap test-op lst))
)

;======================================03=======================================
(define (filter-335 test-op lst)
  (cond
    ((null? lst) (list)) ; case 1: empty list
    ((test-op (car lst)) (cons (car lst) (filter-335 test-op (cdr lst))))
    ; case 2: first element satisfies; cons new list starting with this element
    (else (filter-335 test-op (cdr lst)))) ; case 3: first element loses, ditch it
)

;======================================04=======================================
(define (map-reduce m-op r-op default-el lst)
  (foldl r-op default-el (map m-op lst))
)

;======================================05=======================================
; Helper factorial function. Not TR optimized, but gets the job done.
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1))))
  )

(define (item n)
  (/ (if (even? n) 1 -1)
     (fact (+ n 1))))

(define (series n)
  (foldl + 0 (map item (range (+ 1 n))))
)

;======================================06=======================================
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) (list)
      (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2))))
)

;======================================07=======================================

(define (transpose mat) ; change the matrix from a list of rows to list of cols
  (if (null? (car mat)) (list)
      (cons (map car mat) (transpose (map cdr mat)))))

(define (matrix-to-vector op mat)
  (map (lambda (x) (apply op x)) (transpose mat))  
)

