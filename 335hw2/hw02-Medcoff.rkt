#lang racket
(#%provide (all-defined))
#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (if (list? lst)
  (cond [(null? lst)
        #t]
        [(not (and (number? (car lst)) (even? (car lst))))
        #f]
        [else
        (list-of-even-numbers? (cdr lst))])
  #f)
)

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (if (= n 1)
      1
      (+ (/ 1 (expt n 2)) (series-a (- n 1))))
)

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...

; Helper factorial function. Not TR optimized, but gets the job done.
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1))))
  )

(define (series-b n)
  (if (= n 0)
      1
      (+ (/ (expt -1 n) (fact (+ 1 n))) (series-b (- n 1))))
)

;======================================03=======================================
; Helper function to encase a list with a single item
(define (encase lst i)
  (cons i (append lst (list i))))

; Helper function for a list of symbol characters based on size desired
(define (row n)
  (make-list (+ 1 (* n 2))
             (if (even? n)
                '%
                '+)))

; Helper function to alternate symbols in encasing
(define (encaserow r)
  (encase r (if (even? (/ (- 1 (length r)) 2))
                '+
                '%)))


(define (carpet n)
  (if (= n 0)
      '((%))
      (encase (map
               encaserow (carpet (- n 1)))
               (row n))
      )
)

;======================================04=======================================
(define (binomialrow n k)
  (if (= k 0)
      (list 1)
      (append (binomialrow n (- k 1)) (list(binomial n k)))))

(define (binomial n k)
  (/ (fact n) (* (fact k) (fact (- n k)))))


(define (pascalcall n)
  (cond
    ;((= n 1)
    ; '((1)))
    ((= n 1)
     '((1) (1 1)))
    (else
     (append (pascalcall (- n 1)) (list (binomialrow n n))))
))

(define (pascal n)
  (if (= 1 n)
      '((1))
      (pascalcall (- n 1))))

;======================================05=======================================
(define (balancedr lst n)
  (cond
    ((null? lst)
     (if (zero? n) #t #f))
    ((< n 0) #f)
    ((equal? (car lst) #\()
     (balancedr (cdr lst) (+ 1 n)))
    ((equal? (car lst) #\))
     (balancedr (cdr lst) (- n 1)))
    (else
     (balancedr (cdr lst) n))))
  

(define (balanced? in)
  (balancedr (string->list in) 0)  
)

;======================================06=======================================
(define (list-of-all? predicate lst)
  (if (null? lst) #t
      (and (predicate (car lst)) (list-of-all? predicate (cdr lst))))
)

;======================================07=======================================
(define (getvalue s kys vls)
  (cond
    ((or (null? kys) (null? vls))
     (string-append "Could not find mapping for symbol '"
                    (symbol->string s)))
    ((equal? s (car kys)) (car vls))
    (else
     (getvalue s (cdr kys) (cdr vls)))))

(define (create-mapping keys vals)
  (cond
    ((not (list-of-all? symbol? keys))
     "The keys are not all symbols.")
    ((not (= (length vals) (length keys)))
     "The lists are not of equal length.")
    (else
     (lambda
         (sym)
       (getvalue sym keys vals))))  
)