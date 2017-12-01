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
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. You will
lose up to 25% of the total points for the entire homework depending on the number of errors.
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
  (* (+ 3 3) 9)
)

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
(define (p1-2)
  (/ (* 6 9) (+ (+ 4 2) (* 4 3)))
)

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
  (* 2 (* (- 45 42) (- 20 (/ 91 7))))
)
;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define p2
  "Write the expression's parse tree; the prefix notation is
   (node left-child right-child), recursively visiting child nodes."
)
;======================================03=======================================
;;Write the definitions of x,y,z here:
(define x 2)
(define y 3)
(define z 4)

;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (if (= x y z)
      0
      (if (and (<= x y)
               (<= y z))
          (+ y z)
          ( if (<= y z)
               (+ x z)
               (+ x y))))
)

;======================================05=======================================
(define (p5)
  (if (= x y z)
      0
      (if (and (<= x y)
               (<= y z))
          (+ y x)
          ( if (<= y z)
               (+ y z)
               (+ z x))))  
)

;======================================06=======================================
(define (p6)
  (= x y)  
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "The first statement is a variable assignment; 'thirty-five' will be
   interpreted as the numerical value 35. The second statement defines
   a function taking no arguments that always returns the value 35."
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  "The ' character makes racket interpret what follows as a literal; functions,
   variables, etc. will not be evaluated. The first element of a list with a
   preceding ' will not be interpreted as a function to be called."
)

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "The list function will perform any functions in the arguments as it constructs
   the output list, whereas ' will not."
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "Symbols have string representations, and two symbols are the same if they are
   spelled the same. Strings can be spelled the same, but still be distinct, i.e.
   two different memory locations."
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list 4 2 6 9)
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))
(define (p11-2)
  (list 'spaceship
        (list 'name (list 'serenity))
        (list 'class (list 'firefly)))
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  (list 2 '* (list (list 20 '- (list 91 '/ 7)) '* (list 45 '- '42)))  
)

;======================================12=======================================
(define example '(a b c))

;(d a b c)
(define (p12-1 lst)
  (cons 'd lst)
)

;(a b d a b)
(define (p12-2 lst)
  (cons (car lst) (cons (car (cdr lst)) (cons 'd (cons (car lst) (cons (car (cdr lst)) '())))))
)

;(b c d a)
(define (p12-3 lst)
  (cons (cadr lst) (cons (car (cddr lst)) (cons 'd (cons (car lst) '()))))
)


;======================================13=======================================
(define p13
  "eq? is true for same values and references. equal? is true for same values
   and references, and also checks element-wise in lists."
)
; write your answer as a string; you do not need to write any special escape
; characters to distinguish new lines.


;======================================14=======================================
(define (create-error-msg sym val)
  (string-append "This is a custom error message we will be using next. Symbol '" (symbol->string sym) " was not paired with value " (number->string 42))
)
;======================================15=======================================
(define (check-correctness pair)
  (if (eq? (car pair) 'answer-to-everything)
      (if (eq? (car (cdr pair)) 42)
          #t
          (raise (create-error-msg (car pair) (cadr pair))))
      #f)  
)

;======================================16=======================================
;No answer necessary - just experiment it as instructed in hw01.txt file

