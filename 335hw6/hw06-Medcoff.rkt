#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )

;write your answer in string form:
(define problem-1-answer
  "Tokens are generated based on a few rules. Every whitespace character is
   ignored. A string of arbitrary length contained on one line and beginning
   with a pound sign #, is considered one comment and is ignored. A string of
   sequential digits forms a number. A string of sequential digits preceded by
   a minus sign forms a number. A string containing letters, digits, underscore,
   minus, or question mark but necessarily beginning with a letter is considered
   an identifier."
  )

(define grammar-spec
  '(
    (program (expr (arbno expr)) a-program)
    (expr (number) num-expr)
    (expr ("up" "(" expr ")") up-expr)
    (expr ("down" "(" expr ")") down-expr)
    (expr ("left" "(" expr ")") left-expr)
    (expr ("right" "(" expr ")") right-expr)
    (expr ("(" expr expr ")") point-expr)
    (expr ("+" expr expr) add-expr)
    (expr ("origin?" "(" expr ")") origin-expr)
    (expr ("if" "(" expr ")" "then" expr "else" expr) if-expr)
    (expr ("move" "(" expr expr (arbno expr) ")") move-expr)
    (expr (identifier) iden-expr)
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    (var-expr ("val" identifier "=" expr) val)
    (var-expr ("final val" identifier "=" expr) final-val)))

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
  (flatten (list el1 rest))
  )
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;parser function.
(define (run program-string)
  (if (string? program-string)
      (value-of (make-ast program-string) (empty-env))
      (invalid-args-exception "program-string" "string?" "not-a-string")))

(define (value-of ast env)
  (cond
    ((program? ast) (value-of-program ast env))
    ((expr? ast) (value-of-expr ast env))
    ((var-expr? ast) (value-of-var-expr ast env))
    (else (invalid-args-exception "value-of" "ast?" "not-an-ast"))))

; Helper function to clean up code.
(define make-ast
  (sllgen:make-string-parser lexical-spec grammar-spec))

(define (value-of-program p env)
  (cases program p
    (a-program (first-expr rest-expr)
               (andmap (lambda (x) (value-of x env))
               (flat-list first-expr rest-expr)))))

(define (value-of-expr e env)
  (cases expr e
    (num-expr (number) (num-val number))
    (up-expr (number) (step-val (up-step (num-val->n (value-of number env)))))
    (down-expr (number) (step-val (down-step (num-val->n (value-of number env)))))
    (left-expr (number) (step-val (left-step (num-val->n (value-of number env)))))
    (right-expr (number) (step-val (right-step (num-val->n (value-of number env)))))
    (iden-expr (id) (apply-env env id))
    (point-expr (x y) (point-val (point (num-val->n (value-of x env))
                                        (num-val->n (value-of y env)))))
    (move-expr (point-expr first rest)
               (letrec ((xi (point-val->p (value-of point-expr env)))
                        (moves (map step-val->st
                                    (map (lambda (x) (value-of x env))
                                         (flat-list first rest))))
                        (xf (foldl move xi moves)))
                 (point-val xf)))
    (add-expr (a b)
              
                  (letrec
                      ((a-step (step-val->st (value-of a env)))
                       (b-step (step-val->st (value-of b env)))
                       (mag (+ (get-signed-val a-step) (get-signed-val b-step))))
                    (cond
                      ((or (up-step? a-step) (down-step? a-step))
                       (make-y-step mag))
                      ((or (left-step? a-step) (right-step? a-step))
                       (make-x-step mag))
                       ))
                  )
    (origin-expr (pt)
                 (bool-val (equal? (point 0 0) (point-val->p
                                                (value-of pt env)))))
    (if-expr (c t e)
             (if (bool-val->b (value-of c env))
                 (value-of t env)
                 (value-of e env)))
    (block-expr (var-exprs exprs)
                (andmap (lambda (x) (value-of x (block-env var-exprs env))) exprs))))

(define (value-of-var-expr v env)
  (if (not (var-expr? v))
      (invalid-args-exception "value-of-var-expr" "var-expr?" "not-a-var-expr")
      (cases var-expr v
        (val (id val) (extend-env-wrapper id (value-of val env) env NON-FINAL))
        (final-val (id val) (extend-env-wrapper id (value-of val env) env FINAL))
        (else ("value-of-var-expr" "var-expr?" "not-a-var-expr")))))

(define (move st pt)
  (cases step st
    (up-step (st) (point (point->x pt) (+ (point->y pt) st)))
    (down-step (st) (point (point->x pt) (+ (point->y pt) (* -1 st))))
    (left-step (st) (point (+ (point->x pt) (* -1 st)) (point->y pt)))
    (right-step (st) (point (+ (point->x pt) st) (point->y pt)))))

(define (valid-addition? a b)
  (or
   (and (up-step? a) (up-step? b))
   (and (up-step? a) (down-step? b))
   (and (down-step? a) (up-step? b))
   (and (down-step? a) (down-step? b))
   (and (left-step? a) (left-step? b))
   (and (left-step? a) (right-step? b))
   (and (right-step? a) (left-step? b))
   (and (right-step? a) (right-step? b))))

(define (get-signed-val st)
  (cases step st
    (up-step (n) n)
    (right-step (n) n)
    (down-step (n) (* -1 n))
    (left-step (n) (* -1 n))))

(define (make-x-step mag)
  (if (positive? mag)
      (step-val (right-step mag))
      (step-val (left-step (abs mag)))))

(define (make-y-step mag)
  (if (positive? mag)
      (step-val (up-step mag))
      (step-val (down-step (abs mag)))))

(define (block-env lst env)
  (if (null? lst)
      env
      (block-env (cdr lst) (new-var (car lst) env))))

(define (new-var var env)
  (cases var-expr var
    (final-val (id val) (extend-env-wrapper id (value-of val env) env FINAL))
    (val (id val) (extend-env-wrapper id (value-of val env) env NON-FINAL))))


;for each different ast node type, e.g. <program>, <expr>, <var-expr> you might
;consider implementing a function with the outline:
#|
(define (value-of-ast-node-type ast)
  (cases ast-node-type ast
    (ast-node-type-variant
     (f1 f2)
     'UNIMPLEMENTED
     )
    (else (raise (~a "value-of-ast-node-type error: unimplemented expression: " ast)))
    )
  )
|#
;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST. Take some time to read it.
;you should be able to understand it by now.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;parser is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree (ast). 
(define parser
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the create-ast
;function. This is a one-argument function that takes a 
;string.
(define scanner
  (sllgen:make-string-scanner lexical-spec grammar-spec))