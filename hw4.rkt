#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num  Number]
  [Add  (Listof ALGAE)]
  [Mul  (Listof ALGAE)]
  [Sub  ALGAE (Listof ALGAE)]
  [Div  ALGAE (Listof ALGAE)]
  [Id   Symbol]
  [With Symbol ALGAE ALGAE])

(: parse-sexpr : Sexpr -> ALGAE)
;; parses s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ args ...)     (Add (parse-sexprs args))]
    [(list '* args ...)     (Mul (parse-sexprs args))]
    [(list '- fst args ...) (Sub (parse-sexpr fst) (parse-sexprs args))]
    [(list '/ fst args ...) (Div (parse-sexpr fst) (parse-sexprs args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <ALGAE>s, `x' is some <id>, `y' is a
   *different* <id>)
      N[v/x]                = N
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)        expr]
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
             bound-body
             (subst* bound-body)))]))

#| Formal specs for `eval':
     eval(N)            = N
     eval({+ E ...})    = evalN(E) + ...
     eval({* E ...})    = evalN(E) * ...
     eval({- E})        = -evalN(E)
     eval({/ E})        = 1/evalN(E)
     eval({- E1 E ...}) = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...}) = evalN(E1) / (evalN(E) * ...)
     eval(id)           = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E) = eval(E) if it is a number, error otherwise
|#

(: eval-number : ALGAE -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr)
  (let ([result (eval expr)])
    (if (number? result)
      result
      (error 'eval-number "need a number when evaluating ~s, but got ~s"
             expr result))))

(: value->algae : (U Number) -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number? val) (Num val)]
        ;; Note: a `cond' doesn't make much sense now, but it will when
        ;; we extend the language with booleans.  Also, since we use
        ;; Typed Racket, the type checker makes sure that this function
        ;; is never called with something that is not in its type, so
        ;; there's no need for an `else' branch like
        ;;     [else (error 'value->algae "unexpected value: ~s" val)]
        ;; (Strictly speaking, there's no need for the last predicate
        ;; (which is the only one here until you extend this), but it's
        ;; left in for clarity.)
        ))

;; %%% What is this for? Why not use +?
(: do-add : Number Number -> Number)
(define (do-add a result)
  (+ a result))

(: list-sum : (Listof Number) -> Number)
;; returns the sum of a list of numbers
(define (list-sum num-list)
  (foldl + 0 num-list))

(: list-prod : (Listof Number) -> Number)
;; returns the product of a list of numbers
(define (list-prod num-list)
  (foldl * 1 num-list))

(: div-by-list : Number (Listof Number) -> Number)
;; divides a number by a list of numbers.
;; if the list contains a zero, returns an error.
(define (div-by-list base num-list)
  (let ([product (list-prod num-list)])
    (if (equal? product 0)
        (error 'eval "Cannot divide by zero: ~s / *~s" base num-list)
        (/ base product))))

(: eval : ALGAE -> (U Number))
;; evaluates ALGAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add args) (list-sum (map eval-number args))]
    [(Mul args) (list-prod (map eval-number args))]
    [(Sub fst args) (let ([base (eval-number fst)])
                      (if (null? args)
                          (- 0 base)
                          (- base (list-sum (map eval-number args)))))]
    [(Div fst args) (let ([base (eval-number fst)])
                      (if (null? args)
                          (/ 1 base)
                          (div-by-list base (map eval-number args))))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (U Number))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))

;; minutes spent
(define minutes-spent 0)

;; tests (for simple expressions)
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{- 1 17}") => -16)
(test (run "{* 4 18}") => 72)
(test (run "{/ 35 7}") => 5)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x 13} {* x x}}") => 169)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {with {z {/ 100 5}} {* y z}}}}") => 140)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)

;; error tests
(test (run "{+ x {with {x {+ 5 131}} x}}") =error> "free identifier: x")
(test (run "{jolly}") =error> "bad syntax in (jolly)")
(test (run "{with my best friend}") =error> " bad `with' syntax in (with my best friend)")

;; addition tests
(test (run "{+ 2 3}") => (+ 2 3))
(test (run "{+ 1 2 3 4}") => (+ 1 2 3 4))
(test (run "{+ -1 2 -3 4}") => (+ -1 2 -3 4))
(test (run "{+}") => (+))

;; multiplication tests
(test (run "{* 1 2 3 4}") => (* 1 2 3 4))
(test (run "{* -4 -1 -3 -5}") => (* -4 -1 -3 -5))
(test (run "{* 5}") => (* 5))
(test (run "{*}") => (*))

;; subtraction tests
(test (run "{- 1 2 3 4}") => (- 1 2 3 4))
(test (run "{- -4 -1 -3 -5}") => (- -4 -1 -3 -5))
(test (run "{- 5}") => (- 5))
(test (run "{- -5}") => (- -5))

;; division tests
(test (run "{/ 81 3 3}") => (/ 81 3 3))
(test (run "{/ 10}") => (/ 10))
(test (run "{/ 0 1 2 3 4}") => (/ 0 1 2 3 4))
(test (run "{/ 1000 10 10 0}") =error> "Cannot divide by zero")

