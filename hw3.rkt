;; ** The MUWAE interpreter

#lang pl 03

#| BNF for the MUWAE language:
     <MUWAE> ::= <num>
             | { + <MUWAE> <MUWAE> }
             | { - <MUWAE> <MUWAE> }
             | { * <MUWAE> <MUWAE> }
             | { / <MUWAE> <MUWAE> }
             | { sqrt <MUWAE> }
             | { with { <id> <MUWAE> } <MUWAE> }
             | <id>
|#

;; MUWAE abstract syntax trees
(define-type MUWAE
             [Num (Listof Number)]
             [Add MUWAE MUWAE]
             [Sub MUWAE MUWAE]
             [Mul MUWAE MUWAE]
             [Div MUWAE MUWAE]
             [Sqrt MUWAE]
             [Id Symbol]
             [With Symbol MUWAE MUWAE])

(: parse-sexpr : Sexpr -> MUWAE)
;; parses s-expressions into MUWAEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num (list n))]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt lhs) (Sqrt (parse-sexpr lhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> MUWAE)
;; parses a string containing a MUWAE expression to a MUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      {sqrt E1}             = {sqrt E1[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: subst : MUWAE Symbol MUWAE -> MUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
         [(Num n) expr]
         [(Add l r) (Add (subst l from to) (subst r from to))]
         [(Sub l r) (Sub (subst l from to) (subst r from to))]
         [(Mul l r) (Mul (subst l from to) (subst r from to))]
         [(Div l r) (Div (subst l from to) (subst r from to))]
         [(Sqrt l) (Sqrt (subst l from to))]
         [(Id name) (if (eq? name from) to expr)]
         [(With bound-id named-expr bound-body)
          (With bound-id
                (subst named-expr from to)
                (if (eq? bound-id from)
                    bound-body
                    (subst bound-body from to)))]))

(: sqrt+ : (Listof Number) -> (Listof Number))
;; a version of `sqrt' that takes a list of numbers, and return a
;; list with twice the elements, holding the two roots of each of
;; the inputs; throws an error if any input is negative.
(define (sqrt+ ns)
  (cond
    [(null? ns) null]
    [(< (first ns) 0) (error 'eval "requires a non-negative input")]
    [else
     (let ([slv (sqrt (first ns))])
       (cons slv (cons (- 0 slv) (sqrt+ (rest ns)))))]))

(: bin-op
   :
   (Number Number -> Number)
   (Listof Number)
   (Listof Number)
   ->
   (Listof Number))
;; applies a binary numeric function on all combinations
;; of numbers from the two input lists, and return the
;; list of all of the results
(define (bin-op op ls rs)
  (: helper : Number (Listof Number) -> (Listof Number))
  (define (helper l rs)
    (: f : Number -> Number)
    (define (f r)
      (op l r))
    (map f rs))
  (if (null? ls)
      null
      (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

#| Formal specs for `eval':
     eval(N)         = N
     eval({+ E1 E2}) = eval(E1) + eval(E2)
     eval({- E1 E2}) = eval(E1) - eval(E2)
     eval({* E1 E2}) = eval(E1) * eval(E2)
     eval({/ E1 E2}) = eval(E1) / eval(E2)
     eval({sqrt E1}) = eval(E1) ^ (1/2)
     eval(id)        = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
|#

(: eval : MUWAE -> (Listof Number))
;; evaluates MUWAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
         [(Num n) n]
         [(Add l r) (bin-op + (eval l) (eval r))]
         [(Sub l r) (bin-op - (eval l) (eval r))]
         [(Mul l r) (bin-op * (eval l) (eval r))]
         [(Div l r) (bin-op / (eval l) (eval r))]
         [(Sqrt e) (sqrt+ (eval e))]
         [(With bound-id named-expr bound-body)
          (eval (subst bound-body bound-id (Num (eval named-expr))))]
         [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (Listof Number))
;; evaluate a MUWAE program contained in a string
(define (run str)
  (eval (parse str)))

(define minutes-spent 120)

;; tests

;; basic arithmatic tests
(test (run "5") => '(5))
(test (run "{+ 5 5}") => '(10))
(test (run "{- 6 5}") => '(1))
(test (run "{- 5 9}") => '(-4))
(test (run "{/ 25 5}") => '(5))
(test (run "{/ 25 {* 3 4}}") => '(25/12))
(test (run "{* {+ {/ 30 6} -2} {- 6 4}}") => '(6))

;; with & arithmetic tests
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))

;; error message tests
(test (run "{with {x 1} y}") =error> "free identifier")

(test (run "{with {x 3 2 4} {+ x 3}}")
      =error> "bad `with' syntax in (with (x 3 2 4) (+ x 3))")
(test (run "{dog {x 3 2 4} {+ x 3}}")
      =error> "bad syntax in (dog (x 3 2 4) (+ x 3))")
(test (run "{sqrt -1}") =error> "requires a non-negative input")

;; with tests using multi-valued expressions
(test (run "{with {x {sqrt 9}} {+ x 1}}") => '(4 -2))
(test (run "{with {x {sqrt 0}} {+ x 10}}") => '(10 10))
(test (run "{with {x {sqrt 9}} {with {y {sqrt 144}} {+ x y}}}")
      => '(15 -9 9 -15))
(test (run "{with {x {sqrt 9}} {+ x {sqrt 64}}}") => '(11 -5 5 -11))
(test (run "{with {x {sqrt 16}} {* 15 x}}") => '(60 -60))
(test (run "{with {x {sqrt 25}} {/ 400 x}}") => '(80 -80))

;; with edge case tests
(test (run "{with {x 5} {with {x 6} {+ x x}}}") => '(12))
(test (run "{with {x 4} {with {x 10} {- x x}}}") => '(0))
(test (run "{with {x 4} {with {x {sqrt 81}} {* x x}}}") => '(81 -81 -81 81))

;; sqrt tests
(test (run "{sqrt 9}") => '(3 -3))
(test (run "{sqrt 1}") => '(1 -1))
(test (run "{sqrt 0}") => '(0 0))
(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}") => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}")
      => '(5 -5 4 -4))
