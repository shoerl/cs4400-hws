#lang pl 06

#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | { bind { { <id> .. } { <BRANG> .. } } <BRANG> }
            | <id>
            | { fun { <id> <id> ... } <BRANG> }
            | { call <BRANG> <BRANG> <BRANG> ... }

Core evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(CRef(N),env)          = list-ref(env,N)
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)    = eval(Ef,cons(eval(E2,env1),env2))
                               if eval(E1,env1) = <{fun {x} Ef}, env2>
                             = error!  otherwise
Note that these rules are incomplete, since they don't represent the
language that users actually see.
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Bind (Listof Symbol) (Listof BRANG) BRANG]
  [Bind* (Listof Symbol) (Listof BRANG) BRANG]
  [Fun  (Listof Symbol) BRANG]
  [Call BRANG (Listof BRANG)])

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef  Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> BRANG)
;; parses s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]


    
    [(cons (or 'bind 'bind*) more)
     (match sexpr
       [(list (symbol: binder) exprs body)
        (println exprs)
        (if (eq? binder 'bind)
            (Num 5)
            (Num 10))])]


    
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: names) ...) body)
        (if (null? names)
          (error 'parse-sexpr "`fun' with no arguments in ~s" sexpr)
          (Fun names (parse-sexpr body)))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(cons 'call more)
     (match sexpr
       [(list 'call fun arg args ...)
        (Call (parse-sexpr fun) (map parse-sexpr (cons arg args)))]
       [else (error 'parse-sexpr "missing arguments to `call' in ~s"
                    sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: unzip-binds : (All (A B) (Listof (List A B)) -> (List (Listof A) (Listof B))))
(define (unzip-binds binds)
  (if (null? binds) '(()())
      (let ([vals (first binds)] [next (unzip-binds (rest binds))])
        (list (cons (first vals)(first next))(cons (cadr vals) (cadr next))))))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; These are the values of our language
(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

;; An environment is a simple list of values
(define-type ENV = (Listof VAL))

;; Syntactic environments for the de-Bruijn preprocessing:
;; define a type and an empty environment

(define-type DE-ENV = Symbol -> Natural)

(: de-empty-env : DE-ENV)
;; the empty syntactic environment, always throws an error
(define (de-empty-env id)
  (error 'de-env "Free identifier: ~s" id))

(: de-extend : DE-ENV Symbol -> DE-ENV)
;; extends a given de-env for a new identifier
(define (de-extend env id)
  (lambda (name)
    (if (eq? id name)
      0
      (+ 1 (env name)))))
;; tests, only for demonstration:
(define e1 (de-extend de-empty-env 'b))
(define e2 (de-extend e1 'a))
(test (e1 'a) =error> "Free identifier") ; e1 has no mapping for 'a
(test (e1 'b) => 0)                      ; and 'b is mapped to 0
(test (e2 'a) => 0)                      ; e2 maps 'a to 0
(test (e2 'b) => 1)                      ; and now 'b is mapped to 1

(: preprocess : BRANG DE-ENV -> CORE)
;; replaces identifier expressions into Ref AST values
(define (preprocess expr de-env)
  (: sub : BRANG -> CORE)
  (define (sub expr) (preprocess expr de-env))
  (cases expr
    [(Num n)   (CNum n)]
    [(Add l r) (CAdd (sub l) (sub r))]
    [(Sub l r) (CSub (sub l) (sub r))]
    [(Mul l r) (CMul (sub l) (sub r))]
    [(Div l r) (CDiv (sub l) (sub r))]
    [(Bind syms exprs body) (println expr) (CNum 0)]
    [(Bind* syms exprs body) (println expr) (CNum 1)]
    [(With bound-id named-expr bound-body)
     ;; (CCall (sub (Fun (list bound-id) bound-body))
     ;;        (sub named-expr))
     ;; Better alternative:
     (sub (Call (Fun (list bound-id) bound-body) (list named-expr)))]
    [(Id name) (CRef (de-env name))]
    [(Fun bound-ids bound-body)
     ;; note that bound-ids are never empty
     (if (= 1 (length bound-ids))
       (CFun (preprocess bound-body
                         (de-extend de-env (first bound-ids))))
       ;; similar choice to the above here
       (sub (Fun (list (first bound-ids))
                 (Fun (rest bound-ids) bound-body))))]
    [(Call fun-expr arg-exprs)
     ;; note that arg-exprs are never empty
     (if (= 1 (length arg-exprs))
       (CCall (sub fun-expr) (sub (first arg-exprs)))
       ;; and a similar choice here too
       (sub (Call (Call fun-expr (list (first arg-exprs)))
                  (rest arg-exprs))))]))

(: NumV->number : VAL -> Number)
;; convert a FLANG runtime numeric value to a Racket one
(define (NumV->number val)
  (cases val
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" val)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : CORE ENV -> VAL)
;; evaluates CORE expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef n) (list-ref env n)]
    [(CFun bound-body) (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body (cons (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) de-empty-env) null)])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)

;; test remaining arithmetic functions
(test (run "{call {fun {x} {- x 1}} 4}")
      => 3)
(test (run "{call {fun {x} {* x 3}} 4}")
      => 12)
(test (run "{call {fun {x} {/ x 2}} 4}")
      => 2)

;; test errors
(test (run "{call {fun {x} {? x 1}} 4}")
      =error> "bad syntax in")
(test (run "{call {fun {x} {+ y 1}} 4}")
      =error> "Free identifier: y")
(test (run "{call {fun {x} } 4}")
      =error> "bad `fun' syntax")
(test (run "{call {fun {x} } 4}")
      =error> "bad `fun' syntax")
(test (run "{fun {} 1}")
      =error> "`fun' with no arguments")
(test (run "{with {y} }")
      =error> "bad `with' syntax")
(test (run "{fun {x} {+ x x}}")
      =error> "evaluation returned a non-number")
(test (run "{+}")
      =error> "bad syntax in (+)")
(test (run "{+ {fun {x} x} 1}")
      =error> "arith-op: expected a number")
(test (run "{call 1 1}")
      =error> "expects a function")
(test (run "{call {fun {x} x}}")
      =error> "missing arguments to `call'")

;; test multiple-argument functions
(test (run "{with {add {fun {x y} {+ x y}}} {call add 7 8}}")
      => 15)
(test (run "{with {add {fun {x y} {- x y}}} {call add 10 4}}")
      => 6)

;; Problem 1: This shouldn't work (implicitly currying a two function argument)
(test (run "{with {add {fun {x y} {- x y}}} {call {call add 10} 5}}")
      => 5)

;; Problem 2: Overriding function arguments
(test (run "{with {x 4} {with {add {fun {x y} {+ x y}}} {call add x x}}}") => 8)

(test (run "{bind {{x 1} {y 2}} {+ x y}}") => 3)

