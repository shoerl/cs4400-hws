#lang pl 06

;; ** The Brang interpreter, using environments

#|
The grammar:
  <BRANG> ::= <num>
            | (listof <BRANG>)
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> ... } <BRANG> }
            | { call <BRANG> <BRANG> }

Evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(x,env)                = lookup(x,env)
  eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)
           = eval(Ef,extend(x,eval(E2,env1),env2))
                             if eval(E1,env1) = <{fun {x} Ef}, env2>
           = error!          otherwise
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Fun  (Listof Symbol) BRANG]
  [Call BRANG (Listof BRANG)])


;; ** The Core interpreter, using environments

#|
The grammar:
  <CORE> ::= <num>
            | { + <CORE> <CORE> }
            | { - <CORE> <CORE> }
            | { * <CORE> <CORE> }
            | { / <CORE> <CORE> }
            | { with { <id> <CORE> } <CORE> }
            | <id>
            | { fun { <id> } <CORE> }
            | { call <CORE> <CORE> }

|#

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef  Natural]
  [CFun CORE]
  [CCall CORE CORE])

(: get-list : (Listof Sexpr) -> (Listof BRANG))
(define (get-list li-sexpr)
  (if (null? li-sexpr) '() (cons (parse-sexpr (first li-sexpr)) (get-list (rest li-sexpr)))))

(: parse-sexpr : Sexpr ->  BRANG)
;; parses s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
;;    [(cons (number: n) more) (cons (Num n) (parse-sexpr more))]
    [(symbol: name) (Id name)]
;;    [(cons (symbol: name) more) (cons (Id name) (parse-sexpr more))]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more) 
     (match sexpr
       [(list 'fun (list (symbol: names) ...) body)
        (Fun names (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(cons 'call more)
     (match more
       [(list fun args ...)
        (match args
          [(list xprs ...) (Call (parse-sexpr fun) (get-list xprs))]
          [else (error 'parse-sexpr "You screwed up")])]
       [else (error 'parse-sexpr "lets go")])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function


(define-type ENV
  [EmptyEnv]
  [RefRest VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

(define-type DE-ENV
  [DEmptyEnv]
  [DRest Symbol Natural DE-ENV])

(define de-empty-env (DEmptyEnv))

(: de-extend : DE-ENV Symbol -> DE-ENV)
(define (de-extend denv sym)
  (: de-extend-internal : DE-ENV Symbol Natural -> DE-ENV)
  (define (de-extend-internal denv sym count)
    (cases denv
      [(DEmptyEnv) (DRest sym count (DEmptyEnv))]
      [(DRest osym nat odenv) (DRest osym nat (de-extend-internal odenv sym (+ 1 count)))]))
  (de-extend-internal denv sym 0))

(: de-find : DE-ENV Symbol -> Natural)
(define (de-find denv sym)
  (cases denv
    [(DEmptyEnv) (error 'de-find "symbol does not exist")]
    [(DRest osym nat odenv) (if (equal? sym osym) nat (de-find odenv sym))]))

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

(: extend : VAL ENV -> ENV)
(define (extend id env)
  (cases env
    [(EmptyEnv) (RefRest id (EmptyEnv))]
    [(RefRest v e) (RefRest v (extend id e))]))

(: eval : CORE ENV -> VAL)
;; evaluates BRANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef id)
     (cases env
       [(EmptyEnv) (error  'eval "uh oh")]    
       [(RefRest v e) (if (equal? id 0) v (eval (CRef (- id 1)) e))])]
    [(CFun bound-body)
     (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body
                (extend (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))



(: preprocess : BRANG DE-ENV -> CORE)
(define (preprocess expr deenv)
  (cases expr
    [(Num n) (CNum n)]
    [(Add l r) (CAdd (preprocess l deenv) (preprocess r deenv))]
    [(Sub l r) (CSub (preprocess l deenv) (preprocess r deenv))]
    [(Mul l r) (CMul (preprocess l deenv) (preprocess r deenv))]
    [(Div l r) (CDiv (preprocess l deenv) (preprocess r deenv))]
    [(With bound-id named-expr bound-body)
     (let ([newenv (de-extend deenv bound-id)])
      (CCall (CFun (preprocess bound-body newenv)) (preprocess named-expr deenv)))]
    [(Id name) (CRef (de-find deenv name))]
    [(Fun bound-id bound-body)
    (cond
       [(null? bound-id) (preprocess bound-body deenv)]
       [else (let ([newenv (de-extend deenv (first bound-id))])
                  (CFun (preprocess (Fun (rest bound-id) bound-body) newenv)))])]
    [(Call fun-expr arg-expr)
     (cond
       [(null? arg-expr) (preprocess fun-expr deenv)]
       [else (CCall (preprocess (Call fun-expr (rest arg-expr)) deenv) (preprocess (first arg-expr) deenv))])]))


(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) (DEmptyEnv)) (EmptyEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))



;; basic arithmatic tests


;(test (run "{with {add3 {fun {x} {+ x 3}}}
;              {call add3 1}}")
;      => 4)

(test (run "{call {call {fun {x} {fun {y} {+ x y}}} 1} 5}") => 6)

(test (run "{call {fun {x y} {+ x y}} 1 5}") => 6)
(test (run "{call {fun {x y} {* x y}} 6 5}") => 30)
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{- 6 5}") => 1)
(test (run "{- 5 9}") => -4)
(test (run "{/ 25 5}") => 5)
(test (run "{/ 25 {* 3 4}}") => 25/12)
(test (run "{* {+ {/ 30 6} -2} {- 6 4}}") => 6)

;;;; tests
(test (run "{call {fun {x} {+ x 1}} 4}") => 5)
(test (run "{with {x 5} {+ x 100}}") => 105)

(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)

;(test (run "{with {add3 {fun {x} {+ x 3}}}
;              {with {add1 {fun {x} {+ x 1}}}
;                {with {x 3}
;                  {call add1 {call add3 x}}}}}")
;      => 7)
;(test (run "{with {identity {fun {x} x}}
;              {with {foo {fun {x} {+ x 1}}}
;                {call {call identity foo} 123}}}")
;      => 124)
;(test (run "{with {x 3}
;              {with {f {fun {y} {+ x y}}}
;                {with {x 5}
;                  {call f 4}}}}")
;      => 7)
;(test (run "{call {with {x 3}
;                    {fun {y} {+ x y}}}
;                  4}")
;      => 7)
;(test (run "{with {f {with {x 3} {fun {y} {+ x y}}}}
;              {with {x 100}
;                {call f 4}}}")
;      => 7)
;(test (run "{call {call {fun {x} {call x 1}}
;                        {fun {x} {fun {y} {+ x y}}}}
;                  123}")
;      => 124)