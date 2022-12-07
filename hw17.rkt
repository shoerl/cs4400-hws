#lang pl 17

(define-type Token = (U Symbol Integer))

;; A macro that defines a DFA language
(define-syntax automaton
  (syntax-rules (: ->)
    [(automaton init-state end-state
       [state : (input-sym -> new-state) ...]
       ...)
     (lambda (string)
       (: state : (Listof Token) -> Boolean)
       ...
       (define (state stream)
         (match stream
           ['() (if (symbol=? 'state 'end-state) #t #f)]
           [(cons 'input-sym more) (new-state more)]
           ...
           [_ #f]))
       ...
       (init-state (explode-string string)))]))

(: cXr : String -> Boolean)
;; Identifies strings that match "c[ad]*r+"
(define cXr (automaton init end ; is the accepting state
                       [init : (c -> more)]
                       [more : (a -> more)
                             (d -> more)
                             (r -> end)]
                       [end  : (r -> end)]))

;; tests:
(test (cXr "cadr"))
(test (cXr "cadadadadadadddddaaarrr"))
(test (not (cXr "ccadr")))
(test (not (cXr "c")))

(: div5 : String -> Boolean)
;; Determine whether a binary number is divisible by 5
(define div5
  (automaton mod0 mod0
    [mod0 : (0 -> mod0) (1 -> mod1)]
    [mod1 : (0 -> mod2) (1 -> mod3)]
    [mod2 : (0 -> mod4) (1 -> mod0)]
    [mod3 : (0 -> mod1) (1 -> mod2)]
    [mod4 : (0 -> mod3) (1 -> mod4)]))

(test (div5 ""))
(test (div5 "0"))
(test (div5 "000"))
(test (div5 (number->string 12345 2)))
(test (not (div5 (number->string 123453 2))))

(define-syntax pushdown
  (syntax-rules (: ->)
                [(pushdown init-state end-state
                           [state: ((input-stream ...)
                                    (input-stack ...)
                                    ->
                                    new-state
                                    (new-stack ...)) ...] ...)
                 (lambda (string)
                   (: state : (Listof Token) (Listof Token) -> Boolean)
                   (define (state stream stack)
                     (match (list stream stack)
                       [(list '() '()) (if (symbol=? 'state 'end-state) #t #f)]
                       [(list (list-rest 'input-stream ... more-input)
                              (list-rest 'input-stack  ... more-stack))
                        (new-state more-input (list* 'new-stack ... more-stack))]
                       ...
                       [_ #f]))
                   ...
                   (init-state (append (explode-string string) '(*)) '(*)))]))
                   
                

(: balanced : String -> Boolean)
;; Identifies strings that contain only balanced parentheses
(define balanced (pushdown init init
                   [init : ((open)  ()     -> init (open))
                           ((close) (open) -> init ())
                           ((*)     (*)    -> init ())]))
;; tests:
(test (balanced ""))
(test (balanced "()"))
(test (balanced "(((())))"))
(test (balanced "((()())(()))"))
(test (not (balanced "(")))
(test (not (balanced ")")))
(test (not (balanced ")(")))