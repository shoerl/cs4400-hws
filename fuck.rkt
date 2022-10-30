#lang pl

(: ccons : (All (A) A -> ((Listof A) -> (Listof A))))
;; mimic Schlac's `cons', by currying `cons'
(define ccons (lambda (x) (lambda (l) (cons x l))))

(: append* : (All (A) (Listof (Listof A)) -> (Listof A)))
;; consumes a list of lists, and appends them all to a single list
(define append*
  (lambda (lists)
    (if (null? lists)
        lists
        (if (null? (car lists))
            (append* (cdr lists))
            (cons (car (car lists))
                  (append* (cons (cdr (car lists))
                                 (cdr lists))))))))

(: insertat : (All (A) A (Listof A) Number -> (Listof A)))
(define insertat
  (lambda (x list n)
    (if (zero? n) (cons x list) (cons (car list) (insertat x (cdr list) (- n 1))))))

(: interleave-helper : (All (A) A (Listof A) Number -> (Listof (Listof A))))
(define interleave-helper
  (lambda (x lis n)
    (if (equal? n (length lis))
            (cons (insertat x lis n) null)
            (cons (insertat x lis n) (interleave-helper x lis (+ n 1))))))

(: interleave : (All (A) A -> (Listof A) -> (Listof (Listof A))))
(define interleave
  (lambda (x)
    (lambda (list)
      (interleave-helper x list 0))))

(test ((interleave 0) (cons 1 (cons 2 null))) => '((0 1 2) (1 0 2) (1 2 0)))

;
;(define/rec permutations
;  (lambda (list)
;    (if (null? list)
;        (cons null null)
;        
;    (permutations-help list 0 null)))
  
(: permutations : (All (A) (Listof A) -> (Listof (Listof A))))
;; returns a list of all possible permutations of the input list
(define permutations
  (lambda (list)
    (if (null? list)
        (cons null null)
        (cons (append* ((interleave (first list)) (rest list))) (permutations (rest list))))))
    
    ;; hint: use `append*', `interleave', and recursive calls to
    ;; `permutations' here

 (test (permutations (cons 1 (cons 2 (cons 3 null)))) => '((0 1 2) (1 0 2) (1 2 0)))