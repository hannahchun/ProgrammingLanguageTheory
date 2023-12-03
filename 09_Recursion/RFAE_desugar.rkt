#lang plai
; L15 PPT
; RFAE using desugaring
; do not need to significantly update our interpreter
; code in concrete syntax is complex

(define-type RFAE
    [num    (n number?)]
    [add     (lhs RFAE?) (rhs RFAE?)]
    [sub     (lhs RFAE?) (rhs RFAE?)]
    [mul (lhs RFAE?) (rhs RFAE?)] ; * operator 
    [id         (name symbol?)]
    [fun      (param symbol?) (body RFAE?)]
    [app     (ftn RFAE?) (arg RFAE?)]
    [ifexp (test-expr RFAE?) (then-expr RFAE?)(else-expr RFAE?)] ; ifexp operator
    [orop (lhs RFAE?) (rhs RFAE?)] ;or operator
    [eqop (lhs RFAE?) (rhs RFAE?)]) ;equal operator

; 'RFAE-Value' type
(define-type RFAE-Value
  [numV     (n number?)]
  [closureV (param symbol?) (body RFAE?) (ds DefrdSub?)])

; 'DefrdSub' type
(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value RFAE-Value?) (ds DefrdSub?)])

; [contract]parse: sexp -> RFAE
; [purpose] to convert concrete syntax into abstract syntax, RFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list '* l r)              (mul (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)     (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [(list 'if ex1 ex2 ex3)     (ifexp (parse ex1) (parse ex2) (parse ex3))]
        [(list 'or l r)             (orop (parse l) (parse r))]
        [(list '= l r)              (eqop (parse l) (parse r))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

#|
; [test 1] parse
(test (parse '{with {fac{fun {n}
                             {if {= n 0}
                                 1
                                 {* n {fac {- n 1}}}}}}
                    {fac 10}})
      (app (fun 'fac (app (id 'fac) (num 10))) (fun 'n (ifexp (eqop (id 'n) (num 0)) (num 1) (mul (id 'n) (app (id 'fac) (sub (id 'n) (num 1))))))))

; [test 2] parse
(test (parse '{with {fib {fun {n}
                      {if {or {= n 0} {= n 1}}
                          1
                          {+ {fib {- n 1}}
                             {fib {- n 2}}}}}}
            {fib 10}})
      (app (fun 'fib (app (id 'fib) (num 10))) (fun 'n (ifexp (orop (eqop (id 'n) (num 0)) (eqop (id 'n) (num 1))) (num 1) (add (app (id 'fib) (sub (id 'n) (num 1))) (app (id 'fib) (sub (id 'n) (num 2))))))))
|#

; [contract] num-op : (number number -> number) -> (FAE FAE -> FAE)
; [purpose] takes an operator as an argument. returns a function that takes two 'FAE' expressions, extracts the numeric values from them and applies the operator, and produces result in numV type. 
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))

; [contract] lookup : symbol DefrdSub -> number
; [purpose] For the given symbol, find the corresponding value in the DefrdSub cache. If there is no symbol return free identifier error.
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]
    )
  )

; [contract]find-id : list(i is an identifier of is-recursion/check-id function, b is a function body from check-id) -> boolean
; [purpose]: find the same id in function body
(define (find-id i b)
    (match b
        [(? number?)                false]
        [(list '+ l r)                     (if(or (find-id i l) (find-id i r)) true false)]
        [(list '- l r)                      (if(or (find-id i l) (find-id i r)) true false)]
        [(list 'with (list i v) e)    (if(or (find-id i v) (find-id i e)) true false)]
        [(? symbol?)                (if(equal? i b) true false)]
        [(list 'fun (list p) body)     (if(or (find-id i p) (find-id i body)) true false)]
        [(list f a)                       (if(or (find-id i f) (find-id i a)) true false)]
        [(list 'if c t f)          (if(or (find-id i t) (find-id i f)) true false)] 
        [(list '* l r)             (if(or (find-id i l) (find-id i r)) true false)]
        [(list 'or (list l r))     (if(or (find-id i l) (find-id i r)) true false)]
        [(list '= l r)             (if(or (find-id i l) (find-id i r)) true false)]
        [else                             false]))

; [contract]check-id :list(function identifier(i) , function body(a)) -> boolean
; [purpose]: check pattern of function in function body
(define (check-id i a) 
 (match a
   [(list 'fun (list p) b) (find-id i b)] ; check redundancy of id(func name) in the function body
   [else  false]
   )
  )

; [contract]is-recursion : sexp -> boolean
; [purpose]: to confirm sexp is recursion or not
(define (is-recursion sexp)
  (match sexp
    [(list 'with (list i a) (list f b)) (if (check-id i a) true false)]  
    [else false ]))

#|
; [test 1] is-recursion                           
(is-recursion '{with {fac {fun {n}
                            {if {= n 0}
                                   1
                             {* n {fac {- n 1}}}}}}
                     {fac 10}})

;[test 2] is-recursion  
(is-recursion '{with {fib {fun {n}
                          {if {or {= n 0} {= n 1}}
                              1
                              {+ {fib {- n 1}}
                             {fib {- n 2}}}}}}
      {fib 10}})
|#


;[contract] desugar : list -> list
;[purpose]: to make sugar sexp ->  desugar sexp
(define (desugar sexp)
  (match sexp
    [(list 'with (list i (list 'fun (list n) e)) (list i num))
     (list 'with (list 'mk-rec (list 'fun (list 'body-proc)
                                     (list 'with (list 'fX (list 'fun (list 'fY)
                                                                 (list 'with (list 'f (list 'fun (list 'x)
                                                                                            (list '(fY fY) 'x)))
                                                                       '(body-proc f))))
                                           '(fX fX))))
           (list 'with (list i (list 'mk-rec (list 'fun (list i) (list 'fun (list n) e)))) (list i num)))]))

#|
; [test 1] desugar
(desugar '{with {fac {fun {n}
                            {if {= n 0}
                                   1
                             {* n {fac {- n 1}}}}}}
      {fac 10}})

; [test 2] desugar
(desugar '{with {fib {fun {n}
                          {if {or {= n 0} {= n 1}}
                              1
                              {+ {fib {- n 1}}
                             {fib {- n 2}}}}}}
      {fib 10}})
|#

; interpreter
; [contract]interp: RFAE DefrdSub -> RFAE-Value 
(define (interp rfae ds)
    (type-case RFAE rfae
      [num (n)          (numV n)]
      [add  (l r)         (num+ (interp l ds) (interp r ds))]
      [sub  (l r)         (num- (interp l ds) (interp r ds))]
      [mul (l r)         (num* (interp l ds) (interp r ds))]
      [id  (s)  (lookup s ds)]
      [fun (p b)  (closureV p b ds)]
      [app (f a) (local
                   [(define f-val (interp f ds))
                    (define a-val (interp a ds))]
                   (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val))
                           ))
                   ]
      [ifexp (test-expr then-expr else-expr) (if (interp test-expr ds) (interp then-expr ds) (interp else-expr ds))]
      [orop (l r) (or (interp l ds) (interp r ds))]
      [eqop (l r) (equal? (interp l ds) (interp r ds))])
      )

; [contract] run : expr DefrdSub -> RFAE-Value
; [purpose] receives code written in concrete syntax and deferred substitution cache. Converts the expression into abstract syntax using parser and produces corresponding result in FAE-Value type using interpereter.
(define (run sexp ds)
     (if (equal? (is-recursion sexp) true)
         (interp (parse (desugar sexp)) ds)
         (interp (parse sexp) ds)))

; [test 1]
; (desugar '{with {fac {fun {n} {if {= n 0} 1 {* n {fac {- n 1}}}}}} {fac 4}})
; (parse (desugar '{with {fac{fun {n} {if {= n 0} 1 {* n {fac {- n 1}}}}}} {fac 4}}))
; (interp (parse (desugar '{with {fac{fun {n} {if {= n 0} 1 {* n {fac {- n 1}}}}}} {fac 4}})) (mtSub))

(run '{with {fac{fun {n}
                     {if {= n 0}
                         1
                         {* n {fac {- n 1}}}}}}
            {fac 4}} (mtSub))

; [test 2]
; (desugar '{with {fib {fun {n} {if {or {= n 0} {= n 1}} 1 {+ {fib {- n 1}} {fib {- n 2}}}}}} {fib 10}})
; (parse (desugar '{with {fib {fun {n} {if {or {= n 0} {= n 1}} 1 {+ {fib {- n 1}} {fib {- n 2}}}}}} {fib 10}}))
; (interp (parse (desugar '{with {fib {fun {n} {if {or {= n 0} {= n 1}} 1 {+ {fib {- n 1}} {fib {- n 2}}}}}} {fib 10}})) (mtSub))

(run '{with {fib {fun {n}
                      {if {or {= n 0} {= n 1}}
                          1
                          {+ {fib {- n 1}}
                             {fib {- n 2}}}}}}
            {fib 10}} (mtSub))

; [test 3]
; (desugar '{with {count {fun {n} {if {= n 0} 0 {+ 1 {count {- n 1}}}}}} {count 8}})
; (parse (desugar '{with {count {fun {n} {if {= n 0} 0 {+ 1 {count {- n 1}}}}}} {count 8}}))
; (interp (parse (desugar '{with {count {fun {n} {if {= n 0} 0 {+ 1 {count {- n 1}}}}}} {count 8}})) (mtSub))

(run '{with {count {fun {n}
                        {if {= n 0}
                            0
                            {+ 1 {count {- n 1}}}}}}
            {count 8}} (mtSub))

; [test 4]
; (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(run '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}} (mtSub))
