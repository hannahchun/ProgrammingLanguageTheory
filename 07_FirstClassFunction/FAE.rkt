#lang plai
; FAE that supports deferred substitution

; FAE
(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [mul (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (ftn FAE?) (arg FAE?)]
  [if0 (test-expr FAE?) (then-expr FAE?) (else-expr FAE?)])

; FAE-Value
(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

; DefrdSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

; Parser
; [contract] parse : sexp -> FAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'if0 ex1 ex2 ex3) (if0 (parse ex1) (parse ex2) (parse ex3))]
    [else (error 'parse "bad syntax:~a" sexp)]))

(parse '{with {x 3} {+ x x}})
(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})

; num-op
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))

; lookup : symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]
    )
  )

; numzero? FAE-Value -> boolean
(define (numzero? n)
  (zero? (numV-n n)))

; Interpreter
; [contract] FAE DefrdSub -> FAE-Value
(define (interp fae ds)
  (type-case FAE fae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [mul (l r) (num* (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local
                 [(define f-val (interp f ds))
                  (define a-val (interp a ds))]
                 ; function definition is saved in closureV type
                 (interp (closureV-body f-val)
                         (aSub (closureV-param f-val)
                               a-val
                               (closureV-ds f-val))
                         )
                 )
         ]
    [if0 (test-expr then-expr else-expr) (if (numzero? (interp test-expr ds))
                                             (interp then-expr ds)
                                             (interp else-expr ds))]
    )
  )

(define (run expr ds)
  (interp (parse expr) ds))

; (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})
; (interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub))
; (test (interp (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)) (mtSub)) (numV 7))
(run '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}} (mtSub))
; we now get the intended result which is (numV 7)

; (parse '{with {y 10} {fun {x} {+ y x}}})
; (interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))
; (test (interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))(closureV 'x (add (id 'y) (id 'x)) (aSub 'y (numV 10) (mtSub))))
(run '{with {y 10} {fun {x} {+ y x}}} (mtSub))

(run '{with {y 3} {with {z {fun {x} {+ x y}}} {with {y 10} z}}} (mtSub))


; (parse '{with {fac {fun {n}  {if0 n  1  {* n {fac {- n 1}}}}}}  {fac 10}})
; (run '{with {fac {fun {n}  {if0 n  1  {* n {fac {- n 1}}}}}}  {fac 10}} (mtSub))
; produces free identifer error as out current language does not support recursion.