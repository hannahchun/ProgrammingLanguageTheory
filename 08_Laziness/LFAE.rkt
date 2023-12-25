#lang plai
; LFAE part2
(define-type LFAE
  [num (n number?)]
  [add (lhs LFAE?) (rhs LFAE?)]
  [sub (lhs LFAE?) (rhs LFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body LFAE?)]
  [app (ftn LFAE?) (arg LFAE?)])

; LFAE-Value
(define-type LFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body LFAE?) (ds DefrdSub?)]
  [exprV (expr LFAE?) (ds DefrdSub?)
         (value (box/c (or/c false LFAE-Value?)))]) ; store the value of the argument expression
; box contains either two types of values : false or LFAE-Value

; DefrdSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])

; parser
(define (parse sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse l) (parse r))]
    [(list '- l r)             (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?)               (id sexp)]
    [(list 'fun (list p) b)    (fun p (parse b))]
    [(list f a)                (app (parse f) (parse a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)]))

; num-op: (number number ->number) -> (FWAE FWAE -> FWAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n (strict x)) (numV-n (strict y))))))

(define num+ (num-op +))
(define num- (num-op -))

; strict: LFAE-Value -> LFAE-Value
(define (strict v)
  (type-case LFAE-Value v
    [exprV (expr ds v-box)
           (if (not (unbox v-box)) ; if box contains #f, evaluate argument expression and save that value in the box
               (local
                 [(define v (strict (interp expr ds)))] ; v : the value of our argument expression
                 (begin (set-box! v-box v)
                        v)) ; return v after evaluating it
               (unbox v-box))] ; if box contains a value, just unbox to return the value that was already evalauted once
    [else v])) ; for numV or closureV

; the 'local' block is for definitions + body

; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          (strict v)
                          (lookup name saved))]))

; interpreter
; in app (f a), avoid evaluating 'a' but keep it as it is like ClosureV keeps 'ds'
; in app (f a), also need to apply strict function when interpreting 'f'
(define (interp lfae ds)
  (type-case LFAE lfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [sub (l r) (num- (interp l ds) (interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local
                 [(define ftn-v (strict (interp f ds)))
                  (define arg-v (exprV a ds (box #f)))]
                 (interp (closureV-body ftn-v)
                         (aSub (closureV-param ftn-v)
                               arg-v
                               (closureV-ds ftn-v)
                               )
                         )
                 )
         ]
    )
  )

; run: sexp -> LFAE-Value
; purpose: to run parse and interp in one queue.
(define (run sexp ds)
     (interp (parse sexp) ds))

(run '{{fun {x} {+ 1 x}} 10} (mtSub))
(run '{{fun {f} {f 1}} {fun {x} {+ x 1}}} (mtSub))
(run '{fun {y} {+ x y}} (mtSub))

(run '{{fun {x} 0} {+ 1 {fun {y} 2}}} (mtSub)) ; produces 0 as intended
(run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub)) ; produces 6 as intended
;(run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub)) ; produces error as intended

(parse '{{fun {f} {f 1}} {fun {x} {+ x 1}}})