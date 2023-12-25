#lang plai
; BMFAE
(define-type BMFAE
  [num (n number?)]
  [add (lhs BMFAE?) (rhs BMFAE?)]
  [sub (lhs BMFAE?) (rhs BMFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BMFAE?)]
  [app (ftn BMFAE?) (arg BMFAE?)]
  [newbox (v BMFAE?)]
  [setbox (bn BMFAE?) (v BMFAE?)]
  [openbox (v BMFAE?)]
  [seqn (ex1 BMFAE?) (ex2 BMFAE?)]
  [setvar (name symbol?) (v BMFAE?)] ; variable
  )

; DefrdSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

; BMFAE-Value
; keep a memory address value of a box for static scope
(define-type BMFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BMFAE?) (ds DefrdSub?)]
  [boxV (address integer?)])

; Store
; track dynamic changes of boxes
(define-type Store
  [mtSto]
  [aSto (address integer?) (value BMFAE-Value?) (rest Store?)])

; Value*Store
; return type holds both the value and the storage information. 
(define-type Value*Store
  [v*s (value BMFAE-Value?) (store Store?)])

; parse : sexp -> BMFAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'newbox v) (newbox (parse v))]
    [(list 'setbox bn v) (setbox (parse bn) (parse v))]
    [(list 'openbox v) (openbox (parse v))]
    [(list 'seqn ex1 ex2) (seqn (parse ex1) (parse ex2))]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'setvar n v) (setvar n (parse v))] ; variable
    [else (error 'parse "bad syntax: ~a" sexp)])
  )

; num-op: (number number ->number) -> (BMFAE BMFAE -> BMFAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i adr saved) (if (symbol=? i name)
                            adr
                            (lookup name saved))]))

; store-lookup: address store -> BFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
          (if (= location address)
              value
              (store-lookup address rest-store))]))

; malloc: Store -> Integer
; to allocate memory for a new box
(define (malloc st)
  (+ 1 (max-address st)))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st) (max n (max-address st))]))

; interp-two
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))

; Interpreter
(define (interp expr ds st)
  (type-case BMFAE expr
    [num (n) (v*s (numV n) st)]
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [app (f a) (interp-two f a ds st
                           (lambda (f-value a-value st1)
                             (local ([define new-address (malloc st1)])
                               (interp (closureV-body f-value)
                                       (aSub (closureV-param f-value)
                                             new-address
                                             (closureV-ds f-value))
                                       (aSto new-address
                                             a-value
                                             st1)))))]

    [newbox (val)
            (type-case Value*Store (interp val ds st)
              [v*s (vl st1)
                   (local [(define a (malloc st1))]
                     (v*s (boxV a)
                          (aSto a vl st1)))])]
    
    [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val) st1) st1)])]

    [setbox (bx-expr val-expr)
            (interp-two bx-expr val-expr ds st
                        (lambda (bx-val val st1)
                          (v*s val
                               (aSto (boxV-address bx-val)
                                     val
                                     st1))))]
    [seqn (a b) (interp-two a b ds st
                            (lambda (v1 v2 st1) (v*s v2 st1)))]
    ; In original code, only one type-case is used. But we can still apply interp-two for the seqn branch because we are not using v1 but also v2
    [setvar (id val-expr) (local [(define a (lookup id ds))]
                            (type-case Value*Store (interp val-expr ds st)
                              [v*s (val st)
                                   (v*s val
                                        (aSto a val st))]))])
                            
    )

; run
(define (run sexp ds st)
     (interp (parse sexp) ds st))

; call by value
(run '{with {a 3} {setvar a 5}} (mtSub) (mtSto))
; (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (mtSto))))
(run '{with {a 3} {seqn {{fun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))
; (v*s (numV 3) (aSto 2 (numV 5) (aSto 2 (numV 3) (aSto 1 (numV 3) (mtSto)))))
; for identifier 'a', the address is still 1 and the value is still (numV 3)

; shows the basic concept of call by reference
(run '{with {a {newbox 3}} {seqn {{fun {x} {setbox x 5}} a} {openbox a}}} (mtSub) (mtSto))
; for identifier 'a', the address is still 2 and the value is still (boxV 1)
; however, because the value in address has changed from 3 to 5, the value of 'a' changes to 5
(run '{with {a {newbox 3}} {{fun {x} {setbox x 5}} a}} (mtSub) (mtSto))

; we want a to be 20 and b to be 10
(run '{with {swap {fun {x}
                       {fun {y}
                            {with {z x}
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              a}}}} (mtSub) (mtSto))
; However because our interpreter is based on call-by-value, a returns 10, which is incorrrect.

