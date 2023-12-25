#lang plai
; RBMFAE
; call by value 
; call by reference
(define-type RBMFAE
  [num (n number?)]
  [add (lhs RBMFAE?) (rhs RBMFAE?)]
  [sub (lhs RBMFAE?) (rhs RBMFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RBMFAE?)] ; call-by-value
  [refun (param symbol?) (body RBMFAE?)] ; call-by-reference
  [app (ftn RBMFAE?) (arg RBMFAE?)]
  [newbox (v RBMFAE?)]
  [setbox (bn RBMFAE?) (v RBMFAE?)]
  [openbox (v RBMFAE?)]
  [seqn (ex1 RBMFAE?) (ex2 RBMFAE?)]
  [setvar (name symbol?) (v RBMFAE?)] ; variable
  )

; DefrdSub
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

; RBMFAE-Value
; keep a memory address value of a box for static scope
(define-type RBMFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RBMFAE?) (ds DefrdSub?)]
  [refclosV (param symbol?) (body RBMFAE?) (ds DefrdSub?)] ; call-by-reference
  [boxV (address integer?)])

; Store
; track dynamic changes of boxes
(define-type Store
  [mtSto]
  [aSto (address integer?) (value RBMFAE-Value?) (rest Store?)])

; Value*Store
; return type holds both the value and the storage information. 
(define-type Value*Store
  [v*s (value RBMFAE-Value?) (store Store?)])

; parse : sexp -> RBMFAE
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
    [(list 'refun (list p) b) (refun p (parse b))] ; call-by-reference
    [else (error 'parse "bad syntax: ~a" sexp)])
  )

; num-op: (number number ->number) -> (RBMFAE RBMFAE -> RBMFAE)
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

; store-lookup: address store -> RBMFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
          (if (= location address)
              value
              (store-lookup address rest-store))]))

; malloc: Store -> Integer
; to allocate memory for a new box
; call-by-value
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
  (type-case RBMFAE expr
    [num (n) (v*s (numV n) st)]
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [refun (p b) (v*s (refclosV p b ds) st)]
    [app (f a) (type-case Value*Store (interp f ds st)
                 [v*s (f-value f-store)
                      (type-case RBMFAE-Value f-value
                        [closureV (c-param c-body c-ds)
                                  (type-case Value*Store (interp a ds f-store)
                                    [v*s (a-value a-store)
                                         (local ([define new-address (malloc a-store)])
                                           (interp c-body
                                                   (aSub c-param
                                                         new-address
                                                         c-ds)
                                                   (aSto new-address
                                                         a-value
                                                         a-store)))])]
                        [refclosV (rc-param rc-body rc-ds)
                                  (local [(define address (lookup (id-name a) ds))]
                                    (interp rc-body
                                            (aSub rc-param
                                                  address
                                                  rc-ds)
                                            f-store))]
                        [else (error interp "trying to apply a number")])])]
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

; call by reference
; (parse '{with {a 3} {setvar a 5}})
(run '{with {a 3} {setvar a 5}} (mtSub) (mtSto))
; (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (mtSto))))
; (parse '{with {a 3} {seqn {{refun {x} {setvar x 5}} a} a}})
(run '{with {a 3} {seqn {{refun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))
; (v*s (numV 5) (aSto 1 (numV 5) (aSto 1 (numV 3) (mtSto))))

; we want a to be 20 and b to be 10
(run '{with {swap {refun {x}
                       {refun {y}
                            {with {z x}
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              a}}}} (mtSub) (mtSto))

(run '{with {swap {refun {x}
                       {refun {y}
                            {with {z x}
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              b}}}} (mtSub) (mtSto))
