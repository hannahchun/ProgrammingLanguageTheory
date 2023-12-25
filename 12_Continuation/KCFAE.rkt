#lang plai
; KCFAE
(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [sub (lhs KCFAE?) (rhs KCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [if0 (condition KCFAE?) (then KCFAE?) (else KCFAE?)]
  [withcc (i symbol?) (arg KCFAE?)]
  [app (ftn KCFAE?) (arg KCFAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value KCFAE-Value?) (ds DefrdSub?)])

; KCFAE-Value
(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (c procedure?)])
; continuation itself can be value
; continuation can be implemented using annonymous function, lambda function

; Parser
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'if0 con exp1 exp2) (if0 (parse con) (parse exp1) (parse exp2))]
    [(list 'withcc i exp) (withcc i (parse exp))]
    [else                           (error 'parse "bad syntax")]
  )
)

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error)]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))

; interp : KCFAE DefrdSub (KCFAE-Value -> alpha) -> alpha
; or interp : KCFAE DefrdSub receiver -> doesn't return (but receiver returns)

; we are going to provide the lambda function as the initial continuation
; when a value is passed to the parameter of the lambda function, it returns the argument itself which is alpha
; retriever itself is a continuation, not the final result. 
; continuation itself is a value provided by our interpreter.
(define (interp fae ds k)
  (type-case KCFAE fae
    [num (n) (k (numV n))] ; wrap by k
    [add (l r) (interp l ds
                       (lambda (lv)
                         (interp r ds
                                 (lambda (rv)
                                   (k (num+ lv rv))))))]
    [sub (l r) (interp l ds
                       (lambda (lv)
                         (interp r ds
                                 (lambda (rv)
                                   (k (num- lv rv))))))]
    [if0 (test t f) (interp test ds
                            (lambda (tv)
                              (if (eq? (interp test ds k) (numV 0))
                                  (interp t ds k)
                                  (interp f ds k))))]
    [id (s) (k (lookup s ds))]
    [fun (p b) (k (closureV (lambda (a-val dyn-k)
                              (interp b (aSub p a-val ds) (dyn-k)))))]
    [app (f a) (interp f ds
                       (lambda (f-val)
                         (interp a ds
                                 (lambda (a-val)
                                   (type-case KCFAE-Value f-val
                                     [closureV (c) (c a-val k)]
                                     [contV (c) (c a-val)]
                                     [else (error "not an application value")])))))]
    [withcc (cont-var body)
            (interp body
                    (aSub cont-var
                          (contV (lambda (val)
                                   (k val)))
                           ds)
                          k)]
    ))


(define (run sexp ds)
  (interp (parse sexp) ds (lambda (x) x)))   

(run '{withcc k {+ 1 {k 3}}} (mtSub))
(run '{withcc done                                        ;; done = {fun {x}  x}
           {{withcc esc                              ;; esc = {fun {y} {y 3}}
                     {done {+ 1 {withcc k      ;; k = {fun {z} {{done {+ 1 z}} 3}}
                                              {esc k}}}}}
               3}} (mtSub))     

    