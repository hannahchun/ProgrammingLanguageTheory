#lang plai
; FWAE that supports first-class functions
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (ftn FWAE?) (arg FWAE?)])

;(fun 'x (add (id 'x) (id 'x)))
;(app (fun 'x (add (id 'x) (id 'x))) (num 10))

; Parser
; [contract] parse : sexp -> FWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax:~a" sexp)]))

#|
(parse '{fun {x} {+ x 1}})
(parse '{{fun {x} {+ x 1}} 10})
(parse '{with {x 3} {fun {x} {+ x y}}})
(parse '{with {x 3} {fun {y} {+ x y}}})
(parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}})
|#

; Interpreter
; [contract] interp : FWAE -> FWAE
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (num+ (interp l) (interp r))]
    [sub (l r) (num- (interp l) (interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) fwae]
    [app (f a) (local
                 [(define ftn (interp f))]
                 (interp (subst (fun-body ftn)
                                (fun-param ftn)
                                (interp a)
                                )
                         )
                 )
         ]
    )
  )

; [contract] num-op : op -> FWAE
(define (num-op op)
  (lambda (x y)
    (num (op (num-n x) (num-n y)))
    )
  )

(define num+ (num-op +))
(define num- (num-op -))

; Substitution
; [contract] exp idtf val -> FWAE
(define (subst exp idtf val)
  (type-case FWAE exp
    [num (n) exp]
    [add (l r) (add (subst l idtf val) (subst r idtf val))]
    [sub (l r) (sub (subst l idtf val) (subst r idtf val))]
    [with (i v e) (with i (subst v idtf val)
                        (if (symbol=? i idtf) e (subst e idtf val)))]
    [id (s) (if (symbol=? s idtf) val exp)]
    [app (f a) (app (subst f idtf val) (subst a idtf val))]
    [fun (id body) (if (equal? idtf id) exp (fun id (subst body idtf val)))]
    )
  )

; Test cases provided in PPT
(test (interp (app (fun 'x (add (id 'x) (num 1))) (num 10))) (num 11))
(test (interp (with 'x (num 3) (fun 'x (add (id 'x) (id 'y))))) (fun 'x (add (id 'x) (id 'y))))
(test (interp (with 'x (num 3) (fun 'y (add (id 'x) (id 'y))))) (fun 'y (add (num 3) (id 'y))))

; Test cases provided in Lecture video 
(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) (num 10))
(test (interp (with 'x (num 5) (add (num 1) (with 'y (id 'x) (id 'y))))) (num 6))
(test (interp (parse '{fun {a} {+ a a}})) (fun 'a (add (id 'a) (id 'a))))
(test (interp (parse '{with {fn {fun {a} {+ a a}}} {with {x 1} {fn {with {y 10} {+ y x}}}}})) (num 22))

; Dynamic Scope Issue
(interp (parse '{with {y 3} {with {z {fun {x} {+ x y}}} {with {y 10} z}}})) ; works fine
(interp (parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}})) ; must produce error because 'y' in {fun {x} {+ x y}} is a free identifier
(interp (parse '{with {y 3} {with {z {fun {x} {+ x y}}} {with {y 10} {z 5}}}})) ; works fine
(interp (parse '{with {z {fun {x} {+ x y}}} {with {y 10} {z 5}}})) ; ; must produce error because 'y' in {fun {x} {+ x y}} is a free identifier

; We are adopting static scope.
; If we use deferred substitution, we can solve the dynamic scope issue.