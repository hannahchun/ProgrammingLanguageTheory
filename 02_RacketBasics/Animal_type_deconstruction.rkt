#lang plai
(define-type Animal
  [bear (num_child number?)
        (color string?)]
  [giraffe (height number?)
           (name string?)
           (len_neck number?)]
  [snake (length number?)
         (poison boolean?)])

(define myBear(bear 2 "white"))
(define myGiraffe(giraffe 200 "JC" 300))

; getnumber: Animal -> list of numbers
(define (getnumber a)
  (type-case Animal a
    [bear (n c) n]
    [giraffe (h n l_n) (list h l_n)]
    [snake (l p?) l]))

(getnumber myBear)
(getnumber myGiraffe)

