#lang plai
(define-type Animal
  (bear (num_child number?)
        (color string?))
  (giraffe (height number?)
           (name string?)
           (len_neck number?))
  (snake (length number?)
         (poison boolean?)))

(define myBear(bear 2 "white"))
myBear
; prints the instance

(Animal? myBear)
; returns true because myBear is an instance of Animal
(bear? myBear)
; returns true because myBear is an instance of bear
(snake? myBear)
; returns false because myBear is not an instance of snake

; [variant_id]-[field_id]
(bear-num_child myBear)

