#lang plai
; type definitions
(define-type GUI
  [label  (text string?)]
  [button (text string?)
          (enabled? boolean?)]
  [choice (items (listof string?))
          (selected integer?)])

; create instances based on the definitions

(define myLabel(label "Pick a fruit"))
(define myButton(button "Ok" false))
(define myChoice(choice '("Apple" "Strawberry" "Banana") 0))

(define (read-screen g)
  (type-case GUI g
    [label (t) (list t)]
    [button (t e?) (list t)]
    [choice (i s) i]))

(read-screen myLabel)
(read-screen myButton)
(read-screen myChoice)
