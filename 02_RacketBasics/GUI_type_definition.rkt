#lang plai
; type definitions
(define-type GUI
  [label  (text string?)]
  [button (text string?)
          (enabled? boolean?)]
  [choice (items (listof string?))
          (selected integer?)])


; create instances based on the definitions
(label "Pick a fruit")
(button "Ok" false)
(choice '("Apple" "Strawberry" "Banana") 0)

; assign this entire instance into the identifier named 'ch'
(define ch (choice '("Apple" "Strawberry" "Banana") 0))
; checks whether 'ch' is instance of choice or not
(choice? ch)
; [variant_id]-[field_id]
(choice-selected ch)
; checks whether 'ch' is instance of GUI or not
(GUI? ch)

