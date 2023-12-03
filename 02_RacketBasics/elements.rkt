#lang plai
; What kinds of PL elements exist for Computers?
; Numbers and Arithmetic
(+ 1 2 3) 
(/ 22 7)
(modulo 23 3)
(max 1 4 3 5 6)
(min 2 5 3 4 5)
(abs -6)
(sqrt 4)

; Variables and Functions
; A square of side-length a has the area a^2
(define (area-of-square a)
  (* a a))

(area-of-square 5)
(area-of-square 3)

; A disk of radius r has the approximate area 3.14 * r^2
(define (area-of-disk r)
  (* 3.14 (* r r)))

(area-of-disk 5)
(area-of-disk 3)

; Design the function for the area of a ring
; [contract] area-of-ring: number number -> number
; [purpose] to compute the area of a ring whose radius
; outer and whose hole has a radius of inner
; [tests] (area-of-ring 5 3) should produce 50.24

(define (area-of-ring outer inner) 
  (- (area-of-disk outer)
     (area-of-disk inner)))

(test (area-of-ring 5 3) 50.24)

; Conditional Expressions
; Booleans and relations
(and (> 4 3) (<= 10 100))
(or (> 4 3) (= 10 100))
(not (= 2 3))

; Check whether a given number is between 5 and 6, or over 10
; [contract] is-between-5-6-or-over-10?: number -> boolean
; [purpose] to determine whether n is between 5 and 6
; (exclusive) or larger than or equal to 10

(define (is-between-5-6-or-over-10? n)
  (or (and (< 5 n) (< n 6))
      (>= n 10)))

(test (is-between-5-6-or-over-10? 8) false)
(test (is-between-5-6-or-over-10? 12) true)

; Conditional Functions
; [contract] interest-rate: number -> number
; [purpose] to calculate the interest rate for a given amount
; [tests] (interest-rate 1000) should produce 0.040

; solution 1
(define (interest-rate amount)
  (cond
    [(<= amount 1000) 0.040]
    [(<= amount 5000) 0.045]
    [(> amount 5000) 0.050]))

(test (interest-rate 1000) 0.040)
(test (interest-rate 2000) 0.045)

#|
; solution 2
(define (interest-rate amount)
  (cond
    [(<= amount 1000) 0.040]
    [(<= amount 5000) 0.045]
    [else 0.050]))

(test (interest-rate 1000) 0.040)
(test (interest-rate 2000) 0.045)
|#

; Symbols
(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

(test (reply 'GoodMorning) 'Hi)
