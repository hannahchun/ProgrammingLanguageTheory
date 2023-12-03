#lang plai
; PLT HW1 22000662 Chun Hye Sun

; Problem 1:
; Solved by myself : Y (Logic and Racket code done by myself)
; Time taken : 10 mins
; [contract]
; dollar->won : integer number -> integer number
; [purpose]
; To convert dollar to won
; c.f. 1 dollar = 1,324 Korean won
; [tests]
; (test (dollar->won 2) 2648)
; (test (dollar->won 3) 3972)

(define (dollar->won d)
  (* d 1324))

(test (dollar->won 2) 2648)
(test (dollar->won 3) 3972)

; Problem 2
; Solved by myself : N (logic and racket code done by myself, when calling 'max' function, I accidentally left out parenthesis and ChatGPT helped me fix only that grammar mistake)
; Time taken : 25 mins
; [contract]
; max-of-three-integers : three integer numbers -> integer number
; [purpose]
; consumes three integer numbers and returns the max value of them
; [tests]
; (test (max-of-three-integers 1 2 3) 3)
; (test (max-of-three-integers 4 5 3) 5)
; (test (max-of-three-integers 6 5 4) 6)
; (test (max-of-three-integers 6 6 4) 6)
; (test (max-of-three-integers 4 6 4) 6)

(define (max y z)
  (cond
    [(>= y z) y]
    [else z]))

(define (max-of-three-integers a b c)
  (cond
    [(>= a b) (max a c)]
    [else (max b c)]))

(test (max-of-three-integers 1 2 3) 3)
(test (max-of-three-integers 4 5 3) 5)
(test (max-of-three-integers 6 5 4) 6)
(test (max-of-three-integers 6 6 4) 6)
(test (max-of-three-integers 4 6 4) 6)

; Problem 3
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 10 mins
; [contract]
; volume-cuboid : three integer numbers -> integer number
; [purpose]
; consumes three integer values, length, breadth, height and produces the volume of the cuboid
; [tests]
; (test (volume-cuboid 10 20 30) 6000)
; (test (volume-cuboid 3 4 5) 60)

(define (volume-cuboid a b c)
  (* a (* b c)))

(test (volume-cuboid 10 20 30) 6000)
(test (volume-cuboid 3 4 5) 60)

; Problem 4
; Solved by myself: N (logic and racket code done by myself, when implementing foldr(), I made a mistake in (cons i l) and ChatGPT helped me fix it)
; Time taken : 30mins
; [contract]
; gcd : two integer numbers -> integer number
; [purpose]
; consumes two integer values and returns gcd from the two integer values
; [tests]
; (test (gcd 30 45) 15)
; (test (gcd 24 36) 12)
; (test (gcd 33 121) 11)
; (test (gcd 18 1) 1)
; (test (gcd 11 7) 1)

(define (min-num n1 n2)
  (cond
    [(< n1 n2) n1]
    [else n2]))

(define (div-list start end)
  (build-list (add1 (- end start))
              (lambda (i) (+ start i))))

(define (gcd a b)
  (define min (min-num a b))
  (define divlist (div-list 1 min))
  (define max-com-div
    (foldr (lambda (i l)
             (cond
               [(and (= (modulo a i) 0) (= (modulo b i) 0)) (cons i l)]
               [else l]))
           '()
           divlist))
  (list-ref max-com-div (- (length max-com-div) 1)))

(test (gcd 30 45) 15)
(test (gcd 24 36) 12)
(test (gcd 33 121) 11)
(test (gcd 18 1) 1)
(test (gcd 11 7) 1)

; Problem 5
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 15mins
; [contract]
; combination : two integer numbers -> integer number
; [purpose]
; consumes two integer numbers and returns the number of combinations that can be there
; c.f. when first function parameter is n (which is the number of elements in the set), and second function parameter is k (which is the number of elements in each subset).
; [tests]
; (test (combination 6 2) 15)
; (test (combination 10 4) 210)
; (test (combination 10 6) 210)
; (test (combination 7 3) 35)

(define (factorial num)
  (cond
    [(or (= num 0) (= num 1)) 1]
    [else (* num (factorial (- num 1)))]
    )
  )
   
(define (combination n k)
  (define result
    (/ (factorial n) (* (factorial k) (factorial (- n k))))
    )
  result)

(test (combination 6 2) 15)
(test (combination 10 4) 210)
(test (combination 10 6) 210)
(test (combination 7 3) 35)

; Problem 6

; a.
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 5mins

; Define the type Vehicle, which has three variants, Bicycle, Car, Airplane.
; The Variant `Bicycle' has one attribute, ‘wheels’, which indicates the number of wheels.
; Car has two attributes, ‘wheels’ and ‘windows’, which indicates the number of wheels and windows.
; Airplane has three attributes, ‘wheels’, ‘windows’, and ‘engines’, which indicates the number of wheels, windows, and engines.

(define-type Vehicle
  [Bicycle (wheels number?)]
  [Car (wheels number?)
       (windows number?)]
  [Airplane (wheels number?)
            (windows number?)
            (engines number?)]
  )

; b.
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 20mins
; [contract]
; vehicle-tax : an instance of Vehicle, tax per wheel, tax per window, tax per engine -> the total amount of tax
; [purpose]
; consumes an instance of Vehicle, tax per wheel, tax per window, tax per engine, and returns the total amount of tax that should be paid for that vehicle.
; [tests]
#|
(define myBike(Bicycle 2))
(test(vehicle-tax myBike 10 20 30) 20)
(define myCar(Car 4 5))
(test(vehicle-tax myCar 10 20 30) 140)
(define myAirplane(Airplane 3 4 0))
(test(vehicle-tax myAirplane 10 20 30) 110)
|#

(define (vehicle-tax v twh twi ten)
  (type-case Vehicle v
    [Bicycle (wh) (* wh twh)]
    [Car (wh wi) (+ (* wh twh) (* wi twi))]
    [Airplane (wh wi en) (+ (* wh twh) (+ (* wi twi) (* en ten)))]
    )
  )

(define myBike(Bicycle 2))
(test(vehicle-tax myBike 10 20 30) 20)
(define myCar(Car 4 5))
(test(vehicle-tax myCar 10 20 30) 140)
(define myAirplane(Airplane 3 4 0))
(test(vehicle-tax myAirplane 10 20 30) 110)

; c.
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 10mins
; [contract]
; is-vehicle-safe : an instance of Vehicle -> a string “safe” or “unsafe”
; [purpose]
; consumes an instance of Vehicle, and returns a string indicating safe or not safe.

; For a Bicycle, wheels must be less than 4.
; For a Car, wheels must be more than 3, windows must be more than 2.
; For an Airplane, wheels must be more than 2, windows must be more than 10, engines must be more than 1.
; If any of the conditions are not satisfied, the vehicle is not safe.

; [tests]
#|
(define yourBike(Bicycle 2))
(test(is-vehicle-safe yourBike) "safe")

(define yourCar(Car 4 5))
(test(is-vehicle-safe yourCar) "safe")

(define yourAirplane(Airplane 3 12 2))
(test(is-vehicle-safe yourAirplane) "safe")

(define hisBike(Bicycle 4))
(test(is-vehicle-safe hisBike) "unsafe")

(define hisCar(Car 4 1))
(test(is-vehicle-safe hisCar) "unsafe")

(define hisAirplane(Airplane 3 2 2))
(test(is-vehicle-safe hisAirplane) "unsafe")
|#

(define (is-vehicle-safe v)
  (type-case Vehicle v
    [Bicycle (wh)
             (cond
                  [(< wh 4) "safe"]
                  [else "unsafe"]
                  )
             ]
    [Car (wh wi)
         (cond
              [(and (> wh 3) (> wi 2)) "safe"]
              [else "unsafe"]
              )
         ]
    [Airplane (wh wi en)
        (cond
              [(and (> wh 2) (and (> wi 10) (> en 1))) "safe"]
              [else "unsafe"]
              )
         ]
    )
  )

(define yourBike(Bicycle 2))
(test(is-vehicle-safe yourBike) "safe")

(define yourCar(Car 4 5))
(test(is-vehicle-safe yourCar) "safe")

(define yourAirplane(Airplane 3 12 2))
(test(is-vehicle-safe yourAirplane) "safe")

(define hisBike(Bicycle 4))
(test(is-vehicle-safe hisBike) "unsafe")

(define hisCar(Car 4 1))
(test(is-vehicle-safe hisCar) "unsafe")

(define hisAirplane(Airplane 3 2 2))
(test(is-vehicle-safe hisAirplane) "unsafe")

; Problem 7
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 20mins
; [contract]
; name-alphabet : a list of alphabets -> a list
; [purpose]
; consumes a list of alphabets and produces a corresponding list of an alphabetical character with names starting with the alphabet character

; it names all occurrences of 'a with 'alice, 'c with 'cherry, 'j with 'jc, 'k with 'kate and keeps the other characters as unnamed.

; [tests]
; (test(name-alphabet '(a b n)) '(alice () ()))
; (test(name-alphabet '(c j h k)) '(cherry jc () kate))
; (test(name-alphabet '(b k j w)) '(() kate jc ()))

(define (name-alphabet l)
  (foldr (lambda (s I)
           (cons
             (cond
               [(symbol=? s 'a) 'alice]
               [(symbol=? s 'c) 'cherry]
               [(symbol=? s 'j) 'jc]
               [(symbol=? s 'k) 'kate]
               [else '()]
               )
             I)
           )
         '()
         l
         )
  )

(test(name-alphabet '(a b n)) '(alice () ()))
(test(name-alphabet '(c j h k)) '(cherry jc () kate))
(test(name-alphabet '(b k j w)) '(() kate jc ()))

; Problem 8
; Solved by myself : Y (logic and racket code done by myself)
; Time taken : 15mins
; [contract]
; update-name : two symbols, list of symbols -> a list of symbols 
; [purpose]
; consumes two symbols, called old and new, and a list of symbols and produces a list of symbols by replacing all occurrences of old by new
; [tests]
; (test(update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
; (test(update-name 'cherry 'claire (cons 'cherry (cons 'cherry (cons 'kate empty)))) '(claire claire kate))
; (test(update-name 'hannah 'hans (cons 'john (cons 'cherry (cons 'hannah empty)))) '(john cherry hans))

(define (update-name s1 s2 l)
  (foldr (lambda (s I)
           (cons
            (cond
              [(equal? s s1) s2]
              [else s]
              )
            I)
           )
         '()
         l
         )
  )

(test(update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
(test(update-name 'cherry 'claire (cons 'cherry (cons 'cherry (cons 'kate empty)))) '(claire claire kate))
(test(update-name 'hannah 'hans (cons 'john (cons 'cherry (cons 'hannah empty)))) '(john cherry hans))