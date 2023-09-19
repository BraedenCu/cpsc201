#lang racket

(require racket/trace)

(provide hours
	 bin2dec
	 dec2bin
	 hex2dec
	 dec2hex
	 myassq
	 sorted?
	 inflate
	 iterate
	 add5
	 collatz
	 compound
	 power-set
	 primes
	 prime-factors
	 all-factors
	 )

; Please do not modify the lines above this one.

; ********************************************************
; CS 201 HW #1  DUE Wednesday 9/20/2023, 11:59 pm
;                via the submit system on the Zoo
; ********************************************************
; Name: Braeden Cullen
; Email address: braeden.cullen@yale.edu
; ********************************************************

; This file may be opened in DrRacket.  Lines beginning with
; semicolons are comments.

; If you are asked to write a procedure, please make sure it has the
; specified name, and the specified number and order of arguments.
; The names of the formal arguments need not be the same as in the
; problem specification.

; For each problem, the intended inputs to your procedures are
; specified (for example, "positive integers") and your procedures
; need not do anything reasonable for other possible inputs.

; You may write auxiliary procedures in addition to the requested
; one(s) -- for each of your auxiliary procedures, please include a
; comment explaining what it does, and giving an example or two.

; You may also use procedures you have written elsewhere in this
; assignment or previous assignments.  They only need to be defined
; once somewhere within this file.

; Please use the predicate equal? to test equality of values that may
; not be numbers.  To test equality of numbers, you can use =.

; Reading: Chapters 3 and 4 of the Racket Guide.


;; Concepts include the following:

; defining functions 
; lambda expressions
; recursion: top-level, i.e., cdr recursion 
; list selectors: car, cdr, first, rest
; list constructors: cons, list, append
; list functions: apply, reverse, remove-duplicates, sort, assq, assoc
; various math functions, e.g., quotient, remainder, expt
; defining local variables with let
; functions as arguments
; functions with optional arguments

; ********************************************************
; ** problem 0 ** (1 easy point) 

; Replace the number 0 in the definition below to indicate the number
; of hours you spent doing this assignment Decimal numbers (eg, 6.237)
; are fine.  Exclude time spent reading.

(define hours 10)

; ********************************************************
; ** problem 1 ** (9 points)
; 
; Write two procedures which convert binary numbers to decimal and vice versa

; (bin2dec '(1 0 0))

; that takes a list of binary digits and returns the
; corresponding decimal number

; (dec2bin n)

; that takes a positive integer as input, and returns 
; a list of binary digits
; The following identity should hold:

;; (bin2dec (dec2bin n)) == n


; Examples
; (bin2dec '(1 0 1 0)) => 10
; (bin2dec '(1 1 1 1 1)) => 31
; (bin2dec '(1)) => 1
; (bin2dec '()) => 0
; (bin2dec '(1 0 0 0 0 0 0 0)) => 128
;
; (dec2bin 23) => '(1 0 1 1 1)
; (dec2bin 2) => '(1 0)
; (dec2bin 128) => '(1 0 0 0 0 0 0 0)
; (dec2bin 127) => '(1 1 1 1 1 1 1)

; ********************************************************



(define (bin2dec num)
  (define len (length num))
  (define rnum (reverse num))

  (define (convert n)
    (if (empty? n)
        0
        (+
         (* (car n) (expt 2 (- len (length n))))
         (convert (cdr n)))))

  (convert rnum)
)

(define (dec2bin n)
  (define result empty)
  (define (convert x)
    (cond [(equal? x 0) result]
          [(equal? (modulo x 2) 1) result (append '(1) (convert(truncate (/ (- x 1) 2))))]
          [(equal? (modulo x 2) 0) append result (append '(0) (convert(truncate (/ x 2))))]
          [else (result)]
          )
    )
  (reverse (convert n))
  )

; ********************************************************
; ** problem 2 ** (10 points)

; Write two procedures which convert hexadecimal numbers (base 16) to
; decimal and vice versa

; that takes a list of hexadecimal (base 16) digits and returns the
; corresponding decimal number.  Note that hexadecimal digits may be
; upper or lower case. Your procedure should handle both.

; (dec2hex n)

; that takes a positive integer as input, and returns a list of
; hexadecimal digits.  Your procedure may output either upper or lower
; case hexadecimal digitsl.  The test procedures below expect only
; upper case, but the grading program will accept either.

; The following identity should hold:

;; (hex2dec (dec2hex n)) == n

; Examples

; (hex2dec '(A)) => 10
; (hex2dec '(F F)) => 255
; (hex2dec '(1 0 0)) => 256
; (hex2dec '(d e a d b e e f)) => 3735928559

; (dec2hex 255) => '(F F)
; (dec2hex 10) => '(A)
; (dec2hex 256) => '(1 0 0)
; (dec2hex 3735928559) => '(D E A D B E E F)

; Hint: the racket assq or assoc function might be useful for a simple
; table lookup to convert hex digits to integers and vice versa
; ********************************************************

; mapping function
(define (convertinput n)
  (cond
    [(equal? n 'A) 10]
    [(equal? n 'B) 11]
    [(equal? n 'C) 12]
    [(equal? n 'D) 13]
    [(equal? n 'E) 14]
    [(equal? n 'F) 15]
    [(equal? n "a") 10]
    [(equal? n "b") 11]
    [(equal? n "c") 12]
    [(equal? n "d") 13]
    [(equal? n "e") 14]
    [(equal? n "f") 15]
    [else n]))

(define (hex2dec num)
  (define hex '(1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, E, F))
  (define pairings (list (list 'A 10) (list 'B 11) (list 'C 12) (list 'D 13) (list 'E 14) (list 'F 15)
                         (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5) (list 6 6) (list 7 7)
                         (list 8 8) (list 9 9) (list 'a 10) (list 'b 11) (list 'c 12) (list 'd 13)
                         (list 'e 14) (list 'f 15) (list 0 0)))
  (define len (length num))
  (define rnum (reverse num))
  (define (convert n)
    (if (empty? n)
        0
        (+
         (* (cadr (assq (car n) pairings)) (expt 16 (- len (length n))))
         (convert (cdr n)))))
  (convert rnum)
  )

(define (dec2hex n)
  (define pairings (list (list 10 'A) (list 11 'B) (list 12 'C) (list 13 'D) (list 14 'E) (list 15 'F)
                         (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5) (list 6 6) (list 7 7)
                         (list 8 8) (list 9 9) (list 10 'a) (list 11 'b) (list 12 'c) (list 13 'd)
                         (list 14 'e) (list 15 'f) (list 0 0)))

  (define result empty)
  
  (define (convert x)
    (cond [(equal? x 0) result]
          [(equal? (modulo x 16) 0) result (append '(0) (convert(truncate (/ x 16))))]
          [(equal? (modulo x 16) 1) result (append '(1) (convert(truncate (/ (- x 1) 16))))]
          [(equal? (modulo x 16) 2) result (append '(2) (convert(truncate (/ (- x 2) 16))))]
          [(equal? (modulo x 16) 3) result (append '(3) (convert(truncate (/ (- x 3) 16))))]
          [(equal? (modulo x 16) 4) result (append '(4) (convert(truncate (/ (- x 4) 16))))]
          [(equal? (modulo x 16) 5) result (append '(5) (convert(truncate (/ (- x 5) 16))))]
          [(equal? (modulo x 16) 6) result (append '(6) (convert(truncate (/ (- x 6) 16))))]
          [(equal? (modulo x 16) 7) result (append '(7) (convert(truncate (/ (- x 7) 16))))]
          [(equal? (modulo x 16) 8) result (append '(8) (convert(truncate (/ (- x 8) 16))))]
          [(equal? (modulo x 16) 9) result (append '(9) (convert(truncate (/ (- x 9) 16))))]
          [(equal? (modulo x 16) 10) result (append '(A) (convert(truncate (/ (- x 10) 16))))]
          [(equal? (modulo x 16) 11) result (append '(B) (convert(truncate (/ (- x 11) 16))))]
          [(equal? (modulo x 16) 12) result (append '(C) (convert(truncate (/ (- x 12) 16))))]
          [(equal? (modulo x 16) 13) result (append '(D) (convert(truncate (/ (- x 13) 16))))]
          [(equal? (modulo x 16) 14) result (append '(E) (convert(truncate (/ (- x 14) 16))))]
          [(equal? (modulo x 16) 15) result (append '(F) (convert(truncate (/ (- x 15) 16))))]
          [else (result)]
          )
    )
  (reverse (convert n))
  )

; ********************************************************
; ** problem 3 ** (10 points)


;; In the previous problem, it might be useful to use an association list
;; and the built-in racket function: assq

;; Write a procedure myassq which duplicates the behavior of assq

; (define dectohexalist '((10 a) (11 b) (12 c) (13 d) (14 e) (15 f)))
; '((10 a) (11 b) (12 c) (13 d) (14 e) (15 f)))
; (myassq 10 dectohexalist) => '(10 a)
; (myassq 15 dectohexalist) => '(15 f)
; (myassq 10 (map reverse dectohexalist)) => '()
; (myassq 'a (map reverse dectohexalist)) => '(a 10)
; (myassq 'b (map reverse dectohexalist)) => '(b 11)
; (myassq 'a dectohexalist) => '()
; (myassq 9 dectohexalist) => '()

;; Note: you will get no credit for a trivial solution such as:
;;  (define myassq assq)
;; I am nonplussed to have to say this, but the ULAs thought it was necessary.
;; Sheesh.

; ********************************************************
 
; (Replace empty below with your procedure.)

(define (myassq value alist)
  (cond
    [(empty? alist) '()]
    [(equal? value (car (car alist))) (car alist)]
    [else (myassq value (cdr alist))]))

; ********************************************************
; ** problem 4 ** (10 points)

; Write a procedure

; (sorted? lst . compare?)

; which takes a lst and returns true if the top-level items are sorted
; according to the given comparison operator, compare?.  compare? is
; optional (as indicated by the "." in the definition.)  The default
; comparison operator is <= (less than or equal for numbers)

;; Here are some examples, first with compare? given.

;;  (sorted? '(1 2 3 4) <) => #t
;;  (sorted? '(1 2 3 4) >) => #f
;;  (sorted? '(1 2 3 4 4) <) => #f
;;  (sorted? '(1 1 1 1) =) => #t
;;  (sorted? '(1 1 1 1) <) => #f
;;  (sorted? '(1 1 1 1) <=) => #t
;;  (sorted? '("a" "b" "c") string<=?) => #t
;;  (sorted? '((1) (1 2) (1 2 3) (1 2 3 4)) (lambda (x y) (<= (length x) (length y)))) => #t
;;  (sorted? '((1) (1 2) (1 2 3) (1 2 3 4) (1)) (lambda (x y) (<= (length x) (length y)))) => #f

;; Examples using default comparison operator: <=

;;  (sorted? '(1 2 3 4)) => #t
;;  (sorted? '(1 2 3 4 4 4)) => #t
;;  (sorted? '(1 2 3 4 3 2 1)) => #f
;;  


;(define (sorted? lst . compare?)
;  (define (compareElements lst . compare?)
;    (cond
;      [(empty? lst) #t]
;      [(empty? (cdr lst)) #t]
;      [(compare? (car lst) (cadr lst)) compareElements((cdr lst) compare?)]
;      [else #f]
;      )
;    )
;  (compareElements lst compare?)
;  
;  )

;(sorted? '(1 2 3 4) <)
;(define (sorted? lst . compare?)
;  (if (null? compare?)
;      (cond
;        [(empty? lst) #t]
;        [(empty? (cdr lst)) #t]
;        [(<= (car lst) (cadr lst)) sorted?((cdr lst) '<=)]
;        [else #f]
;        )
;      (cond
;        [(empty? lst) #t]
;        [(empty? (cdr lst)) #t]
;        [(> (car lst) (cadr lst)) sorted?((cdr lst) compare?)]
;        [else #f]
;    )
;      )
;  )
  

(define (sorted? lst . compare?) empty)


;; or
; (define (sorted? lst [compare? <=])
;   empty)

; ********************************************************
; ** problem 5 ** (10 points)

; Write a procedure

; (inflate lst [value 1])

;; which returns a list with all the top level numeric values in the
;; original list incremented by the optional value, which defaults to 1.
;; Non-numeric values are unchanged.

; Examples

;; (inflate '(1 2 3)) => '(2 3 4)
;; (inflate '(1)) => '(2)
;; (inflate '()) => '()
;; (inflate '(a b c 2 3 4)) => '(a b c 3 4 5)
;; (inflate '((1) (2) (3))) => '((1) (2) (3))

;; (inflate '(1 2 3) 2) => '(3 4 5)
;; (inflate '(1 2 3) 0) => '(1 2 3)
;; (inflate '(1 2 3) -1) => '(0 1 2)
;; (inflate '(1 2 3) 100) => '(101 102 103)
;; (inflate '(a b c 2 3 4) 5) => '(a b c 7 8 9)

; ********************************************************
 
; (Replace this comment with your procedure(s).)

(define (inflate lst [value 1])
  (define convertedlst (map (lambda (l)
                              (if (number? l)
                                  (+ l value)
                                  l))
                            lst))
  convertedlst
  )

; ********************************************************
; ** problem 6 ** (10 points)

; Write a procedure

; (iterate start proc n)

; which executes the function proc n times, beginning with the argument
; start, and using the result of the previous function as the argument
; for the next call.  It returns a list of all the results.

(define (add5 x) (+ x 5))
; (iterate 2 add5 10) => '(7 12 17 22 27 32 37 42 47 52)

; (iterate 0 (lambda (x) (+ x 1)) 3) => '(1 2 3)
; (iterate 1 (lambda (n) (* n 2)) 10) => '(2 4 8 16 32 64 128 256 512 1024)
; (iterate 1 (lambda (x) (* x -2)) 10) => '(-2 4 -8 16 -32 64 -128 256 -512 1024)
; (iterate 10 (lambda (n) (- n 1)) 10) => '(9 8 7 6 5 4 3 2 1 0)
; (iterate 3 (lambda (n) (+ n 2)) 10) => '(5 7 9 11 13 15 17 19 21 23)

(define (collatz n)
  (if (= (modulo n 2) 0) (/ n 2)
      (+ 1 (* n 3))))

; (iterate 100 collatz 25) => '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; ********************************************************

; (Replace this comment with your procedure(s).)

(define (iterate start proc n)
  (define (compute iter val) 
    (if (= iter n)
        (list val)
        (cons val (compute (+ iter 1) (proc val)))))
  (rest (compute 0 start))
 )


; ********************************************************
; ** problem 7 ** (15 points)

; Write a procedure

; (compound start proc test)

; which executes the function proc until test is true, beginning with
; the argument start, and using the result of the previous function as
; the argument for the next call.  It returns a list of all the
; results.  Thus, compound is pretty much like iterate above, except
; that instead of executing a given number of times, it conditionally
; executes until the given test is satisfied.

; To see how this might matter, consider the last example of iterate:

; (iterate 100 collatz 25) => '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)

; Normally, a collatz series should stop when it reaches 1.  However,
; look what happens:

; (iterate 100 collatz 26) => 
; '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1 4)

; We can solve this problem with compound:

; (compound 100 collatz (lambda (x) (= x 1)))
; '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; (compound 200 collatz (lambda (x) (= x 1)))
; '(100 50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
; (compound 256 collatz (lambda (x) (= x 1)))
; '(128 64 32 16 8 4 2 1)

; More examples:

; (compound 10 (lambda (n) (- n 1)) (lambda (n) (<= n 0))) => '(9 8 7 6 5 4 3 2 1 0)
; (compound 0 add5 (lambda (x) (> x 50))) => '(5 10 15 20 25 30 35 40 45 50 55)
; (compound 0 add5 (lambda (x) (>= x 50))) => '(5 10 15 20 25 30 35 40 45 50)
; (compound 2 (lambda (n) (* n 2)) (lambda (x) (>= x 50))) => '(4 8 16 32 64)

; ********************************************************
 
; (Replace this comment with your procedure(s).)

(define (compound start proc test)
  (define (compute val)
    (if (test val)
       (list val)
       (cons val (compute (proc val)))
           )
        )
  (rest (compute start))
  )

;(test 'compound (compound 100 collatz (lambda (x) (= x 1))) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))

; ********************************************************
; ** problem 8 (15 points)
; Write 

; (power-set lst)

; which treats the lst as a set and returns a list of all possible
; subsets.  Both top-level and element-wise order don't matter.  The
; test code below depends on order, but the grading program does not.

; Examples:

; Note: the following five cases demonstrate the recursive,
; set-building pattern of the algorithm.

; (power-set ‘()) => ‘(())
; (power-set ‘(1)) => ‘(() (1))
; (power-set ‘(1 2)) => ‘(() (1) (2) (1 2))
; (power-set ‘(1 2 3)) => ‘(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))
; (power-set ‘(1 2 3 4)) => ’(() (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3) (4) (1 4) (2 4) (1 2 4) (3 4) (1 3 4) (2 3 4) (1 2 3 4))

; (define toppings '(onion peppers bacon sausage mushroom))
; (power-set toppings)
; '(() (mushroom) (sausage) (sausage mushroom) (bacon) (bacon mushroom)
; (bacon sausage) (bacon sausage mushroom) (peppers) (peppers mushroom)
; (peppers sausage) (peppers sausage mushroom) (peppers bacon) (peppers
; bacon mushroom) (peppers bacon sausage) (peppers bacon sausage
; mushroom) (onion) (onion mushroom) (onion sausage) (onion sausage
; mushroom) (onion bacon) (onion bacon mushroom) (onion bacon sausage)
; (onion bacon sausage mushroom) (onion peppers) (onion peppers
; mushroom) (onion peppers sausage) (onion peppers sausage mushroom)
; (onion peppers bacon) (onion peppers bacon mushroom) (onion peppers
; bacon sausage) (onion peppers bacon sausage mushroom))

; (Replace this comment with your procedures.)


(define (power-set lst)
  ;(trace power-set)
  (if (empty? lst)
      (list empty)
      (append
       (power-set (cdr lst))
       (map (lambda (l)
          (cons (car lst) l)) (power-set (cdr lst)))
       )
      )
  )


; ********************************************************
; ** problem 9 (10 points)
; 

; OK.  Now we are going to put power-set to use.  First, we give you a
; function to generate all the prime factors of a given positive
; integer.  (Last year I had the students write this function.)

(define (primes n)
  (define (sift list p)
    (filter (lambda (n)
              (not (zero? (modulo n p))))
            list))
  (define (iter nums primes)
    (let ((p (car nums)))
      (if (> (* p p) n)
          (append (reverse primes) nums)
          (iter (sift (cdr nums) p) (cons p primes)))))
  (iter (cdr (build-list n add1)) '()))

(define (divides? p q)
  (zero? (modulo q p)))

(define (prime-factors n)
  (let loop ((primes (primes n)))
    (cond ((memq n primes) (list n))
          ((divides? (car primes) n)
           (cons (car primes) (prime-factors (/ n (car primes)))))
          (else (loop (cdr primes))))))


; Use prime-factors to write the following procedure:

; (all-factors n) which generates a sorted list of all the factors of
; the positive integer n, without duplicates.

;; Note: racket has a remove-duplicates function

;; Hint: the factors of a positive number can be obtained from the
;; power set of the number's prime factors.

; Examples:

; (all-factors 20) => '(1 2 4 5 10 20)
; (all-factors 32) => '(1 2 4 8 16 32)
; (all-factors 97) => '(1 97)
; (all-factors 1000) => '(1 2 4 5 8 10 20 25 40 50 100 125 200 250 500 1000)
; (all-factors 30030) => '(1 2 3 5 6 7 10 11 13 14 15 21 22 26 30 33
;; 35 39 42 55 65 66 70 77 78 91 105 110 130 143 154 165 182 195 210 231
;; 273 286 330 385 390 429 455 462 546 715 770 858 910 1001 1155 1365
;; 1430 2002 2145 2310 2730 3003 4290 5005 6006 10010 15015 30030)


; (Replace this comment with your procedure(s).)

(define (all-factors n)
  (define primefactors (prime-factors n))
  ;(println primefactors)
  (define (prod l)
    (if (empty? l)
        1
        (* (car l) (prod (cdr l))))
    )
  ;(println (power-set primefactors))
  (define factors (map (lambda (x) (prod x))
                       (power-set primefactors)))
  (sort (remove-duplicates factors) <)
  )
    

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (oldtest name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

#|
(test 'hours hours (lambda (x) (> x 0)))
	
(test 'bin2dec (bin2dec '(1 0 1 0)) 10)
(test 'bin2dec (bin2dec '(1 1 1 1 1)) 31)
(test 'bin2dec (bin2dec '(1)) 1)
(test 'bin2dec (bin2dec '())  0)
(test 'bin2dec (bin2dec '(1 0 0 0 0 0 0 0)) 128)


(test 'dec2bin (dec2bin 10) '(1 0 1 0))
(test 'dec2bin (dec2bin 31) '(1 1 1 1 1))
(test 'dec2bin (dec2bin 23) '(1 0 1 1 1))
(test 'dec2bin (dec2bin 2) '(1 0))
(test 'dec2bin (dec2bin 128) '(1 0 0 0 0 0 0 0))
(test 'dec2bin (dec2bin 127) '(1 1 1 1 1 1 1))



(test 'hex2dec (hex2dec '(A)) 10)
(test 'hex2dec (hex2dec '(F F)) 255)
(test 'hex2dec (hex2dec '(1 0 0)) 256)
(test 'hex2dec (hex2dec '(d e a d b e e f)) 3735928559)

(test 'dec2hex (dec2hex 10) '(A))
(test 'dec2hex (dec2hex 255) '(F F))
(test 'dec2hex (dec2hex 256) '(1 0 0))
(test 'dec2hex (dec2hex 3735928559) '(D E A D B E E F))


(define dectohexalist '((10 a) (11 b) (12 c) (13 d) (14 e) (15 f)))

(test 'myassq (myassq 10 dectohexalist) '(10 a))
(test 'myassq (myassq 15 dectohexalist) '(15 f))
(test 'myassq (myassq 10 (map reverse dectohexalist)) '())
(test 'myassq (myassq 'a (map reverse dectohexalist)) '(a 10))
(test 'myassq (myassq 'b (map reverse dectohexalist)) '(b 11))
(test 'myassq (myassq 'a dectohexalist) '())
(test 'myassq (myassq 9 dectohexalist) '())
|#


;(test 'sorted? (sorted? '(1 2 3 4) <) #t)
;(test 'sorted? (sorted? '(1 2 3 4) >) #f)
;(test 'sorted? (sorted? '(1 2 3 4 4) <) #f)
;(test 'sorted? (sorted? '(1 1 1 1) =) #t)
;(test 'sorted? (sorted? '(1 1 1 1) <) #f)
;(test 'sorted? (sorted? '(1 1 1 1) <=) #t)
;(test 'sorted? (sorted? '("a" "b" "c") string<=?) #t)
;(test 'sorted? (sorted? '((1) (1 2) (1 2 3) (1 2 3 4)) (lambda (x y) (<= (length x) (length y)))) #t)
;(test 'sorted? (sorted? '((1) (1 2) (1 2 3) (1 2 3 4) (1)) (lambda (x y) (<= (length x) (length y)))) #f)

;(test 'sorted? (sorted? '(1 2 3 4)) #t)
;(test 'sorted? (sorted? '(1 2 3 4 4 4)) #t)
;(test 'sorted? (sorted? '(1 2 3 4 3 2 1)) #f)


(test 'inflate (inflate '(1 2 3)) '(2 3 4))
(test 'inflate (inflate '(1)) '(2))
(test 'inflate (inflate '()) '())
(test 'inflate (inflate '(a b c 2 3 4)) '(a b c 3 4 5))
(test 'inflate (inflate '((1) (2) (3))) '((1) (2) (3)))





;(test 'iterate (iterate 2 add5 10) '(7 12 17 22 27 32 37 42 47 52))
;(test 'iterate (iterate 0 (lambda (x) (+ x 1)) 3) '(1 2 3))
;(test 'iterate (iterate 1 (lambda (n) (* n 2)) 10) '(2 4 8 16 32 64 128 256 512 1024))
;(test 'iterate (iterate 1 (lambda (x) (* x -2)) 10) '(-2 4 -8 16 -32 64 -128 256 -512 1024))
;(test 'iterate (iterate 10 (lambda (n) (- n 1)) 10) '(9 8 7 6 5 4 3 2 1 0))
;(test 'iterate (iterate 3 (lambda (n) (+ n 2)) 10) '(5 7 9 11 13 15 17 19 21 23))
;(test 'iterate (iterate 100 collatz 25) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))


;(test 'compound (compound 100 collatz (lambda (x) (= x 1))) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
;(test 'compound (compound 200 collatz (lambda (x) (= x 1)))  '(100 50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
;(test 'compound (compound 256 collatz (lambda (x) (= x 1)))  '(128 64 32 16 8 4 2 1))

;(test 'compound (compound 10 (lambda (n) (- n 1)) (lambda (n) (<= n 0))) '(9 8 7 6 5 4 3 2 1 0))
;(test 'compound (compound 0 add5 (lambda (x) (> x 50))) '(5 10 15 20 25 30 35 40 45 50 55))
;(test 'compound (compound 0 add5 (lambda (x) (>= x 50))) '(5 10 15 20 25 30 35 40 45 50))
;(test 'compound (compound 2 (lambda (n) (* n 2)) (lambda (x) (>= x 50))) '(4 8 16 32 64))


;(test 'power-set (power-set '()) '(()))
;(test 'power-set (power-set '(1)) '(() (1)))
;(test 'power-set (power-set '(1 2)) '(() (2) (1) (1 2)))
;(test 'power-set (power-set '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))


;(test 'all-factors (all-factors 20) '(1 2 4 5 10 20))
;(test 'all-factors (all-factors 32) '(1 2 4 8 16 32))
;(test 'all-factors (all-factors 97) '(1 97))
;(test 'all-factors (all-factors 1000) '(1 2 4 5 8 10 20 25 40 50 100 125 200 250 500 1000))
;(test 'all-factors (all-factors 30030) '(1 2 3 5 6 7 10 11 13 14 15 21 22 26 30 33 35 39 42 55 65 66 70 77 78 91 105 110 130 143 154 165 182 195 210 231 273 286 330 385 390 429 455 462 546 715 770 858 910 1001 1155 1365 1430 2002 2145 2310 2730 3003 4290 5005 6006 10010 15015 30030))

;*********************************************************
;***** end of hw #1