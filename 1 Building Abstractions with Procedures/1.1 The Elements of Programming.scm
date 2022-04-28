#lang sicp
; 1.1.4 Compound Procedures

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

; 1.1.5 The Substitution Model for Procedure Application

(f 5)
; Value 136
; This is equivalent to the following substitutions (applicative-order evaluation)

(sum-of-squares (+ a 1) (* a 2))
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square 6) (square 10))
(+ (* 6 6) (* 10 10))
(+ 36 100)
136

; Alternatively we could expand all the operands only substituting with values when they are necessary (normal-order evaluation)

(sum-of-squares (+ 5 1) (* 5 2))
(+ (square (+ 5 1)) 
   (square (* 5 2)))
(+ (* (+ 5 1) (+ 5 1)) 
   (* (* 5 2) (* 5 2)))
(+ (* 6 6) 
   (* 10 10))
(+ 36 100)
136

; Conditional Expressions and Predicates

; Exercise 1.1 Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

10
; 10
(+ 5 3 4)
; 5+3+4 = 12
(- 9 1)
; 9-1=8
(/ 6 2)
; 6/2=3
(+ (* 2 4) (- 4 6))
; (2*4) + (6-4) = 8-2 = 6
(define a 3)
; Value: a where a = 3
(define b (+ a 1))
; Value: b where b = 3+1=4
(+ a b (* a b))
; a+b+(a*b)=3+4+(3*4)=7+12=19
(= a b)
; false Value: #f
(if (and (> b a) (< b (* a b)))
    b
    a)
; b>a is true, and b<a*b so b which is 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; a is not 4, so it will evaluate the second expression because b is 4 which is 6+7+3=16
(+ 2 (if (> b a) b a))
; 2+b=2+4=6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; a < b so b*(a+1)=4*4=16

;Exercise 1.2: Translate the following expression into prefix form:
;5+4+(2−(3−(6+45)))3(6−2)(2−7)
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;Exercise 1.3: Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
(define (ss a b c)
	(cond ((> a b) 
		(cond ((> b c) (+ (square a) (square b)))
		(else (+ (square a) (square c)))))
	(else (cond ((> a c) (+ (square b) (square a)))
				(else (+ (square b) (square c)))))))
; Exercise 1.4: Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; If b is positive then add a to b, otherwise subtract b from a, effectively this adds a to the absolute value of b

;Exercise 1.5: Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))
;Then he evaluates the expression

(test 0 (p))
;What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)

; In applicative order the interpreter will first substitute the expressions for the arguments of test which in this case is 0 and (p).  But p is a procedure with nothing in it.  In observations the repl stalled indefinitely.  With normal order evaluation, the test procedure will first be expanded into (if (= x 0) 0 y)) and since x is 0, the output will be 0 and skip the evaluation of y completely.

; 1.1.7 Example: Square Roots by Newton's Method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y) 
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to be provided as a special form. “Why can’t I just define it as an ordinary procedure in terms of cond?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0
;Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
;What happens when Alyssa attempts to use this to compute square roots? Explain.
; Since new-if is a regular procedure it will be expended into 3 arguments before being passed into the cond procedure.  Since the else-clause keeps expanding indefinitely the procedure will never stop.  The original function depends on the special nature of the cond procedure to stop evaluating arguments the first time it finds a true condition.

;Exercise 1.7: The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?
; good-enough? currently checks if the square of the guess is within a set value of the actual value to be square rooted.  If the value itself is of equal size to that tolerance then the procedure will terminate with a guess that is off from the true value by any large percentage value.  For very large numbers with limited precision, a value extremely close to the true answer might never be satisfied because numerical noise in the decimal places will never allow a 0.001 tolerance to be achieved.  For example:
(sqrt 0.0001)

;Value: .03230844833048122
;While the true value should be 0.01

(sqrt 1e124)
;procedure times out


(define (sqrt-iter guess x)
  (define newguess (improve guess x))
  (if (good-enough? newguess guess)
      newguess
      (sqrt-iter newguess x)))

(define (good-enough? newguess guess)
  (< (/ (abs (- newguess guess)) guess) 0.001))

; Now when we run
(sqrt 0.0001)

;Value: 1.0000000025490743e-2
; the output is correct

; And
(sqrt 1e124)

;Value: 1.0000001427889285e62
; doesn't time out

;Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
;(x/y^2+2y)/3.
;Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (improvecube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cbrt-iter guess x)
  (define newguess (improvecube guess x))
  (if (good-enough? newguess guess)
      newguess
      (cbrt-iter newguess x)))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8)
;Value: 2.000000000012062

(cbrt 1e150)
;Value: 1.000000000012699e50

(cbrt 1e-75)
;Value: 1.0000005195680928e-25
