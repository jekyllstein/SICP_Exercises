#lang sicp
;1.2.1 Linear Recursion and Iteration
;Exercise 1.9: Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.

(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))

;Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5). Are these processes iterative or recursive?

;for the first process 
(+ 4 5) 
(if (= 4 0) 
	5 
	(inc (+ (dec 4) 5)))

(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc(+ 0 5)))))
(inc (inc (inc (inc(5)))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;for the second process 
(+ 4 5)
(if (= 4 0)
	5
	(+ (dec 4) (inc 5)))
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

; second process is interative, first process is recursive


;Exercise 1.10: The following procedure computes a mathematical function called Ackermann’s function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
;What are the values of the following expressions?

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
;.... nested to (A 1 1) because the conditional will terminate first when y = 1
;(A 1 1) = 2, and (A 0 2) = 2*2 so the answer is 2^10 = 1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
; similar to above (A 1 N) = 2^N so the answer is 2^16=65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 4))
(A 1 16)
; 2^16 = 65536

;Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
;Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values of n. For example, (k n) computes 5n2.

;f for x = 0, 2*n
;g for x = 1, 2^n
;h 
;(A 2 n)
;(A 1 (A 2 n-1))
;(A 1 (A 1 (A 2 n-2)))...until (n-m) = 1 when it will be replaced by 2
;so (A 1 (A 1 (A 1 2))) = 2^(2^(2^2)) and (h n) = 2^2^...^2 n times

;Exercise 1.11: A function f is defined by the rule that f(n)=n if n<3 and f(n)=f(n−1)+2f(n−2)+3f(n−3) if n≥3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

;recursive definition
(define (f n)
	(if (< n 3) 
		n
		(+ (f (- n 1))	(* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;iterative definition
;if n = 1, f(1) = 1
;if n = 2, f(2) = 2 
;if n = 3, f(3) = f(2) + 2*f(1) + 3*f(0) = 2+2+0 = 4
;if n = 4, f(4) = f(3) + 2*f(2) + 3*f(1) = 4+(2*2)+(3*1) = 11
;if n = 5, f(5) = f(4) + 2*f(3) + 3*f(2) = 11 + 2*4 + 3*2

(define (f-iter n count f1 f2 f3)
	(if (= count n)
		f1
		(f-iter n (+ count 1) (+ f1 (* 2 f2) (* 3 f3)) f1 f2)))
(define (fnew n)
	(if (< n 3)
		n
		(f-iter n 2 2 1 0)))
;at each stage the iteration keeps track of 4 state variables which are all that is needed to go to the next step.  f1 is replaced by the f(n) definition using the previous 3 f states to genreate the next one.  The solution is built up starting from the first 3 values of f in reverse with f(2), f(1), and f(0) being 2, 1, 0 respectively for the base case of f(3).

;1.2.2 Tree Recursion
;Exercise 1.12: The following pattern of numbers is called Pascal’s triangle.

         1
       1   1
     1   2   1
   1   3   3   1
 1   4   6   4   1
;       . . .
;The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.
(define (pascal r c)
	(cond 	((= c 1) 1)
			((= c r) 1)
			(else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))
;computes the element of the triangle specified by r (row) and c (column), so c = 1 is always the first element of the row and c = r is always the last element of the row.  See the following outputs
(pascal 5 1)
;Value: 1
(pascal 5 2)
;Value: 4
(pascal 5 3)
;Value: 6

;Exercise 1.13: Prove that Fib(n) is the closest integer to φn/5⎯⎯√, where φ=(1+5⎯⎯√)/2. Hint: Let ψ=(1−5⎯⎯√)/2. Use induction and the definition of the Fibonacci numbers (see 1.2.2) to prove that Fib(n)=(φn−ψn)/5⎯⎯√.
;φ+ψ=1
;φ-ψ=sqrt(5)
;fib(0) = 0
;fib(1) = 1 = φ+ψ
;fib(n) = fib(n-1) + fib(n-2)
;(φ+ψ)^n = 1
;(φ-ψ)^n = 5^(n/2)
;(a-b)^2 = a^2 - 2ab + b^2
;(a+b)^2 = a^2 + 2ab + b^2
;a^2 + b^2 = (a-b)^2 + (a+b)^2 / 2
;Trying to prove that Fib(n)=(φn−ψn)/5⎯⎯√ 
;Fib(0) = (1 - 1)/sqrt(5) = 0 which holds true
;Assume that Fib(k)=(φk−ψk)/5⎯⎯√ , show that Fib(k+1)=(φk+1−ψk+1)/5⎯⎯√ 
;We know that Fib(k+1) = Fib(k) + Fib(k-1) = (φk−ψk)/5⎯⎯√ + Fib(k-1)
;Also we know that (φk−ψk)/5⎯⎯√ = Fib(k-1) + Fib(k-2), since k is an arbitrary variable, we can rewrite this as (φk+1−ψk+1)/5⎯⎯√ = Fib(k) + Fib(k-1) which matches the definition of Fib(k+1) given by the formula. Since ψ is less than 1, it tends to 0 as n gets large while φ is greater than 1 so the formula will be donimated by the φ term.

;1.2.3 Orders of Growth
;Exercise 1.14: Draw the tree illustrating the process generated by the count-change procedure of 1.2.2 in making change for 11 cents. What are the orders of growth of the space and number of steps used by this process as the amount to be changed increases?
(count-change 11)
(cc 11 5)
(+ (cc 11 4) (cc (- 11 50) 5))
(+ (cc 11 4) 0)
(cc 11 4)
(+ (cc 11 3) (cc (- 11 25) 4))
(c 11 3)
(+ (c 11 2) (cc (- 11 10) 3))
(+ (c 11 2) (cc 1 3))
(cc 1 3)
(+ (cc 1 2) (cc (- 1 3)))
(cc 1 2)
(+ (cc 1 1) (cc (- 1 5) 2))
(+ (cc 1 1) 0)
(cc 1 1)
(+ (cc 1 0) (cc 0 1))
(+ 0 1)
1
(cc 11 2)
(+ (cc 11 1) (cc (- 11 5) 2))
(+ (cc 11 1) (cc 6 2))
(+ (+ (cc 11 0) (cc 10 1)) (cc 6 2))
(+ (cc 10 1) (cc 6 2))
(+ (+ (cc 10 0) (cc 9 1)) (cc 6 2))
;...
(+ 1 (cc 6 2))
(cc 6 2)
(+ (cc 6 1) (cc 1 2))
(+ 1 (+ (cc 1 1) (cc -4 2)))
(+ 1 (cc 1 1))
(+ 1 (+ (cc 1 0) (cc 0 1)))
(+ 1 1)
2

;so originally we had
(+ (cc 11 2) (cc 1 3))
;(cc 11 2) = (+ 1 (cc 6 2)) = (+ 1 2) = 3
;(cc 1 3) = 1
;so the final answer is 4
;number of steps grows on the order of the amount of change and space grows with the amoun of change squared

;Exercise 1.15: The sine of an angle (specified in radians) can be computed by making use of the approximation sinx≈x if x is sufficiently small, and the trigonometric identity
;sinx=3sinx3−4sin3x3
;to reduce the size of the argument of sin. (For purposes of this exercise an angle is considered “sufficiently small” if its magnitude is not greater than 0.1 radians.) These ideas are incorporated in the following procedures:

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))
;1. How many times is the procedure p applied when (sine 12.15) is evaluated?
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
;6 applications
;2. What is the order of growth in space and number of steps (as a function of a) used by the process generated by the sine procedure when (sine a) is evaluated?
;if a required N steps to get to a size of 0.1, then that means a/(3^N) ~ 0.1 => 3^N ~ 10a => Nln3~ln(10a) => N ~ ln(10a)/ln3 so N scales with the log of a.  Each step has one additional application of p so the space growth is also on the order of log N

;1.2.4 Exponentiation
;Exercise 1.16: Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt. (Hint: Using the observation that (bn/2)2=(b2)n/2, keep, along with the exponent n and the base b, an additional state variable a, and define the state transformation in such a way that the product abn is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)

(define (fast-expt b n)
	(fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
	(cond ((= n 0)
			a)
			((even? n)
				(fast-expt-iter (* b b) (/ n 2) a))
			(else
				(fast-expt-iter b (- n 1) (* a b)))))

(define (even? n)
  (= (remainder n 2) 0))
;so if n is even, change the base to b*b and half the exponent keeping a unchanged, otherwise keep the base unchanged, reduce n by 1 and multiply a and b

;Exercise 1.17: The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication. In a similar way, one can perform integer multiplication by means of repeated addition. The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
;This algorithm takes a number of steps that is linear in b. Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2. Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.
(define (double a)
	(+ a a))

(define (half a)
	(/ a 2))

(define (fast-mult a b)
  (cond ((= b 0) 
         0)
        ((even? b) 
         (double (fast-mult a (half b))))
        (else 
         (+ a (fast-mult a (- b 1))))))

;Exercise 1.18: Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.
(define (fast-mult a b)
	(fast-mult-iter a b 0))

(define (fast-mult-iter a b m)
	(cond ((= b 0)
			m)
			((even? b)
				(fast-mult-iter (double a) (half b) m))
			(else
				(fast-mult-iter a (- b 1) (+ m a)))))

;Exercise 1.19: There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number of steps. Recall the transformation of the state variables a and b in the fib-iter process of 1.2.2: a←a+b and b←a. Call this transformation T, and observe that applying T over and over again n times, starting with 1 and 0, produces the pair Fib(n+1) and Fib(n). In other words, the Fibonacci numbers are produced by applying Tn, the nth power of the transformation T, starting with the pair (1, 0). Now consider T to be the special case of p=0 and q=1 in a family of transformations Tpq, where Tpq transforms the pair (a,b) according to a←bq+aq+ap and b←bp+aq. Show that if we apply such a transformation Tpq twice, the effect is the same as using a single transformation Tp′q′ of the same form, and compute p′ and q′ in terms of p and q. This gives us an explicit way to square these transformations, and thus we can compute Tn using successive squaring, as in the fast-expt procedure. Put this all together to complete the following procedure, which runs in a logarithmic number of steps:

;Tpq(a, b) = a <- bq + aq + ap and b <- bp + aq
;applying this twice we get a' = bq + aq + ap and b' = bp + aq
; a'' = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
; b'' = b'p + a'q = (bp + aq)p + (bq + aq + ap)q
; a'' = bpq + aqq + bqq + aqq + apq + bqp + aqp + app = a(qq + qq + pq + qp + pp) + b(pq + qq + qp) = a(2qq + 2pq + pp) + b(2pq + qq)
; b'' = bpp + aqp + bqq + aqq + apq = b(pp + qq) + a(qp + qq + pq) = b(pp + qq) + a(2qp + qq)
;so a'' = a(q' + p') + bq' where q' = (2qp + qq) and p' = pp + qq, applying this to b'' we see that b'' = bp' + aq' with the same p' and q'

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 q p) (square q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))

; 1.2.5 Greatest Common Divisors
;Exercise 1.20: The process that a procedure generates is of course dependent on the rules used by the interpreter. As an example, consider the iterative gcd procedure given above. Suppose we were to interpret this procedure using normal-order evaluation, as discussed in 1.1.5. (The normal-order-evaluation rule for if is described in Exercise 1.5.) Using the substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations that are actually performed. How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;for normal order evaluation
(gcd 206 40)
(if (= 40 0)
	a
	(gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)
	40
	(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0)
	(remainder 206 40)
	(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
	(remainder 40 (remainder 206 40))
	(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;now b is 0 so the calculation terminates with the if statement so arguments b and a can be fully evaluated here
(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;so these two calls require 11 calls to remainder.  Also all of the previous if statement arguments need to be fully evaluated so that's an additional 7 calls to remainder making the total 18

;for applicative order evaluation
(gcd 206 40)
(if (= 40 0)
	206
	(gcd 40 (remainder 206 40)))

(gcd 40 6)
(if (= 6 0)
	40
	(gcd 6 (remainder 40 6)))
(gcd 6 4)
(if (= 4 0)
	6
	(gcd 4 (remainder 6 4)))
(gcd 4 2)
(if (= 2 0)
	4
	(gcd 2 (remainder 4 2)))
(gcd 2 0)
; since b = 0 the if statement terminates returning 2
; since there are 4 steps and the remainder is evaluated at each one and passed down, there are only 4 calls to remainder

;1.2.6 Example: Testing for Primality
;Exercise 1.21: Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.

(smallest-devisor 199)
(find-divisor 199 2)
(divides? 2 199) 
; false
(find-divisor 199 3)
(divides? 3 199) ;false
(find-divisor 199 4)
(divides? 199 4) ;false
;...
;199 is prime so the smallest divisor is 199
;1999 is the same
;19999 has a divisor of 7 which is the smallest

;Exercise 1.22: Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime. If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
;Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of Θ(n⎯⎯√), you should expect that testing for primes around 10,000 should take about 10⎯⎯⎯⎯√ times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the Θ(n⎯⎯√) prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes n1 n2)
	(if (even? n1)
		(search-for-primes-iter n1 n2 (+ n1 1))
		(search-for-primes-iter n1 n2 n1)))

(define (search-for-primes-iter n1 n2 n)
	(timed-prime-test n)
	(define nextn (+ n 2)) 
	(if (> nextn n2)
		(display "finished")	
		(search-for-primes-iter n1 n2 nextn)))

(search-for-primes 1000 1021)
;1001
;1003
;1005
;1007
;1009 *** 0.
;1011
;1013 *** 0.
;1015
;1017
;1019 *** 0.
;1021 *** 0.finished

(search-for-primes 10000 10050)
;10001
;10003
;10005
;10007 *** 0.
;10009 *** 0.
;10011
;10013
;10015
;10017
;10019
;10021
;10023
;10025
;10027
;10029
;10031
;10033
;10035
;10037 *** 0.
;10039 *** 0.
;10041
;10043
;10045
;10047
;10049finished
(search-for-primes 100000 100050)
;100001
;100003 *** 0.
;100005
;100007
;100009
;100011
;100013
;100015
;100017
;100019 *** 0.
;100021
;100023
;100025
;100027
;100029
;100031
;100033
;100035
;100037
;100039
;100041
;100043 *** 9.999999999999981e-3
;100045
;100047
;100049 *** 0.finished
(search-for-primes 1000000 1000050)
;1000001
;1000003 *** 1.0000000000000009e-2
;1000005
;1000007
;1000009
;1000011
;1000013
;1000015
;1000017
;1000019
;1000021
;1000023
;1000025
;1000027
;1000029
;1000031
;1000033 *** 0.
;1000035
;1000037 *** 0.
;1000039 *** 0.
;1000041
;1000043
;1000045
;1000047
;1000049finished

;showing only results for a typical prime found
(search-for-primes 1000000000 1000000050)
;1000000021 *** 4.0000000000000036e-2
(search-for-primes 1000000000000 1000000000050)
;1000000000039 *** 1.2899999999999996
;so increasing n by 1000 increased search time by 32.25 which is very close to sqrt(1000) = 31.6227

;Exercise 1.23: The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any larger even numbers. This suggests that the values used for test-divisor should not be 2, 3, 4, 5, 6, …, but rather 2, 3, 5, 7, 9, …. To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12 primes found in Exercise 1.22. Since this modification halves the number of test steps, you should expect it to run about twice as fast. Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms, and how do you explain the fact that it is different from 2?

(define (next n)
	(if (= n 2)
		3
		(+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))


(timed-prime-test 1000000021)
;1000000021 *** 2.0000000000000462e-2
;this is exactly half of the previous value

(timed-prime-test 1000000000039)
;1000000000039 *** .7699999999999996
;this is slightly more than half of the previous time, perhaps at numbers this large  there is overhead time getting to higher steps  that is added onto the total number of steps

;Exercise 1.24: Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime? (the Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has Θ(logn) growth, how would you expect the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10000)
      (report-prime (- (runtime) 
                       start-time))))

(timed-prime-test 1000003)
;1000003 *** .6200000000000045

(timed-prime-test 1000000021)
;1000000021 *** .9500000000000028

(timed-prime-test 1000000000039)
;1000000000039 *** 1.3199999999999967

;with logn scaling every increase in n of 1000 should be a roughtly 3x increase in time but I see far less than that probably because a majority of this is still overhead time

;Exercise 1.25: Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After all, she says, since we already know how to compute exponentials, we could have simply written

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;the first procedure never actually calculates base^exp, in the fermat-test procedure we call (expmod a n n) so let's take n = 13 and a = 3.  For the second procedure we'd calculate 
(fast-expt 3 13) ;first and then the remainder with 13
(* 3 (fast-expt 3 12))
(* 3 (square (fast-expt 3 6)))
(* 3 (square (square (fast_expt 3 3))))
(* 3 (square (square (* 3 (fast_expt 3 2)))))
(* 3 (square (square (* 3 (square (fast_expt 3 1))))))
(* 3 (square (square (* 3 (square (* 3 (fast_expt 3 0)))))))
(* 3 (square (square (* 3 (square (* 3 1))))))
(* 3 (square (square (* 3 (square 3)))))
(* 3 (square (square (* 3 9))))
(* 3 (square (square 27)))
(* 3 (square 729))
(* 3 531441)
1594323

;then we call 
(remainder 1594323 13)
3
;so we had 3 multiplications and 3 squares followed by the remainder of this large number

;let's compare to the same proecure with the first expmod
(expmod 3 13 13)
(remainder (* 3 (expmod 3 12 13)) 13)
(remainder (* 3 (remainder (square (expmod 3 6 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (expmod 3 3 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (expmod 3 2 13)) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (remainder (square (expmod 3 1 13)))) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (remainder (square (remainder (* 3 (expmod 3 0 13)))))) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (remainder (square (remainder (* 3 1) 13)) 13)) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (remainder (square (remainder 3 13)) 13)) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (remainder (square 3) 13)) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (remainder 9 13)) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 9) 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square (remainder 27 13)) 13)) 13)) 13)
(remainder (* 3 (remainder (square (remainder (square 1) 13)) 13)) 13)
(remainder (* 3 (remainder (square 1) 13)) 13)
(remainder (* 3 1) 13)
(remainder 3 13)
3
;so at its most expanded we have 3 multiplications, 3 squares, and 6 calls to remainder.  So the math operations are the same and we have 5 more calls to remainder.  However, every call to remainder where the first argument is smaller than teh second is simply the first argument, so it might compute very quickly.  So in this example every call to remainder either had the first argument smaller than the second except for 1 which was (remainder 27 13). Since the base is always smaller than the prime, this expansion yields many remainder expressions which are trivial so it may be faster depending on how long it takes to calculate the remainder of very large numbers with our prime.

;Exercise 1.26: Louis Reasoner is having great difficulty doing Exercise 1.24. His fast-prime? test seems to run more slowly than his prime? test. Louis calls his friend Eva Lu Ator over to help. When they examine Louis’s code, they find that he has rewritten the expmod procedure to use an explicit multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base 
             (expmod base (- exp 1) m))
          m))))
;“I don’t see what difference that could make,” says Louis. “I do.” says Eva. “By writing the procedure like that, you have transformed the Θ(logn) process into a Θ(n) process.” Explain.

;Although (* a a) is equivalent to (square a) in this case there is an expression for each element a, so the recursive call to expmod is occuring twice.  Thus each time the exp is even, there are two recursive calls resulting in exponential growth of the tree.  Since log(exp(n)) = n then the exponential  expansion in calls cancels out the logn scaling of steps to yield n scaling.

;Exercise 1.27: Demonstrate that the Carmichael numbers listed in Footnote 47 really do fool the Fermat test. That is, write a procedure that takes an integer n and tests whether an is congruent to a modulo n for every a<n, and try your procedure on the given Carmichael numbers.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test-a n a)
  (= (expmod a n n) a))
 
(define (check-fermat-iter n a)
	(define check (expmod a n n))
	(if (= check a)
		(if (= a (- n 1)) (display "passed") (check-fermat-iter n (+ a 1)))
		(display "failed")))

(define (check-fermat n)
	(check-fermat-iter n 1))

;To show that prime numbers generally pass, here's some results
(check-fermat 2)
;passed
(check-fermat 3)
;passed
(check-fermat 13)
;passed

;while a non prime number fail
(check-fermat 10)
;failed
(check-fermat 1001)
;failed

;now for the Carmichael Numbers 561, 1105, 1729, 2465, 2821, and 6601 which are not prime but still pass the test
(check-fermat 561)
;passed
(check-fermat 1105)
;passed
(prime? 1105) ;fails for example

(check-fermat 1729)
;passed

(check-fermat 2465)
;passed

(check-fermat 2821)
;passed

(check-fermat 6601)
;passed

;Exercise 1.28: One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat’s Little Theorem, which states that if n is a prime number and a is any positive integer less than n, then a raised to the (n−1)-st power is congruent to 1 modulo n. To test the primality of a number n by the Miller-Rabin test, we pick a random number a<n and raise a to the (n−1)-st power modulo n using the expmod procedure. However, whenever we perform the squaring step in expmod, we check to see if we have discovered a “nontrivial square root of 1 modulo n,” that is, a number not equal to 1 or n−1 whose square is equal to 1 modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n is not prime. It is also possible to prove that if n is an odd number that is not prime, then, for at least half the numbers a<n, computing an−1 in this way will reveal a nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous to fermat-test. Check your procedure by testing various known primes and non-primes. Hint: One convenient way to make expmod signal is to have it return 0.

(define (signal)
	(display "Non-trivial root found")
	(newline)
	0)

(define (check-nontrivial-sqrt v n)
	(define s (square v))
	(define r (remainder s n))
	(cond 	((= v 1) r)
			((= v (- n 1)) r)
			((= r 1) (signal))
			(else r)))

(define (expmod-signal base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt (expmod-signal base (/ exp 2) m) m))
        (else
         (remainder 
          (* base (expmod-signal base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-signal a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))
 
(define (check-miller-rabin-iter n a)
	(define check (expmod-signal a (- n 1) n))
	(if (= check 1)
		(if (= a (- n 1)) (display "passed") (check-miller-rabin-iter n (+ a 1)))
		(display "failed")))

(define (check-miller-rabin n)
	(check-miller-rabin-iter n 1))

;To show that the miller-rabin test is robust, we can check it against every possible a less than n like we did for the fermat test.  Even for the Carmichael numbers the test should fail for some a and for every prime number it should pass for all a
(check-miller-rabin 2)
;passed
(check-miller-rabin 3)
;passed
(check-miller-rabin 13)
;passed

;while a non prime number fail
(check-miller-rabin 10)
;failed
(check-miller-rabin 1001)
;failed

;now for the Carmichael Numbers 561, 1105, 1729, 2465, 2821, and 6601 which are not prime but still pass the test
(check-miller-rabin 561)
;Non-trivial root found
;failed
(check-miller-rabin 1105)
;failed
(check-miller-rabin 1729)
;failed
(check-miller-rabin 2465)
;failed
(check-miller-rabin 2821)
;passed
(check-miller-rabin 6601)
;failed

;so unlike the fermat test these fail when tested for all a.  However if we limit the number of tests and do it probabalistically instead miller-rabin-fast-prime? can show passes for these values that are not prime.
(miller-rabin-fast-prime? 6601 3000)   
;Non-trivial root found
;Value: #f
(miller-rabin-fast-prime? 6601 3000)   
;Value: #t
;after 10 attempts at 3000 trials this passed once meaning we didn't select an a that caused it to fail and didn't find a non-trivial root

;In contrast the fermat method will fail very time because the test will pass for all values of a < n.
