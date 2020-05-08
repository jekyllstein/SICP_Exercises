#lang sicp
;1.3.1 Procedures as Arguments
(define (sum-integers a b)
  (if (> a b) 
      0 
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b) 
      0 
      (+ (cube a) 
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) 
         (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

 (integral cube 0 1 0.01)

;Exercise 1.29: Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s Rule, the integral of a function f between a and b is approximated as
;h3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn),
;where h=(b−a)/n, for some even integer n, and yk=f(a+kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1 (with n=100 and n=1000), and compare the results to those of the integral procedure shown above.
(define (simpson-integral f a b n)
	(define h (/ (- b a) n))
	(define (term k)
		(define y
			(f (+ a (* k h))))
		(define c 	(cond 	((OR (= k 0) (= k n)) 1)
							((= (remainder k 2) 0) 2)
							(else 4)))
		(* y c))
	(define s (sum term 0 inc n))
	(/ (* h s) 3))

(simpson-integral cube 0 1 100)
;Value: 1/4
(simpson-integral cube 0 1 1000)
;Value: 1/4
;Since I left the calculation as a ration number, the value is shown to be exactly 0.25

;Exercise 1.30: The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum identity 1 inc 10) 
;Value: 55

(sum cube 1 inc 10) 
;Value: 3025

;Exercise 1.31:

;1. The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to π using the formula52
;π4=2⋅4⋅4⋅6⋅6⋅8⋅⋯3⋅3⋅5⋅5⋅7⋅7⋅⋯.
(define (prod term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod term (next a) next b))))

(define (factorial n)
	(prod identity 1 inc n))

(define (term-denom k)
	(if (even? k) (+ k 1) (+ k 2)))

(define (term-num k)
	(cond 	((= k 1) 2)
			((even? k) (+ k 2))
			(else (+ k 1))))

(define (approx-pi n)
	(define denom (prod term-denom 1 inc n))
	(define num (prod term-num 1 inc n))
	(* 4. (/ num denom))) 

(approx-pi 10)
;Value: 3.2751010413348074
(approx-pi 100)
;Value: 3.1570301764551676

;2. If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (prod term a next b)
  (define (iter a result)
	  (if (> a b)
	      result
	      (iter (next a) (* result (term a)))))
  (iter a 1))

;Exercise 1.32:

;1. Show that sum and product (Exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
;(accumulate 
 ;combiner null-value term a next b)
;Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.
(define (accumulate combiner null-value term a next b)
	 (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
	(accumulate + 0 term a next b))

(define (prod term a next b)
	(accumulate * 1 term a next b))
; for sum the null value is 0 (a+0=a) and the combiner is +
; for prod the null value is 1 (a*1=a) and the combiner is *

;If your accumulate procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

(define (accumulate combiner null-value term a next b)
	 (define (iter a result)
	 	(if (> a b)
      	result
      	(iter (next a) (combiner result (term a)))))
	(iter a null-value))

;Exercise 1.33: You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
(define (filtered-accumulate filter combiner null-value term a next b)
	 (define (filtered-term a)
	 	(if (filter a)
	 		(term a)
	 		null-value))
	 (define (iter a result)
	 	(if (> a b)
      		result
      		(iter (next a) (combiner result (filtered-term a)))))
	(iter a null-value))

;1. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
(define (prime-sq-sum a b) (filtered-accumulate prime? + 0 square a inc b))
;2. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i<n such that GCD(i,n)=1).  
(define (prod-rel-prime n)
	(define (filter i)
		(= (gcd i n) 1))
	(filtered-accumulate filter * 1 identity 1 inc (- n 1)))

;1.3.2 Constructing Procedures Using Lambda
(lambda (x) (+ x 4))
;and
(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;Sometimes we can use internal definitions to get the same effect as with let. For example, we could have defined the procedure f above as

(define (f x y)
  (define a 
    (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))
;We prefer, however, to use let in situations like this and to use internal define only for internal procedures.54

;Exercise 1.34: Suppose we define the procedure

(define (f g) (g 2))
;Then we have

(f square)
4

(f (lambda (z) (* z (+ z 1))))
6
;What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

(f f)
(f 2)
(2 2) ;attempting to treat 2 as a procedure but 2 is not such so it will error

(f f)

;The object 2 is not applicable.

;1.3.3 Procedures as General Methods
(define (search f neg-point pos-point)
  (let ((midpoint 
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond 
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))

(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))

(define (average a b)
	(/ (+ a b) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) 
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value) 
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of 
                   opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point 
   (lambda (y) (average y (/ x y)))
   1.0))

;Exercise 1.35: Show that the golden ratio φ (1.2.2) is a fixed point of the transformation x↦1+1/x, and use this fact to compute φ by means of the fixed-point procedure.
; x = 1 + 1/x => x*x = x + 1 => x*x - x - 1 = 0 => x = (1 +-sqrt(1 + 4))/2 = (1 +- sqrt(5))/2
; so since phi is the positive root of this equation it is a fixed point of the transformation

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
;Value: 1.6180327868852458

;Exercise 1.36: Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display primitives shown in Exercise 1.22. Then find a solution to xx=1000 by finding a fixed point of x↦log(1000)/log(x). (Use Scheme’s primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes with and without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1)=0.)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)	
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;9.965784284662087 step 1
;...
;Value: 4.555532270803653 ~ 35 steps
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
;5.9828921423310435 step 1
;Value: 4.555537551999825 9 steps so with damping is much faster

;Exercise 1.37:

;1. An infinite continued fraction is an expression of the form
;f=N1D1+N2D2+N3D3+….
;As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal to 1 produces 1/φ, where φ is the golden ratio (described in 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation—a so-called finite continued fraction k-term finite continued fraction—has the form
;N1D1+N2⋱+NkDk.
;Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/φ using
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)
;for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?
(define (cont-frac n d k)
	(define (iter i)
		(if (= i k)
			0
			(/ (n i) (+ (d i) (iter (+ i 1))))))
	(iter 1))

;1/phi = 0.61803398875...

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
;Value: .6181818181818182

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           13)
;Value: .6180257510729613 which is accurage to 4 decimals

;2. If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
;iterative process building up fraction starting with term k and adding it to the new denominators
(define (cont-frac n d k)
	(define (iter i result)
		(if (= i 0)
			result
			(iter (- i 1) (/ (n i) (+ result (d i))))))
	(iter k 0))

;Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e−2, where e is the base of the natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, …. Write a program that uses your cont-frac procedure from Exercise 1.37 to approximate e, based on Euler’s expansion.
;e - 2 = 0.71828182845...

(define (d i)
	(let ((r (remainder i 11)))
		(cond 	((= r 1) 1)
				((= r 2) 2)
				((= r 3) 1)
				((= r 4) 1)
				((= r 5) 4)
				((= r 6) 1)
				((= r 7) 1)
				((= r 8) 6)
				((= r 9) 1)
				((= r 10) 1)
				((= r 0) 8))))


(cont-frac (lambda (i) 1.0)
           d
           13)
;Value: .7182818267351813 already accurate to 8 decimal places

;Exercise 1.39: A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:
;tanx=x1−x23−x25−…,
;where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert’s formula. k specifies the number of terms to compute, as in Exercise 1.37.

(define (tan-cf x k)
	(define (n i)
		(if (= i 1) x
			(* -1 (square x))))
	(cont-frac n (lambda (i) (- (* i 2) 1.)) k)) 

(tan-cf 1 13)
;Value: 1.557407724654902

;tangent of 1 radian is 1.55740772465 so this matches to over 6 decimals

;1.3.4 Procedures as Returned Values
;We can express the idea of average damping by means of the following procedure:

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

;Using average-damp, we can reformulate the square-root procedure as follows:
(define (sqrt x)
  (fixed-point 
   (average-damp 
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point 
   (average-damp 
    (lambda (y) 
      (/ x (square y))))
   1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
;along with the definition

(define dx 0.00001)

;With the aid of deriv, we can express Newton’s method as a fixed-point process:

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))
;This provides yet another form of the square-root procedure:

(define (sqrt x)
  (newtons-method 
   (lambda (y) 
     (- (square y) x)) 
   1.0))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt x)
  (fixed-point-of-transform 
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

;Exercise 1.40: Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form

(newtons-method (cubic a b c) 1)
;to approximate zeros of the cubic x3+ax2+bx+c.

(define (cubic a b c)
	(lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;Exercise 1.41: Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned by

(define (double p)
	(lambda (x) (p (p x))))

(((double (double double)) inc) 5)
;Value: 21 which is equivalent to adding 16 which is doubling inc 4 times

(double double)
(lambda (x) (double (double x)))
;which applies double twice
;calling double with this as the argument will then double 4 times and apply to inc resuling in adding 16

;Exercise 1.42: Let f and g be two one-argument functions. The composition f after g is defined to be the function x↦f(g(x)). Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument,
(define (compose f g)
	(lambda (x) (f (g x))))

((compose square inc) 6)
49

;Exercise 1.43: If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f, which is defined to be the function whose value at x is f(f(…(f(x))…)). For example, if f is the function x↦x+1, then the nth repeated application of f is the function x↦x+n. If f is the operation of squaring a number, then the nth repeated application of f is the function that raises its argument to the 2n-th power. Write a procedure that takes as inputs a procedure that computes f and a positive integer n and returns the procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:

((repeated square 2) 5)
625
;Hint: You may find it convenient to use compose from Exercise 1.42.

(define (repeated f n)
	(define (iter i)
		(if (= i n)
			f
			(compose f (iter (+ i 1)))))
	(iter 1))

;Exercise 1.44: The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value at a point x is the average of f(x−dx), f(x), and f(x+dx). Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtain the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from Exercise 1.43.

(define dx 0.001)
(define (smooth f)
	(lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

;to smooth a function n times
(repeated (smooth f) n)

;Exercise 1.45: We saw in 1.3.3 that attempting to compute square roots by naively finding a fixed point of y↦x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y↦x/y2. Unfortunately, the process does not work for fourth roots—a single average damp is not enough to make a fixed-point search for y↦x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y↦x/y3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y↦x/yn−1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of Exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

;n corresponds to the nth root and m is the number of repeated average dampings
(define (nth-root x n m)
  (fixed-point-of-transform 
   (lambda (y) (/ x (expt y (- n 1))))
   (repeated average-damp m)
   1.0))

(nth-root 4 4 1)
;oscillates forever

(nth-root 4 4 2)
1.75
1.499088921282799
1.4211535422753858
1.414264232528399
1.4142135650961398
1.414213562373095
;Value: 1.414213562373095 so two repeated smoothings is sufficient for 4th roots

(nth-root 4 5 1)
;oscillates forever

(nth-root 4 5 2)
1.75
1.419122240733028
1.3109006318788339
1.3218019481550276
1.3189443376397907
1.319649406337878
1.3194725748062066
1.3195167471304052
1.3195057018314507
1.3195084630175
;Value: 1.3195084630175 so two repeated smoothings is sufficient for 5th roots

(nth-root 4 6 2)
1.75
1.3734269947045874
1.2347015652148348
1.2745159220234072
1.2532408651499738
1.2633956234761656
1.2582194659448458
1.2607804868663908
1.259493526361727
1.260135356103848
1.2598140334326664
1.2599745922197598
1.2598942872642036
1.2599344333421092
1.2599143587043606
1.2599243956233894
1.259919377063932
;Value: 1.259919377063932

(nth-root 4 7 2)
;Value: 1.2190169347581832

(nth-root 4 8 2)
;oscillates forever

(nth-root 4 8 3)
1.375
1.2569335286981882
1.2006919022437224
1.1895843345492516
1.1892075333970369
1.1892071150032364
;Value: 1.1892071150032364 so for 8th roots 3 smoothings are required

;3 smoothings work at least up to 15th roots

(nth-root 4 16 3)
;oscillates forever

(nth-root 4 16 4)
1.1875
1.1322673486990298
1.1002893419608526
1.091133631936854
1.0905104181968075
1.0905077327148582
;Value: 1.0905077327148582 so for 16 roots, 4 smoothings are required

;If we extrapolate then nth roots where n = 2^k to 2^(k+1)-1 require at least k smoothings to not oscillate forever

;Exercise 1.46: Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of 1.1.7 and the fixed-point procedure of 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve)
	(define  (iter guess) 
		(if (good-enough? guess)
			guess 
			(iter (improve guess))))
	(lambda (x) (iter x)))

;the sqrt procedure from 1.1.7 was as follows
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;rewriting it with iterative-improve
(define (sqrt x)
	(define (improve guess)
		(average guess (/ x guess)))
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.001))
	((iterative-improve good-enough? improve) 1.0))
;here I embed improve and good-enough inside the same function taking advantage of the fact that x is a local variable and doesn't need to be included in the arguments

;the fixed-point procedure of 1.3.3 was as follows
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;rewriting it with iterative-improve
(define (fixed-point f first-guess)
	(define (close-enough? guess)
		(< (abs (- guess (f guess))) tolerance))
	((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)
;Value: .7390893414033928

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;Value: 1.2587228743052672

;so this seems to work but one inefficient aspect of this is that close-enough? only has one argument so it needs to apply f to the guess to get the next guess but then it will be recalculated inside iterative-improve in the next step.  It would be better to pass the extra result from close-enough? forward to the next iteration instead of forgetting it.  Also close-enough? returning true will result in the current guess being returned instead of in the original procedure where v2 will be returned which is the subsequent guess.