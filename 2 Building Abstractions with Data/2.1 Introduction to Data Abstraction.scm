;2.1.1 Example: Arithmetic Operations for Rational Numbers

;Thus, we can use cons, car, and cdr as follows:

(define x (cons 1 2))

(car x)
1

(cdr x)
2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z))
1

(car (cdr z))
3

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;Now we can try our rational-number procedures:

(define one-half (make-rat 1 2))
(print-rat one-half)
1/2

(define one-third (make-rat 1 3))
(print-rat
 (add-rat one-half one-third))
5/6

(print-rat
 (mul-rat one-half one-third))
1/6

(print-rat
 (add-rat one-third one-third))
6/9

;we can use gcd to reduce the numerator and the denominator to lowest terms before constructing the pair:
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) 
          (/ d g))))

;Now we have

(print-rat 
 (add-rat one-third one-third))
2/3

;Exercise 2.1: Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< n 0)
    	(if (< d 0)
    		(cons (/ (* -1 n) g) 
          		  (/ (* -1 d) g))
    		(cons (/ n g) 
          		  (/ d g)))
    	(if (< d 0)
    		(cons (/ (* -1 n) g) 
          		  (/ (* -1 d) g))
    		(cons (/ n g) 
          		  (/ d g))))))

(make-rat -10 -100)
;Value: (1 . 10)

(make-rat 10 -100)
;Value: (-1 . 10)

(make-rat -10 100)
;Value: (-1 . 10)

(make-rat 10 100)
;Value: (1 . 10)

;2.1.2 Abstraction Barriers
;Exercise 2.2: Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures, you’ll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start-point end-point)
	(cons start-point end-point))
(define (start-segment segment)
	(car segment))
(define (end-segment segment)
	(cdr segment))

(define (make-point x y)
	(cons x y))
(define (x-point point)
	(car point))
(define (y-point point)
	(cdr point))

(define (midpoint-segment segment)
	(let ((point1 (start-segment segment))
		 (point2 (end-segment segment)))
		 (make-point (average (x-point point1) (x-point point2)) (average (y-point point1) (y-point point2)))))

(midpoint-segment (make-segment (make-point 0 0) (make-point 1 1)))
;Value: (1/2 . 1/2)

;Exercise 2.3: Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

;choosing to represent rectangle as a segment and a height which is perpendicular to the base segment, so 5 numbers
(define (make-rectangle segment h)
	(cons segment h))

(define (length segment)
	(let ((point1 (start-segment segment))
		 (point2 (end-segment segment)))
		 (sqrt (+ (square (- (x-point point2) (x-point point1))) (square (- (y-point point2) (y-point point1)))))))
(define (get-base rectangle)
	(length (car rectangle)))

(define (get-height rectangle)
	(cdr rectangle))

(define (perimeter rectangle)
	(+ (* 2 (get-base rectangle)) (* 2 (get-height rectangle))))

(define (area rectangle)
	(* (get-base rectangle) (get-height rectangle)))

(define rec (make-rectangle (make-segment (make-point 0 0) (make-point 0 10)) 2))

(perimeter rec)
;Value: 24.000000000279794
(area rec)
;Value: 20.000000000279794

;Now define rectangle as a center point, base, height, and rotation so 5 numbers again
(define (make-rectangle c b h r)
	(cons (cons (cons b h) r) c))

(define (get-base rectangle)
	(car (car (car rectangle))))

(define (get-height rectangle)
	(cdr (car (car rectangle))))

;since perimeter and area are defined in terms of get-base and get-height once we rewrite these the procedure will work with the new definition of rectangle

(define rec (make-rectangle (make-point 1 5) 10 2 90))

(perimeter rec)
;Value: 24
(area rec)
;Value: 20

;2.1.3 What Is Meant by Data?
;Exercise 2.4: Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y)) yields x for any objects x and y.

(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))
	
(car (cons 1 2))
((cons 1 2) (lambda (p q) p))
(lambda (m) (m 1 2) (lambda (p q) p))
((lambda (p q) p) 1 2)
;Value: 1

(define (cdr z)
	(z (lambda (p q) q)))

(cdr (cons 1 2))
;Value: 2

;Exercise 2.5: Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2a3b. Give the corresponding definitions of the procedures cons, car, and cdr.

(define (cons a b) (* (expt 2 a) (expt 3 b)))

;p = 2^a * 3^b 
;so this is an even number if a and b are both non-negative integers, if we divide this by 2 repeatedly, eventually we'll get an odd number which corresponds to when we repeat a times.  So in this way we can find a, then once we have a we can find b by repeatedly dividing by 3 until we get 1.

(define (car p)
	(define (iter result count)
		(if (even? result)
			(iter (/ result 2) (+ count 1))
			count))
	(iter p 0))

(car (cons 5 6))
;Value: 5

(car (cons 11 12))
;Value: 11

(car (cons 2 14))
;Value: 2


(define (cdr p)
	(define (iter result count)
		(if (= result 1)
			count 
			(iter (/ result 3) (+ count 1))))
	(iter (/ p (expt 2 (car p))) 0))

(cdr (cons 5 6))
;Value: 6

(cdr (cons 11 12))
;Value: 12

(cdr (cons 2 14))
;Value: 14

;Exercise 2.6: In case representing pairs as procedures wasn’t mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the λ-calculus.

;Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).

;if we were to write the procedure inside of zero as a definition we'd have
(define (inner-zero f)
	(define (inner-inner-zero x)
		x))
;so zero contains a procedure with argument f that returns a procedure with argument x that returns the value x

;if we were to write the procedure inside of add-1 as a definition we'd have
(define (add-1 n)
	(define (inner-add-1 f)
		(define (inner-inner-add-1 x)
			(f ((n f) x)))))
;so the number n is a procedure we apply to f, and then apply that to x, and then apply f to that so f is a kind of incrementer procedure.

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
;now what is (zero f)
((lambda (g) (lambda (y) y)) f)
;since g is never used the return result is the same which is the procedure (lambda (y) y), so appyling this substitution above
(lambda (f) (lambda (x) (f ((lambda (y) y) x))))
(lambda (f) (lambda (x) (f x))) ;which can be used to define one

(define one (lambda (f) (lambda (x) (f x))))
;unlike 0, one returns an anonymous function that applies a function f once to an argument x

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
;so what is (one f)
((lambda (g) (lambda (x) (g x))) f)
(lambda (x) (f x))
;so putting it back in above
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
;so two can be defined as a procedure that applies some function f two an argument x twice
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
	(lambda (f) (lambda (x) ((b f) ((a f) x))))) ;composing the procedures defined by a and b

;2.1.4 Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

;Exercise 2.7: Alyssa’s program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))
;Define selectors upper-bound and lower-bound to complete the implementation.

(define (upper-bound interval)
	(cdr interval))

(define (lower-bound interval)
	(car interval))

;Exercise 2.8: Using reasoning analogous to Alyssa’s, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

;if you subtract two intervals then the upper bound is if you subtract the higher upper bound from the lower lower bound and the smallest is if you subtract the lower upper bound from higher lower bound
(define (sub-interval x y)
	(make-interval (- (lower-bound x) 
                    (upper-bound y))
                   (- (upper-bound x) 
                    (lower-bound y))))

;Exercise 2.9: The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

(define (width interval)
	(/ (- (upper-bound interval) (lower-bound interval)) 2))

(define x (make-interval l1 u1))
(define y (make-interval l2 u2))
(width x) = (u1 - l1)/2
(width y) = (u2 - l2)/2
s = (add-interval (x y)) = (make-interval (+ l1 l2) (+ u1 u2))
(width s) = ((u1 + u2) - (l1 + l2))/2 = ((u1 - l1) + (u2 - l2))/2 = (+ (width x) (width y))

d = (sub-interval (x y)) = (make-interval (- l1 u2) (- u1 l2))
(width d) = ((u1 - l2) - (l1 - u2))/2 = ((u1 - l1) + (u2 - l2))/2 = (+ (width x) (width y))

;for multiplications we have p1 = l1*l2, p2 = l1*u2, p3 = u1*l2, p4 = u1*u2, let's say that the interval we end up with is p1 to p4, then the width is ((u1*u2) - (l1*l2))/2 which is not a function of width x and width y.  for example (width x)*(width y) = (u1*u2 - l1*u2 - u1*l2 + l1*l2)/4.  To get rid of the extra cross terms we'd have to explicitely add l1*u2 and u1*l2 which are not themselves functions if width x or width y.  Since division is equivalent to multiplcation of two intervals the same thing applies.

;e.g. 
(define x (make-interval 0.5 0.6))
(define y (make-interval 2.0 2.1))
(define wx (width x))
(define wy (width y))
wx
;Value: .04999999999999999
wy
;Value: 5.0000000000000044e-2

(define m (mul-interval x y))
(width m)
;Value: .13

(define d (div-interval x y))
(width d)
;Value: 3.0952380952380953e-2

;try with new x and y that each have the same width of 0.05
(define x (make-interval 1.1 0.2))
(define y (make-interval 2.5 2.6))
(define m (mul-interval x y))
(width m)
;Value: 1.1800000000000002 which is a different answer than above

(define d (div-interval x y))
(width d)
;Value: .18153846153846157 which is a different answer than above

;Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa’s code to check for this condition and to signal an error if it occurs.
(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))
; if y spans 0 then upper-bound y will be > 0 and lower-bound y will be < 0.  So 1/upper-bound y will still be > 0 and 1/lower-bound y will still be < 0 so the resulting interval formed will not be valid because what is supposed to be the lower bound will be greater than the upper bound.
(define (make-interval a b) 
	(if (> a b)
		(error "Attemping to form interval with lower bound > upper bound")
		(cons a b)))

(define x (make-interval 1.1 1.2))
(define y (make-interval -.1 .1))
(div-interval x y)
;Attemping to form interval with lower bound > upper bound

;Exercise 2.11: In passing, Ben also cryptically comments: “By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.” Rewrite this procedure using Ben’s suggestion.

;here | represents the sepration from negative to positive numbers so the 9 cases are the sign of the lower and upper bounds of x and y
;lx ux | ly uy => lx*uy, ux*ly x
;lx ux ly | uy => lx*uy, lx*ly x
;lx | ux ly uy => lx*uy, ux*uy x
;lx ux ly uy | => ux*uy is smaller than lx*ly x
;ly uy | lx ux => ly*ux, uy*lx x
;ly uy lx | ux => ly*ux, ly*lx x
;ly | uy lx ux => ly*ux, uy*ux
;| lx ux ly uy => lx*ly is smaller than ux*uy x
;lx ly | ux uy lx*ly > 0, ux*uy>0, not sure which is bigger so need to check more cases
;only the case where both x and y span 0 needs to check for multiple cases

(define (mul-interval x y)
  (let ((lx (lower-bound x))
  		(ux (upper-bound x))
  		(ly (lower-bound y))
  		(uy (upper-bound y)))
  		(define (neg? a) (< a 0))
  		(define (pos? a) (NOT (neg? a)))
  		(define (check-cond a b c d)
  			(AND (a lx) (b ux) (c ly) (d uy)))
  		(cond 	((check-cond neg? neg? pos? pos?) (make-interval (* lx uy) (* ly ux)))
  				((check-cond neg? neg? neg? pos?) (make-interval (* lx uy) (* lx ly)))
  				((check-cond neg? pos? pos? pos?) (make-interval (* lx uy) (* ux uy)))
  				((check-cond neg? neg? neg? neg?) (make-interval (* ux uy) (* lx ly)))
  				((check-cond pos? pos? neg? neg?) (make-interval (* ly ux) (* uy lx)))
  				((check-cond neg? pos? neg? neg?) (make-interval (* ly ux) (* lx ly)))
  				((check-cond pos? pos? neg? pos?) (make-interval (* ly ux) (* uy ux)))
  				((check-cond pos? pos? pos? pos?) (make-interval (* lx ly) (* ux uy)))  
	  			  (else (let (  (p1 (* lx ly))
						        (p2 (* lx uy))
						        (p3 (* ux ly))
						        (p4 (* ux uy)))
							    (make-interval (min p1 p2 p3 p4)
							                   (max p1 p2 p3 p4)))))))  

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

;Exercise 2.12: Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.

(define (make-center-percent c p)
	(let ((w (abs (* c p))))
		(make-interval (- c w) (+ c w))))

(define (percent interval)
	(define (average x y)
		(/ (+ x y) 2))
	(/ (width interval) (center interval)))

(make-center-percent 10 .2)
;Value: (8. . 12.)

(percent (make-interval 8 12))
;Value: 1/5

;Exercise 2.13: Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

;x is an inverval with (cx-(cx*tx), cx+(cx*tx) = (cx(1-tx), cx(1+tx))
;similarly y is an interval (cy(1-ty), cy(1+ty))
;assuming all numbers are positive then x*y is (cx(1-tx)*cy(1-ty), (cx(1+tx)*cy(1+ty)))
;expanding the lower and upper bound we have (cx*cy*(1-ty-tx+tx*ty)), cx*cy*(1+tx+ty+tx*ty)))
;this looks like a percent interval with center cx*cy and tolerance tx*ty except for an extra factor of +tx*ty for both the upper and lower bound, but in the limit where the tolerance is very small then these terms are much smaller than tx or ty so it simplifies to simply having the percent tolerance of the produce equal to the product of the percent tollerances.

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

;Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see Exercise 2.12).
(define r1 (make-center-percent 1 0.01))
;Value: r1

(define r2 (make-center-percent 2 0.01))
;Value: r2

(par1 r1 r2)
;Value: (.646930693069307 . .686936026936027)

(par2 r1 r2)
;Value: (.66 . .6733333333333333)

;so we get different answers for par1 and par2

(define A (make-center-percent 100 0.01))
(define B (make-center-percent 200 0.01))

(div-interval a a)
(div-interval a b)

(div-interval a a)
;Value: (.9801980198019802 . 1.0202020202020203)
;about 2% tolerance

(div-interval a b)
;Value: (.4900990099009901 . .5101010101010102)
;also about 2% tolerance

;Exercise 2.15: Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute with intervals using Alyssa’s system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she says, par2 is a “better” program for parallel resistances than par1. Is she right? Why?
;so since 1 is an interval with tolerance for par2 we get tr1 + tr2 as the final tolerance
;for par1 r1*r2 will have tolerance (tr1*tr2), then r1+r2 will have tolerance (tr1+tr2), and then the final division will have tolerance (tr1*tr2 * (tr1+tr2)) which is larger than the par2 calculation.  In general adding more expressions with uncertain terms will grow the tolerance value so rewriting equations so that they only appear once is better for keeping the tolerance low.

;Exercise 2.16: Explain, in general, why equivalent algebraic expressions may lead to different answers. Can you devise an interval-arithmetic package that does not have this shortcoming, or is this task impossible? (Warning: This problem is very difficult.)
;If you had a symbolic algebraic simplifier then it could be possible but once you complete the calculation of sub expressions then you grow the tolerance values.  As a starting point you could for example add to the definition of div-interval so that if both arguments are the same it returns (make-interval 1 1)

