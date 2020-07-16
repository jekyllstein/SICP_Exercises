;2.3.1 Quotation

;Exercise 2.53: What would the interpreter print in response to evaluating each of the following expressions?

(list 'a 'b 'c)
; (a b c)
(list (list 'george))
; ((george))
(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
; (y1 y2)
(pair? (car '(a short list)))
; false
(memq 'red '((red shoes) (blue socks)))
; false
(memq 'red '(red shoes blue socks))
; (red shoes blue socks)

;Exercise 2.54: Two lists are said to be equal? if they contain equal elements arranged in the same order. For example,

(equal? '(this is a list) 
        '(this is a list))
;is true, but

(equal? '(this is a list) 
        '(this (is a) list))
;is false. To be more precise, we can define equal? recursively in terms of the basic eq? equality of symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using this idea, implement equal? as a procedure.

(define (equal? a b)
	(if (pair? a)
		(if (pair? b)
			(if (eq? (car a) (car b))
				(equal? (cdr a) (cdr b))
				false)
			false)
		(if (pair? b)
			false
			(eq? a b))))

;Exercise 2.55: Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)
;To her surprise, the interpreter prints back quote. Explain.

;using ' before a list object creates a list of symbols of the following list.  Using ' before a quoted expression applies the ' first to the ' term and then to the following expression forming a list of the two.  The symbolic representation of the ' symbol is itself quote which will be the first element of the list produced

;2.3.2Example: Symbolic Differentiation

;Exercise 2.56: Show how to extend the basic differentiator to handle more kinds of expressions. For instance, implement the differentiation rule
;d(un)dx=nun−1dudx
;by adding a new clause to the deriv program and defining appropriate procedures exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.) Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself.
(define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**)))
(define (make-exponentiation e1 e2)
	(cond ((=number? e2 0) 1)
		  ((=number? e2 1) e1)
		  (else (list '** e1 e2))))
(define (base x)
	(cadr x))
(define (exponent x)
	(caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
	         (make-product
	          (exponent exp)
	          (make-exponent 
	           (base exp)
	           (make-sum (exponent exp) -1)))
	     	  (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

;Exercise 2.57: Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the last example above could be expressed as

(deriv '(* x y (+ x 3)) 'x)
;Try to do this by changing only the representation for sums and products, without changing the deriv procedure at all. For example, the addend of a sum would be the first term, and the augend would be the sum of the rest of the terms.

(define (make-sum a1 a2 . rest) (append (list '+ a1 a2) rest))
(define (make-product m1 m2 . rest) (append (list '* m1 m2) rest))
(define (augend s) 
	(let ((term3 (cddr s)))
		(if (null? (cdr term3))
			(car term3)
			(append '(+) term3))))

(define (multiplicand s) 
	(let ((term3 (cddr s)))
		(if (null? (cdr term3))
			(car term3)
			(append '(*) term3))))

;Exercise 2.58: Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

;1. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.
(define (make-sum a b)
	(list a '+ b))
(define (make-product a b)
	(list a '* b))
(define (addend s)
	(car s))
(define (augend s)
	(caddr s))
(define (multiplier p)
	(car p))
(define (multiplicand p)
	(caddr p))
(define (sum? x)
	(and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
	(and (pair? x) (eq? (cadr x) '*)))

(define expr (make-sum 'x (make-product 3 (make-sum 'x (make-sum 'y 2)))))
;Value: (x + (3 * (x + (y + 2))))

(deriv expr 'x)
;Value: (1 + ((3 * (1 + (0 + 0))) + (0 * (x + (y + 2)))))

(deriv expr 'y)
;Value: (0 + ((3 * (0 + (1 + 0))) + (0 * (x + (y + 2)))))

;these results are unsimplified.  I can improve the appearance by adding the simplication conditions to the make-sum and make-product procedures
(define (make-sum a b)
	(cond ((=number? a 0) b)
		  ((=number? b 0) a)
		  ((and (number? a) (number? b))
		    (+ a b))
		  (else (list a '+ b))))
(define (make-product a b)
	(cond ((or (=number? a 0)
			   (=number? b 0))
            0) 
		   ((=number? a 1) b)
		   ((=number? b 1) a)
		   ((and (number? a) (number? b))
		    (* a b))
		   (else (list a '* b))))

(deriv expr 'x)
;Value: 4

(deriv expr 'y)
;Value: 3


;2. The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?

(define (make-sum a b)
	(cond ((=number? a 0) b)
		  ((=number? b 0) a)
		  ((and (number? a) (number? b))
		    (+ a b)) 
		  ((or (and (sum? a) (sum? b)) (and (product? a) (product? b)))
			 (append a (append '(+) b)))
		  ((or (sum? b) (product? b))
		  	 (append (list a '+) b))
		  ((or (sum? a) (product? a))
		  	 (append a (list '+ b)))
		  (else (list a '+ b))))
(make-sum 'x (make-sum 'y 2))
;Value: (x + y + 2)

(define (make-product a b)
	(cond ((or (=number? a 0)
			   (=number? b 0))
            0) 
		   ((=number? a 1) b)
		   ((=number? b 1) a)
		   ((and (number? a) (number? b))
		    (* a b))
		   ((and (product? a) (product? b)) (append a (append '(*) b)))
		   ((product? a) (append a (list '* b)))
		   ((product? b) (list a (append '(*) b)))
		   (else (list a '* b))))
(make-sum 'x (make-product 3 (make-sum 'x (make-sum 'y 2)))))
;Value: (x + 3 * (x + y + 2))

;so far we create the expected simplified algebraic expressions
(define (addend s)
	(car s))
(define (augend s) 
	(let ((term3 (cddr s)))
		(if (null? (cdr term3))
			(car term3)
			term3)))
(define (multiplier p)
	(car p))
(define (multiplicand s) 
	(let ((term3 (cddr s)))
		(if (null? (cdr term3))
			(car term3)
			term3)))
;the augend and mutiplicand are modified like in the list case so that if the second term is itself a pair then use it
(define (sum? x)
	(and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
	(and (pair? x) (eq? (cadr x) '*)))

(define expr (make-sum 'x (make-product 3 (make-sum 'x (make-sum 'y 2)))))
;Value: (x + 3 * (x + y + 2))

(deriv expr 'x)
;Value: 4

(deriv expr 'y)
;Value: 3

;so at for this case we get the same solution as before.  What about with a case where the answer should have symbols?
(define expr2 (make-product 'x (make-sum 'x 5)))
;Value: (x * (x + 5))

(deriv expr2 'x)
;Value: (x + x + 5)

;looks good expect we don't have the simplification rule to convert a sum of identical elements into a product times 2

(define (make-sum a b)
	(cond ((=number? a 0) b)
		  ((=number? b 0) a)
		  ((and (number? a) (number? b))
		    (+ a b))
		  ((equal? a b) (make-product 2 a)) 
		  ((or (and (sum? a) (sum? b)) (and (product? a) (product? b)))
			 (append a (append '(+) b)))
		  ((or (sum? b) (product? b))
		  	 (if (and (sum? b) (equal? (car b) a))
		  	 	 (append (make-product 2 a) (cdr b)) 
			  	 (append (list a '+) b)))
		  ((or (sum? a) (product? a))
		  	 (append a (list '+ b)))
		  (else (list a '+ b))))

;this also has an added rule that if a is not a sum or product and b is a sum, then check if a matches the addend of b to simplify this to 2*a.  This ensures that in our case the sum (x + x + 5) will simplify to (2*x + 5) even though x doesnt match x+5 which is the augend.

(deriv expr2 'x)
;Value: (2 * x + 5)

(make-sum (make-product 2 'x) (make-product 3 'x))
;Value: (2 * x + 3 * x)
;we can see with an expression like this that the value should be 5x but making all these simplification rules would require even more checks into make-sum

;2.3.3Example: Representing Sets
;Exercise 2.59: Implement the union-set operation for the unordered-list representation of sets.
(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) 
                                set2)))))

(union-set (list 1 2 3) (list 1 2 3))
;Value: (1 2 3)

(union-set (list 1 2 3) (list 2 3 4))
;Value: (1 2 3 4)

;Exercise 2.60: We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on this representation. How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one?

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;this procedure can be the same as before since as soon as the element is encountered it is in the set in both cases

(define (adjoin-set x set)
      (cons x set))
;since duplicates are allowed here, we can just adjoin the element no matter what

(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) 
                                set2)))))
;no longer checks if it is already an element, just adds all the elements from each set together

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))
;remains unchanged from before but here the element-of-set? procedure will take longer to run because set2 has duplicates and some elements of set1 might be checked twice as well if they are in both

;for set union and adjoining these procedures will be faster because there's no element checking but for all other procedures it will be slower


;Exercise 2.61: Give an implementation of adjoin-set using the ordered representation. By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
  		((= x (car set)) set)
  		((< x (car set)) (cons x set))
  		(else (cons (car set) (adjoin-set x (cdr set))))))
;on average will only need to check half the elements in the set because once x is smaller than the first item in the rest of the set it is inserted in

(adjoin-set 3 (list 1 2 4 5 6))
;Value: (1 2 3 4 5 6)

;Exercise 2.62: Give a Θ(n) implementation of union-set for sets represented as ordered lists.
(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        (else (let ((a (car set1)) (b (car set2)))
    		 	(cond ((equal? a b) (cons a (union-set (cdr set1) (cdr set2))))
    		 	   	  ((< a b) (cons a (union-set (cdr set1) set2)))
    		 	   	  (else (cons b (union-set set1 (cdr set2)))))))))

(union-set (list 1 2 3) (list 2 3 4))
;Value: (1 2 3 4)

(union-set (list 1 2 3) (list 6 7 8))
;Value: (1 2 3 6 7 8)

(union-set (list 1 3 5) (list 2 4 6))
;Value: (1 2 3 4 5 6)

;Exercise 2.63: Each of the following two procedures converts a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))
;1. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?
;2. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?