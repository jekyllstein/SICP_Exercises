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

;For tree->list-1, the result for the first tree in 2.16 is
(append (tree->list1 (left-branch tree) (cons 7 (tree->list-1 (right-branch tree)))))
;7 is the first entry
(tree->list-1 (left-branch tree))
;expanding the first term
(append (list 1) (cons 3 (list 5)))
(list 1 3 5)
(tree->list-1 (right-branch tree))
;expanding the second term
(append () (cons 9 (list 11)))
(list 9 11)
;so the final list will be (1 3 5 7 9 11

;For tree->list-2 the result for the first tree in 2.16 is
(copy-to-list (left-branch tree) (cons 7 (copy-to-list (right-branch tree) '())))
;expanding the last term
(copy-to-list (right-branch tree) '())
(copy-to-list '() (cons 9 (copy-to-list (list 11 '() '()) '())))
(cons 9 (cons 11))
;replacing this into the orignal call
(copy-to-list (left-branch tree) (list 7 9 11))
(copy-to-list (list 1 '() '()) (cons 3 (copy-to-list (list 5 '() '()) (list 7 9 11))))
(copy-to-list '() (cons 1 (copy-to-list '() (cons 3 (copy-to-list '() (cons 5 (list 7 9 11)))))))
(cons 1 (cons 3 (cons 5 (list 7 9 11))))
(list 1 3 5 7 9 11)
;so this matches the result for the first method

;For tree->list-1, the result for the second tree in 2.16 is
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11))))))
(list 1 3 5 7 9 11)
;which is the same result as the previous tree

;For tree->list-2, the result for the second tree in 2.16 is


;For tree->list-1, the result for the third tree in 2.16 is
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11))))))
(list 1 3 5 7 9 11)

;2. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?

;both procedures build the list starting in the middle and appending the left branch first followed by the right branch.  For all the trees in Figure 2.16 they will produce an ordered list from 1 to 11.  The diffrence between the first and second procedure is the second uses cons repeatedly to construct the list one element at a time.  The first procedure uses both cons and append so for the first balanced tree in 2.16 at the final step we have (append (list 1 3 5) (list 7 9 11)) which itself will take 3 steps and each of those sublists had an append command.  For the second procedure and this same tree it expands out fully to (cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11))))) as it will for the other trees.  So this procedure will only ever take N steps where N is the number elements in the tree.  The first procedure will take 3 steps to form the right list, 3 steps to form the left list, and then another 3 to append each together so 9 all together instead of 6 for the second procedure.  In general if we have a perfectly balanced tree with an odd number of elements N, then the number of steps for procedure 1 will be (N-1)/2 for the top merger, and then this value repeated down for each subtree until the subtree only has 1 element for which a list of one element is formed so each of teh final leaves will contribute one step.  As an example, take N = 7, then we have 3 + 2*(3-1)/2 + 4 where the number of subtrees doubles each step and the elements are reduced to (N-1)/2.  So the total steps is 3 + 2 + 4 = 9.  To make the next larger balanced tree from 7, we need to append 2 leaves to each leaf of the 7 treee.  There are 4 leaves in that tree so we add an additional 8 yielding 15.  At each level we have 8, 4, 2, 1 elements and the number of steps are (15-1)/2 + 2*(7-1)/2 + 4*(3-1)/2 + 8 = 7 + 6 + 4 + 8 = 25.

;For a tree of size sum([2^i for i in 0:l]) = 2^l+1 - 1 = N where is is the number of levels of the tree, the number of steps is (2^l - 1) + 2*(2^(l-1) - 1) + 4*(2^(l-2) - 1)+...+ 2^l = 2^l + sum((i+1)*2^(l-i) - 1 for i=0:(l-1)) = -3*l + 2^(l+2) - 4 + 2^l.  And 2^l+1 = N + 1, (l+1)*log(2) = log(N+1), (l+1) = log(N+1)/log(2), l = log(N+1)/log(2) - 1.  2^l = (N+1)/2, 2^(l+2) = (N+1)*2.  So the number of steps in terms of N is -3*log(N+1)/log(2) + (N+1)*2 - 4 + (N+1)/2 ~ 5*N/2 - 3*log(N)/log(2) for large N vs N steps for method 1, so about 2.5 times longer for large N.


;Exercise 2.64: The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list. The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))
;1. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

;showing procedure below where each indentation indicates the local variables in the recursive call to partial-tree.  The final line of the indented portion is equal to the value of the variable named prior to the indentation

;elts = (list 1 3 5 7 9 11)
;n = 6, left-size = 2
;left-result = (partial-tree elts 2)
;	n = 2
;	left-size = 0
;	left-result = (partial-tree elts 0) = (cons '() elts) = (() 1 3 5 7 9 11)
;	left-tree = ()
;	non-left-elts = (1 3 5 7 9 11)
;	right-size = 2 - 1 = 1
;	this-entry = 1
;	right-result = (partial-tree (list 3 5 7 9 11) 1)
;		n = 1
;		left-size = 0
;		left-result = (() 3 5 7 9 11)
;		left-tree = ()
;		non-left-elts = (3 5 7 9 11)
;		right-size = 1 - 1 = 0
;		this-entry = 3
;		right-result = (partial-tree (list 5 7 9 11) 0) = (() 5 7 9 11)
;		right-tree = ()
;		remaining-elts = (5 7 9 11)
;		(cons (make-tree 3 () ()) (list 5 7 9 11)) = ((3 () ()) (5 7 9 11))
;	right-tree = (3 () ())
;	remaining-elts = (5 7 9 11)
;	(cons (make-tree 1 () (3 () ())) (5 7 9 11)) = ((1 () (3 () ()))) (5 7 9 11))
;left-tree = (1 () (3 () ()))
;non-left-elts = (5 7 9 11)
;right-size = 6 - 3 = 3
;this-entry = 5
;right-result = (partial-tree (list 7 9 11) 3)
;	n = 3
;	left-size = 1
;	left-result = (partial-tree (list 7 9 11) 1)
;		n = 1
;		left-size = 0
;		left-result = (() 7 9 11)
; 		left-tree = ()
;		non-left-elts = (7 9 11)
;		right-size = 1 - 1 = 0
;		this-entry = 7
;		right-result = (partial-tree (list 9 11) 0) = (() 9 11)
;		right-tree = ()
;		remaining-elts = (9 11)
;		(cons (make-tree 7 () ()) (list 9 11)) = ((7 () ()) (9 11))
;	left-tree = (7 () ())
;	non-left-elts = (9 11)
;	right-size = 3 - 2 = 1
;	this-entry = 9
;	right-result = (partial-tree (list 11) 1)
;		n = 1
;		left-size = 0
;		left-result = (partial-tree (list 11) 0) = (() 11)
;		left-tree = ()
;		non-left-elts = (11)
;		right-size = 1 - 1 = 0
;		this-entry = 11
;		right-result = (partial-tree () 0) = (() ())
;		right-tree = ()
;		remaining-elts = ()
;		(cons (make-tree 11 () ()) ()) = ((11 () ()) ())
;	right-tree = (11 () ())
;	remaining-elts = ()
;	(cons (make-tree 9 (7 () ()) (11 () ())) ()) = ((9 (7 () ()) (11 () ())) ())
;right-tree = (9 (7 () ()) (11 () ()))
;remaining-elts = ()
;(cons (make-tree 5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) ())
;((5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) ())

;			5
;		/		\	
;	1				9
;	  \			  /   \		
;		3		7		11

;the procedure first splits a list in half with the left branch having n-1/2 elements rounding down.  The first element of the right half becomes the entry for the top node and then the remaining right elements are taken as a list which is passed to the same procedure as the original list with the appropriate number of elements.  On the left side the same thing is done with the left list.  In the limiting case if only 1 element remains in the list, then it creates a tree which terminates like (this-entry () ()) with two empty left and right branches.  Similarly if a list with only 2 elements is used then it will create a tree with the smaller entry being the "this-entry" and the larger entry being the right tree which itself terminates with empty branches.  

;2. What is the order of growth in the number of steps required by list->tree to convert a list of n elements?

;There are 11 steps per call of partial-tree and a complete call per element in the list so the number of steps should equal about 11*N which scales with N.

;Exercise 2.65: Use the results of Exercise 2.63 and Exercise 2.64 to give Θ(n) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees.

;this is the implementation for sets as ordered lists
(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        (else (let ((a (car set1)) (b (car set2)))
    		 	(cond ((equal? a b) (cons a (union-set (cdr set1) (cdr set2))))
    		 	   	  ((< a b) (cons a (union-set (cdr set1) set2)))
    		 	   	  (else (cons b (union-set set1 (cdr set2)))))))))

;this implementation for trees uses a simlar procedure but if a is not equal to b then a call to adjoin-set is required to add whichever element is not used as teh top entry to the left or right result for one of the branches, so potentially this woud require a call to adjoin-set for each element in the set so would scale like NlogN
(define (union-set set1 set2)
  (cond ((null? set1) set2) 
        ((null? set2) set1)
        (else (let ( (a (car set1)) 
        			 (b (car set2))
        			 (left-result (union-set (left-branch set1) (left-branch set2)))
        			 (right-result (union-set (right-branch set1) (right-branch set2))))
    		 	(cond ((equal? a b) (make-tree a left-result right-result))
    		 	   	  ((< a b) (make-tree b (adjoin-set a left-result) right-result))
    		 	   	  (else (make-tree a (adjoin-set b left-result) right-result)))))))
;this method also breaks the rule that only one element can exist per tree because if a<b then a is made the entry of the union set but a could still be in the left branch of set 2 and would hten appear in the set twice


;instead we can try a method where we first convert each set to an ordered list and then take the union of the lists
(define (union-set set1 set2)
	(let ((list1 (tree->list-2 set1))
		  (list2 (tree->list-2 set2)))
		(define (union-list list1 list2)
		  (cond ((null? list1) list2) 
		        ((null? list2) list1)
		        (else (let ((a (car list1)) (b (car list2)))
		    		 	(cond ((equal? a b) (cons a (union-list (cdr list1) (cdr list2))))
		    		 	   	  ((< a b) (cons a (union-list (cdr list1) list2)))
		    		 	   	  (else (cons b (union-list list1 (cdr list2)))))))))
	(list->tree (union-list list1 list2))))
;each procedure here scales on the order of N so combined they all scale with N


;this is the implementation for ordered lists
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

;this is the implementation for unordered lists
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

;using the same idea for unordered lists, going through set 1 by exploring the left
;and right branch and adding to the tree if the entry  is found in set2.  Unfortunately 
;this method calls element-of-set? for each element in set1 so it scales like NlogN number
;of steps where N is the size of the set
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) 
         '()
  	  (let ((x1 (entry set1))
  			  (left-result  (intersection-set (left-branch set1) set2))
  			  (right-result (intersection-set (right-branch set1) set2)))
        	 (if (element-of-set? x1 set2)
         		 (make-tree x1 left-result right-result)
                 (intersection-set left-result right-result))))) 


;this solution doesn't work, just testing out ideas
;if a = b, then (make-tree a (intersection-set (left-branch set1) (left-branch set2)) (intersection-set (right-branch set1) (right-branch set2)))
;if a < b, then (intersection-set set1 (left-branch set2)) because a could be in the left-branch of set2.  Then we have to worry about the right-branch of set2
;if a > b, then (intersection-set set1 (right-branch set2))

(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2)) 
         '()
		(let ((a (entry set1))
		      (b (entry set2)))
			 (cond ((= a b)
			 		(make-tree a (intersection-set (left-branch set1) (left-branch set2))
			 					 (intersection-set (right-branch set1) (right-branch set2))))
			 	   ((< a b) 
			 	   	(intersection-set (intersection-set set1 (left-branch set2)) (right-branch set2)))
			 	   ((> a b)
			 	    (intersection-set (intersection-set set1 (right-branch set2)) (left-branch set2)))))))

;from exercise 2.63 and 2.64 we have two procedures to convert a tree to a list and vice versa
;from this we can use the procedure we already have for ordered lists which scales with N
(define (intersection-set set1 set2)
	(let ((list1 (tree->list-2 set1))
		  (list2 (tree->list-2 set2)))
		(define (intersection-list list1 list2)
		  (if (or (null? list1) (null? list2))
		      '()
		      (let ((x1 (car list1)) (x2 (car list2)))
		        (cond ((= x1 x2)
		               (cons x1 (intersection-list 
		                         (cdr list1)
		                         (cdr list2))))
		              ((< x1 x2) (intersection-list 
		                          (cdr list1) 
		                          list2))
		              ((< x2 x1) (intersection-list 
		                          list1 
		                          (cdr list2)))))))
	(list->tree (intersection-list list1 list2))))
;this method first converts each set to a list which takes N steps.  Then it defines intersection-list for ordered lists which itself takes N steps.  Then it converts the resulting list into a tree again which scales on teh order of N. So overall the scaling of this is on the order of N

;Exercise 2.66: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.

;procedure for unordered lists
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup given-key 
                 (cdr set-of-records)))))

;takes advantage of tree structure by branching entries to search left or right depending on the value.  Should scale in number of steps by logN where N is the number of records.
(define (lookup given-key set-of-records)
  (if 	(null? set-of-records) 
  		false
        (let ((x (entry set-of-records))
              (k (key x)))
        	 (cond 
        	 	((equal? given-key k) x)
        	 	((< given-key x) (lookup given-key (left-branch set-of-records)))
        	 	((> given-key x) (lookup given-key (right-branch set-of-records)))))))

;2.3.4Example: Huffman Encoding Trees

;Representing Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;Exercise 2.67: Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))
;Value: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;Use the decode procedure to decode the message, and give the result.

;0 -> a, 1 1 0 -> d, 0 -> a, 1 0 -> b, 1 0 -> b, 1 1 1 -> c, 0 -> a
;so the message is a d a b b c a

(decode sample-message sample-tree)
;Value: (a d a b b c a)

;Exercise 2.68: The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))
;Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree. You should design encode-symbol so that it signals an error if the symbol is not in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.

(define (encode-symbol s tree)
	(if (leaf? tree)
		(if (eq? (symbol-leaf tree) s)
			'()
			(error "Symbol not found in tree"))
		(if (element-of-set? s (symbols (left-branch tree)))
			(cons '0 (encode-symbol s (left-branch tree)))
			(cons '1 (encode-symbol s (right-branch tree))))))

(encode '(a d a b b c a) sample-tree)
;Value: (0 1 1 0 0 1 0 1 0 1 1 1 0)

sample-message
;Value: (0 1 1 0 0 1 0 1 0 1 1 1 0)

;Exercise 2.69: The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))
;Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. Successive-merge is the procedure you must write, using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)

(define (successive-merge leaf-set)
	(cond 
		((null? leaf-set) leaf-set)
		((eq? (length leaf-set) 1) (car leaf-set))
		(else 
			(successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))))

(define pairs (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
;these pairs match the sample-tree generated earlier

(generate-huffman-tree pairs)
;Value: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)
;this matches the result from the sample tree above 
;Value: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

;Exercise 2.70: The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the “symbols” of an “alphabet” need not be individual letters.)


;A    2    NA  16
;BOOM 1    SHA  3
;GET  2    YIP  9
;JOB  2    WAH  1
;Use generate-huffman-tree (Exercise 2.69) to generate a corresponding Huffman tree, and use encode (Exercise 2.68) to encode the following message:

(define pairs (list (list 'a 2) (list 'boom 1) (list 'get 2) (list 'job 2) (list 'na 16) (list 'sha 3) (list 'yip 9) (list 'wah 1)))
;Value: ((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))
(define rock-tree (generate-huffman-tree pairs))
;Value: ((leaf na 16) ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20) (na yip a wah boom sha job get) 36)

;Get a job
;Sha na na na na na na na na

;Get a job
;Sha na na na na na na na na

;Wah yip yip yip yip 
;yip yip yip yip yip
;Sha boom

(define message '(get a job Sha na na na na na na na na get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
(encode message rock-tree)
;Value: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

;How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?
(length (encode message rock-tree))
;Value: 84
;so 84 bits for encoding.  For a fixed length code that can represent 8 symbols, each symbol would require 3 bits.  There are 36 symbols in the message to encode, so the total number of bits would be 108 bits


;Exercise 2.71: Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of the symbols are 1,2,4,…,2n−1. Sketch the tree for n=5; for n=10. In such a tree (for general n) how many bits are required to encode the most frequent symbol? The least frequent symbol?

;for n = 5, the frequencies are 1, 2, 4, 8, 16 so the total is 31
;			(n1 n2 n3 n4 n5) 31
;	n5 16					(n1 n2 n3 n4) 15
;					n4 8				(n1 n2 n3) 7
;								n3 4			(n1 n2) 3
;											n2 2	n1 1

;the most frequen symbol as is all huffman encoding requires 1 bit.  The least frequent symbols requires n-1 bits

;Exercise 2.72: Consider the encoding procedure that you designed in Exercise 2.68. What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of the n symbols are as described in Exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.

;for each bit in the symbol, it needs to check if the symbol is in the symbol list at that level which scales wtih the number of symbols in the list at each level.  In the case of the huffman tree for 2.71, the first branch contains n-1 symbols and this is the only check that needs to happen for the most frequent symbol.  For the least frequent symbol it needs to check n-1, then n-2, then n-3, until n-(n-1), so the total number of steps is 1 + 2 + ... + n-1 ~ n*n/2
