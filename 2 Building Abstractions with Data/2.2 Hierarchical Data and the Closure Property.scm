;2.2.1 Representing Sequences

;Exercise 2.17: Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (last-pair l)
	(cond 	((null? l) (error "list is empty"))
			((= (length l) 1) l)
		(else (last-pair (cdr l)))))

(last-pair (list 23 72 149 34))
(34)


;Exercise 2.18: Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(define (reverse l)
	(define (iter oldl newl)
		(if (null? oldl) 
			newl
			(iter (cdr oldl) (cons (car oldl) newl))))
	(iter l ()))

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)

;Exercise 2.19: Consider the change-counting program of 1.2.2. It would be nice to be able to easily change the currency used by the program, so that we could compute the number of ways to change a British pound, for example. As the program is written, the knowledge of the currency is distributed partly into the procedure first-denomination and partly into the procedure count-change (which knows that there are five kinds of U.S. coins). It would be nicer to be able to supply a list of coins to be used for making change.

;We want to rewrite the procedure cc so that its second argument is a list of the values of the coins to use rather than an integer specifying which coins to use. We could then have lists that defined each kind of currency:

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))
We could then call cc as follows:

(cc 100 us-coins)
292
;To do this will require changing the program cc somewhat. It will still have the same form, but it will access its second argument differently, as follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))
;Define the procedures first-denomination, except-first-denomination and no-more? in terms of primitive operations on list structures. Does the order of the list coin-values affect the answer produced by cc? Why or why not?

(define (first-denomination coin-values)
	(car coin-values))

(define (except-first-denomination coin-values)
	(cdr coin-values))

(define (no-more? coin-values)
	(null? coin-values))

; the recursive procedure doesn't depend on the order of the coins considered
(define us-coins 
  (list 5 25 50 1 10))

(cc 100 us-coins)
;Value: 292 same answer as before

;Exercise 2.20: The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures is to use define with dotted-tail notation. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter’s value will be a list of any remaining arguments. For instance, given the definition

(define (f x y . z) ⟨body⟩)
;the procedure f can be called with two or more arguments. If we evaluate

(f 1 2 3 4 5 6)
;then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the definition

(define (g . w) ⟨body⟩)
;the procedure g can be called with zero or more arguments. If we evaluate

(g 1 2 3 4 5 6)
;then in the body of g, w will be the list (1 2 3 4 5 6).77

;Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)

(same-parity 2 3 4 5 6 7)
(2 4 6)

(define (same-parity a . b)
	(define (check-parity n)
		(if (even? a)
			(even? n)
			(odd? n)))
	(define (filter l)
		(if (null? l)
			()
			(if (check-parity (car l))
				(cons (car l) (filter (cdr l)))
				(filter (cdr l)))))
	(cons a (filter b)))

;Exercise 2.21: The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)
;Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:

(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;Exercise 2.22: Louis Reasoner tries to rewrite the first square-list procedure of Exercise 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
;Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?
;after the first call of iter you'll have (cons (square item1) nil) which will then be passed as the answer to the next function call.  After that the second item will be appended to the start but we will still have a situation where the first item is the inner most part of the answer list.

;Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))
;This doesn’t work either. Explain.
;now the first iteration of answer will start with (cons nil item1) which isn't even a correct list object.  The next iteration will produce (cons (cons nil items 1) item2) which won't produce a valid list either since the first object in the final cons will be a deeply nested object instead of a single value.

;Exercise 2.23: The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all—for-each is used with procedures that perform an action, such as printing. For example,

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

57
321
88
;The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as true. Give an implementation of for-each.

(define (for-each p l)
	(if (null? l)
		true
		(let ((a (car l)))
			 (p a)
			 (for-each p (cdr l)))))

;2.2.2 Hierarchical Structures

;Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in Figure 2.6).

;Value: (1 (2 (3 4)))


;Exercise 2.25: Give combinations of cars and cdrs that will pick 7 from each of the following lists:
(1 3 (5 7) 9)
(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))
;Value: 7

((7))
(define l (list (list 7)))
(car (car l))
;Value: 7

(1 (2 (3 (4 (5 (6 7))))))
(define l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))
;Value: 7

;Exercise 2.26: Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))
;What result is printed by the interpreter in response to evaluating each of the following expressions:

(append x y)
;(1 2 3 4 5 6)
(cons x y)
;((1 2 3) 4 5 6)
(list x y)
;((1 2 3) (4 5 6))

;Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

(define (reverse l)
	(define (iter oldl newl)
		(if (null? oldl) 
			newl
			(iter (cdr oldl) (cons (car oldl) newl))))
	(iter l ()))

(define x 
  (list (list 1 2) (list 3 4)))

x
;Value: ((1 2) (3 4))

(reverse x)
;Value: ((3 4) (1 2))

(define (deep-reverse l)
	(define (iter oldl newl)
		(if (null? oldl) 
			newl
			(let ((head (car oldl)))
				 (if (pair? head) 
					  (iter (cdr oldl) (cons (iter head ()) newl))
					  (iter (cdr oldl) (cons head newl))))))
				 	 
	(iter l ()))

(define (deep-reverse l)
	(define (iter oldl newl)
		(if (null? oldl) 
			newl
			(let ((head (car oldl)))
				  (iter (cdr oldl) (cons (if (pair? head) (iter head ()) head) newl)))))
	(iter l ()))

(deep-reverse x)
;Value: ((4 3) (2 1))

;Exercise 2.28: Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

(define x 
  (list (list 1 2) (list 3 4)))

(define (fringe x)
	(cond ((null? x) x)
		  ((NOT (pair? x)) (list x))
		  (else (append (fringe (car x)) (fringe (cdr x))))))
	
(fringe x)
;Value: (1 2 3 4)

(fringe (list x x))
;Value: (1 2 3 4 1 2 3 4)

;Exercise 2.29: A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))
;A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))
;1. Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch))) 


;2. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
(define (total-weight mobile)
	(define (get-weight branch)
		(let ((s (branch-structure branch)))
		     (if (pair? s)
		     	 (total-weight s)
		     	 s)))
	(+ (get-weight (left-branch mobile)) (get-weight (right-branch mobile))))

;3. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.
(define (balanced? mobile)
	(define (branch-torque branch)
		(let ((s (branch-structure branch)))
			 (* (branch-length branch) 
				(if (pair? s)
				    (total-weight s)
				    s))))
	(if (NOT (pair? mobile))
		true
		(let ((lb (left-branch mobile))
			  (rb (right-branch mobile)))
			  (if (= (branch-torque lb) (branch-torque rb))
			  	  (AND (balanced? (branch-structure lb)) (balanced? (branch-structure rb)))
			  	  false))))


;4. Suppose we change the representation of mobiles so that the constructors are
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))
;How much do you need to change your programs to convert to the new representation?
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))
;these need to be changed since cdr is now sufficient to extract the second piece of each pair

(define (scale-tree tree factor)
  (cond ((null? tree) ())
        ((not (pair? tree)) 
         (* tree factor))
        (else
         (cons (scale-tree (car tree) 
                           factor)
               (scale-tree (cdr tree) 
                           factor)))))

(scale-tree (list 1 
                  (list 2 (list 3 4) 5) 
                  (list 6 7))
            10)
;Value: (10 (20 (30 40) 50) (60 70))

;Exercise 2.30: Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21. That is, square-tree should behave as follows:
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;Value: (1 (4 (9 16) 25) (36 49))
;Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

(define (square-tree tree)
  (map (lambda (sub-tree) 
  		  (if (pair? sub-tree)
  		      (square-tree sub-tree)
  		      (square sub-tree)))
  	    tree))

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) 
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

;Exercise 2.31: Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

(define (square-tree tree) 
  (tree-map square tree))


(define (tree-map p tree)
  (map (lambda (sub-tree) 
  		  (if (pair? sub-tree)
  		      (tree-map p sub-tree)
  		      (p sub-tree)))
  	    tree))

;Exercise 2.32: We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l)) rest)))))

;so the subsets of set s is the subsets of s excluding the first element plus each of those subsets with the first element appended.  As this process continues down recursively you get to the base case of an empty list, which gets the last element appended to the empty list.  So we'll build up the set with stacks of rest as follows from the inner most case.
;rest = ()
;rest = ((sn) ())
;rest = ((sn-1 sn) (sn-1) (sn) ())
;rest = ((sn-2 sn-1 sn) (sn-2 sn-1) (sn-2) (sn-1 sn) (sn-1) (sn) ())
;and so on so every combination of subsets is reached

;2.2.3 Sequences as Conventional Interfaces
(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate 
   +
   0
   (map square
        (filter odd?
                (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate 
   cons
   nil
   (filter even?
           (map fib
                (enumerate-interval 0 n)))))

;Exercise 2.33: Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              () sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (a b) (+ b 1)) 0 sequence))

;Exercise 2.34: Evaluating a polynomial in x at a given value of x can be formulated as an accumulation. We evaluate the polynomial
;anxn+an−1xn−1+⋯+a1x+a0
;using a well-known algorithm called Horner’s rule, which structures the computation as
;(…(anx+an−1)x+⋯+a1)x+a0.
;In other words, we start with an, multiply by x, add an−1, multiply by x, and so on, until we reach a0.82

;Fill in the following template to produce a procedure that evaluates a polynomial using Horner’s rule. Assume that the coefficients of the polynomial are arranged in a sequence, from a0 through an.

(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms))) 
   0
   coefficient-sequence))
;For example, to compute 1+3x+5x3+x5 at x=2 you would evaluate

(horner-eval 2 (list 1 3 0 5 0 1))

;Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (a) (if (pair? a) (count-leaves a) 1)) t)))

;Exercise 2.36: The procedure accumulate-n is similar to accumulate except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the sequence (22 26 30). Fill in the missing expressions in the following definition of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Exercise 2.37: Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as sequences of vectors (the rows of the matrix). For example, the matrix
;146257368469
;is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:
;(dot-product v w)(matrix-*-vector m v)(matrix-*-matrix m n)(transpose m)returns the sumΣiviwi;returns the vectort,whereti=Σjmijvj;returns the matrixp,wherepij=Σkmiknkj;returns the matrixn,wherenij=mji.
;We can define the dot product as83

(define mat (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
;Value: ((1 2 3 4) (4 5 6 6) (6 7 8 9))

(define v (list 1 2 3 4))
;Value: (1 2 3 4)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in Exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))
;Value: ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

;Exercise 2.38: The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;What are the values of

(fold-right / 1 (list 1 2 3))
; 1/1=2, 2/1=2, 3/2
;Value: 3/2
(fold-left  / 1 (list 1 2 3))
; 1/1=1, 1/2, 1/2/3=1/6
;Value: 1/6
(fold-right list () (list 1 2 3))
; (3 ()), (2 (3 ())), (1 (2 (3 ())))
(fold-left  list () (list 1 2 3))
; (() 1), ((() 1) 2), (((() 1) 2) 3) 
;Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.

;(op a b) = (op b a) for all b and a

;Exercise 2.39: Complete the following definitions of reverse (Exercise 2.18) in terms of fold-right and fold-left from Exercise 2.38:

(define (reverse sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) () sequence))

(reverse (list 1 2 3))
;Value: (3 2 1)

(define (reverse sequence)
  (fold-left 
   (lambda (x y) (cons y x)) () sequence))
(reverse (list 1 2 3))
;Value: (3 2 1)


(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

;Exercise 2.40: Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with 1≤j<i≤n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (unique-pairs n)
	(flatmap 
		(lambda (i) 
			(map 
				(lambda (j) 
					(cons i j)) 
				(enumerate-interval 1 (- i 1)))) 
	(enumerate-interval 1 n)))

(unique-pairs 5)
;Value: ((2 . 1) (3 . 1) (3 . 2) (4 . 1) (4 . 2) (4 . 3) (5 . 1) (5 . 2) (5 . 3) (5 . 4))

(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter
			prime-sum?
				(unique-pairs n))))

;Exercise 2.41: Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.

(define (triple-sum-filter n s)
	(filter 
		(lambda (t) 
			(= (accumulate + 0 t) s))
		(flatmap
			(lambda (i)
				(flatmap
					(lambda (j)
						(map 
							(lambda (k) 
								(list i j k))
						(enumerate-interval 1 n)))
					(enumerate-interval 1 n)))
			(enumerate-interval 1 n))))

(triple-sum-filter 3 3)
;Value: ((1 1 1))

(triple-sum-filter 3 9)
;Value: ((3 3 3))

(triple-sum-filter 3 4)
;Value: ((1 1 2) (1 2 1) (2 1 1))

;Exercise 2.42: The “eight-queens puzzle” asks how to place eight queens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal). One possible solution is shown in Figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k−1 queens, we must place the kth queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively: Assume that we have already generated the sequence of all possible ways to place k−1 queens in the first k−1 columns of the board. For each of these ways, generate an extended set of positions by placing a queen in each row of the kth column. Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens. This produces the sequence of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.

;We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n×n chessboard. Queens has an internal procedure queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;In this procedure rest-of-queens is a way to place k−1 queens in the first k−1 columns, and new-row is a proposed row in which to place the queen for the kth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe—the other queens are already guaranteed safe with respect to each other.)

;positions will be a list of pairs of length k where the first number represents the column and the second number represents the row that the queen is in

(define (adjoin-position new-row k rest-of-queens)
	(append rest-of-queens (list (cons k new-row))))

(define empty-board ())

;for each position, the rows for each k-1 columns are all illegal and the diagonal values, for the 1st column if a queen is in row a, then row a in column k is not safe as well as row a + k and row a - k
(define (safe? k positions)
	(define (get-last l)
		(define (iter result tail)
			(if (null? tail)
				result
				(iter (car tail) (cdr tail))))
		(iter (car l) (cdr l)))
	(if (= (length positions) 1)
		true 
		(let ((last-row (cdr (get-last positions))))
			 (define (iter p)
			 	(let ((currentp (car p)))
			 		 (let ((currentc (car currentp))
			 		 	   (currentr (cdr currentp)))
			 		  	   (cond ((= currentc k) true)
			 		  	   	     ((= currentr last-row) false)
			 		  	   	     ((= (+ currentr (- k currentc)) last-row) false)
			 		  	   	     ((= (- currentr (- k currentc)) last-row) false)
				 	 	  		 (else (iter (cdr p)))))))
				(iter positions))))

;Exercise 2.43: Louis Reasoner is having a terrible time doing Exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6×6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))
;Explain why this interchange makes the program run slowly. Estimate how long it will take Louis’s program to solve the eight-queens puzzle, assuming that the program in Exercise 2.42 solves the puzzle in time T.

;Comparing to the initial solution
(flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position 
            new-row k rest-of-queens))
         (enumerate-interval 1 board-size)))
  (queen-cols (- k 1)))

;At the top level of the procedure, board-size is fixed and passed as the first k to the queen-cols call.  The inner most map runs from 1 to the entire board size as new rows adjoining the positions in rest-of-queens to new queens in column k.  At the bottom level, k = 0 and then the functions returns an empty board which is just an empty list.  So for that step we'd have 
(map (lambda (new-row)
       (adjoin-position 
        new-row k empty-board))
     (enumerate-interval 1 board-size))
;In this case the 0 being passed to queen-cols corresponds to a value of 1 for k in the inner map, so new-row iterates from 1 to board-size while k stays fixed at 1.  So we'd create a list of positions like this (((1 . 1)) ((1 . 2)) ((1 . 3))) etc... since positions are represented as a list of pairs where each pair is the column and row of the queen.  Then this list of positions for (queen-cols 1) is filtered for safe positions for the queen in column 1 with respect to the positions we have so far.  Since this list of positions only has one queen in each, it has to be safe so nothing will be filtered away.  With a board size of 4 then the next call will be
(flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position 
            new-row 2 rest-of-queens))
         (enumerate-interval 1 4)))
  (list (list (cons 1 1)) (list (cons 1 2)) (list (cons 1 3)) (list (cons 1 4))))
;Now each of these position lists will be adjointed to positions for queens in column 2 and rows 1-4.  For the first position list this will yield a new list of positions (((1 . 1) (2 . 1)) ((1 . 1) (2 . 2)) ((1 . 1) (2 . 3)) ((1 . 1) (2 . 4))) each with two queens and similarly for the other positions.  So we'll have 4 positions produced for each of the 4 positions so far yielding 16 positions that will all be combined in the flat map into one list of positions.  Then each of these 16 will be filtered for whether they are safe for the queen added in column 2 since every queen added was in column 2.  For the first set of 4 positions, the first two will be filtered out since the queen in (1 . 1) blocks one queen in its row and another in its diagonal.  For the second set of 4 positions starting with (1 . 2) it will block 3 queens because it blocks both its row and two diagonals.  This is true for the 3rd set as well and then the 4th set only has 2 blocked again.  So the flatmap after the filter will contain (4 - 2) + (4 - 3) + (4 - 3) + (4 - 2) = 6.  Then each of these 6 will have 4 new positions cretaed, concatenated, and then filtered themselves.  For example let's consider the first position of ((1 . 1) (2 . 3)) which will yield 4 new positions by adding either (3 . 1), (3 . 2), (3 . 3), or (3 . 4).  Clearly at least two of these, additions, in the rows of the original are invalid as well as any diagonal conflict.  In this case the invalid additions are (3 . 1), (3 . 3), (3 . 2), (3 . 4) so all of them.  For this type of case the maxmimum number of additions that would be allowed is N - 3 and the mininum would be N - 6 in which both rows and both diagonals are independent.  Similar to the k = 2 case, the number of valid remaining positions can be grouped into the branches from each of the 4 original positions (2)   If no positions are filtered along the way then for k = 3 we'd have 64 positions to evaluate and then 256 for the last step.  But the exponential growth is substantially reduced by filtering safe positions at each step so if after k = 2 we only have 4 safe positions out of 16, then at the next step we'd only have to evaluate 16 total positions instead of 64.   

;Now let's compare this to Louis Reasoner's solution
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;so starting at the base level (queen-cols 0) will still return (list empty-board) so with k = 1 we have
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row 1 rest-of-queens))
        (list empty-board)))
 (enumerate-interval 1 4))
;which will create a list of positions (((1 . 1)) ((1 . 2)) ((1 . 3)) ((1 . 4))) which as discussed above will not be filtered  since there is only one queen in each position.  Thus this is the output for (queen-cols 1) with a board size of 4.  Now what happens in the subsequent call passed up for k = 2?
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row 2 rest-of-queens))
        (queen-cols 1)))
 (enumerate-interval 1 4))
;so for new-row = 1 it will add to each of the 4 positions a queen in row 1 and column 2 and so forth for row 2, 3, and 4 again yielding 16 new positions to evaluate from the 4 initial ones.  But unlike in the original case, (queen-cols 1) is passed to the inner map from the flatmap so it will be called 4 times.  For every subsequent call, queen-cols will always be called 4 times in the flatmap instead of just once in the original design.  So the time to calculate (queen-cols N) will be N times to time to evaluate (queen-cols (- N 1)).  Extending the chain down the total time should be N^N times the time to evaluate (queen-cols 1) for which the case would take the same time as the original solution.    So for (queen-cols 1) it evaluates and filters 4 positions.  Then for (queen-cols 2) it has to evaluate and filter 16 positions 4 times and yield 64 positions.  

;Compared to the first solution, each new column evaluation will need to check N times filtered safe positions in both cases.  But in the second solution both the position creation and filtering will need to be done N times.  If for solution 1 the time to get a solution for a board of size N is t1+t2+...+tN where each t is the time to form and filter positions up to the column with that number, then we can compare the time for solution 2 to this.  If we look at the top level for the second solution then we need to evaluate (queen-cols (- board-size 1)) N times.  If the other method for queen-cols is used instead of this new method then the time would already be N times longer.  But this effect continues all the way down the chain until k = 0.  So the time is a multiple of the time for the orignal solution N^N times longer.

;For N = 8 this is 16,777,216 times longer so about 16 million times longer.

;2.2.4 Example: A Picture Language

;Exercise 2.44: Define the procedure up-split used by corner-split. It is similar to right-split, except that it switches the roles of below and beside.
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))

;Exercise 2.45: Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))
;produces procedures right-split and up-split with the same behaviors as the ones already defined.

(define (split op1 op2)
	(lambda (painter) 
		(if (= n 0)
			painter
			(let ((smaller (split painter
									(- n 1))))
			(op1 painter
				(op2 smaller smaller))))))

;Exercise 2.46: A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:
;(x1,y1)+(x2,y2)(x1,y1)−(x2,y2)s⋅(x,y)===(x1+x2,y1+y2),(x1−x2,y1−y2),(sx,sy).

(define (make-vect x y)
	(cons x y))
(define (xcor-vect v)
	(car v))
(define (ycor-vect v)
	(cdr v))
(define (add-vect v1 v2)
	(make-vect 
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
	(make-vect 
		(- (xcor-vect v1) (xcor-vect v2))
		(- (ycor-vect v1) (ycor-vect v2))))	
(define (scale-vect s v)
	(make-vect
		(* s (xcor-vect v)
		(* s (ycor-vect v)))))

;Exercise 2.47: Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
;For each constructor supply the appropriate selectors to produce an implementation for frames.

;for the first constructor
(define (origin-frame frame)
	(car frame))
(define (edge1-frame frame)
	(car (cdr frame)))
(define (edge2-frame frame)
	(cdr (cdr frame)))

;for the second constructor
(define (origin-frame frame)
	(car frame))
(define (edge1-frame frame)
	(car (cdr frame)))
(define (edge2-frame frame)
	(cdr (cdr frame)))
;so both solutions are the same

;Exercise 2.48: A directed line segment in the plane can be represented as a pair of vectors—the vector running from the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from Exercise 2.46 to define a representation for segments with a constructor make-segment and selectors start-segment and end-segment.
(define (make-segment v1 v2)
	(cons v1 v2))
;where v1 and v2 are vectors
(define (start-segment segment)
	(car segment))
(define (end-segment segment)
	(cdr segment))

;Exercise 2.49: Use segments->painter to define the following primitive painters:
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))
;The painter that draws the outline of the designated frame.
(segments->painter (list 	(make-segment (make-vect 0 0) (make-vect 0 1))
							(make-segment (make-vect 0 1) (make-vect 1 1))
							(make-segment (make-vect 1 1) (make-vect 1 0))
							(make-segment (make-vect 1 0) (make-vect 0 0))))
;The painter that draws an “X” by connecting opposite corners of the frame.
(segments->painter (list 	(make-segment (make-vect 0 0) (make-vect 1 1))
							(make-segment (make-vect 0 1) (make-vect 1 0))))
;The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(segments->painter (list 	(make-segment (make-vect 0.5 0) (make-vect 1 0.5))
							(make-segment (make-vect 1 0.5) (make-vect 0.5 1))
							(make-segment (make-vect 0.5 1) (make-vect 0 0.5))
							(make-segment (make-vect 0 0.5) (make-vect 0.5 0))))
;The wave painter.
;yeah...right this would be a lot of segments to make it smooth, impractical to list

;Exercise 2.50: Define the transformation flip-horiz, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;Exercise 2.51: Define the below operation for painters. Below takes two painters as arguments. The resulting painter, given a frame, draws with the first painter in the bottom of the frame and with the second painter in the top. Define below in two different ways—first by writing a procedure that is analogous to the beside procedure given above, and again in terms of beside and suitable rotation operations (from Exercise 2.50).

;this is the beside procedure
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;version 1
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-above  (transform-painter 
                        painter2
                        split-point
                        (make-vect 1.0 0.5)
                        (make-vect 0.0 1.0)))
          (paint-below (transform-painter
                        painter1
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        split-point)))
      (lambda (frame)
        (paint-above frame)
        (paint-below frame)))))

;version 2
(define (below painter1 painter2)
	(rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;Exercise 2.52: Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:

;Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).
;Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).

;original
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))
;using only one copy of up-split and right-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter up)
                  (below right 
                         corner))))))
;Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)

;original
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

;modified so face looks outwards
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside quarter
                        (flip-horiz quarter))))
      (below (flip-vert half) half))))