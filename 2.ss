
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



(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define one-half (make-rat 1 2))

(print-rat one-half)


(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))



;;Exercise 2.1.  Define a better version of make-rat that handles both 
;;positive and negative arguments. Make-rat should normalize the sign so 
;;that if the rational number is positive, both the numerator and denominator 
;;are positive, and if the rational number is negative, only the numerator is 
;;negative. 


(define (reduce-frac n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (inv-sign x) (* x -1))

(define (make-rat n d)
  (if (< d 0) 
      (let ((n (inv-sign n)) (d (inv-sign d))) (reduce-frac n d))
      (reduce-frac n d)))
   


;;Exercise 2.2.  Consider the problem of representing line segments in a 
;;plane. Each segment is represented as a pair of points: a starting point 
;;and an ending point. Define a constructor make-segment and selectors 
;;start-segment and end-segment that define the representation of segments in 
;;terms of points. Furthermore, a point can be represented as a pair of 
;;numbers: the x coordinate and the y coordinate. Accordingly, specify a 
;;constructor make-point and selectors x-point and y-point that define this 
;;representation. Finally, using your selectors and constructors, define a 
;;procedure midpoint-segment that takes a line segment as argument and returns 
;;its midpoint (the point whose coordinates are the average of the coordinates 
;;of the endpoints). To try your procedures, you'll need a way to print points:


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment l) (car l))

(define (end-segment l) (cdr l))



(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p)) 


(define (midpoint-segment l)
  (make-point 
   (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
   (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))


;;Exercise 2.3.  Implement a representation for rectangles in a plane. 
;;(Hint: You may want to make use of exercise 2.2.) In terms of your 
;;constructors and selectors, create procedures that compute the perimeter 
;;and the area of a given rectangle. Now implement a different representation 
;;for rectangles. Can you design your system with suitable abstraction 
;;barriers, so that the same perimeter and area procedures will work using 
;;either representation? 


;;A)
(define (rect-area r)
  (* (rect-height r) (rect-width r)))

(define (rect-perimeter r)
  (+ (* (rect-height r) 2) (* (rect-width r) 2)))


(define (make-rect topleft-point width height)
  (cons topleft-point (cons width height)))


(define (rect-height r)
  (cdr (cdr r)))

(define (rect-width r)
  (car (cdr r)))


;;B) re-check this!
(define (length-segment l) 
  (sqrt 
   (+ 
    (square 
     (- (x-point (end-segment l)) (x-point (start-segment l))))
    (square
     (- (y-point (end-segment l)) (y-point (start-segment l)))))))
    


(define (make-rect l1 l2)
  (cons l1 l2))

(define (rect-height r)
  (length-segment (car r)))

(define (rect-width r)
  (length-segment (cdr r)))
  


;;Exercise 2.4.  Here is an alternative procedural representation of pairs. 
;;For this representation, verify that (car (cons x y)) yields x for any 
;;objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;;What is the corresponding definition of cdr? (Hint: To verify that this 
;;works, make use of the substitution model of section 1.1.5.) 


(define (cdr z)
  (z (lambda (p q) q)))


;;Exercise 2.5.  Show that we can represent pairs of nonnegative integers 
;;using only numbers and arithmetic operations if we represent the pair a and 
;;b as the integer that is the product (2**a)*(3**b) Give the corresponding 
;;definitions of the procedures cons, car, and cdr. 


(define (pow a b)
  (define (iter-pow a b res)
    (if (> b 0) (iter-pow a (- b 1) (* a res)) res))
  (iter-pow a b 1))


(define (mycons a b)(* (pow 2 a) (pow 3 b)))

(define (factred n test x y)
  (define (iter n a)
    (if (> n 1)
	(if (test n)
	    (iter (/ n x) (+ a 1))
	    (iter (/ n y) a))
	a))
  (iter n 0))
	    

(define (mycar z)
  (factred z even? 2 3))

(define (mycdr z)
  (factred z odd? 3 2))


    
;;Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling 
;;enough, consider that, in a language that can manipulate procedures, we can 
;;get by without numbers (at least insofar as nonnegative integers are 
;;concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


;;This representation is known as Church numerals, after its inventor, 
;;Alonzo Church, the logician who invented the calculus.

;;Define one and two directly (not in terms of zero and add-1). 
;;(Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition 
;;of the addition procedure + (not in terms of repeated application of add-1). 


(define one (lambda (f) (lambda (x) (f x))))
 
(define two (lambda (f) (lambda (x) (f (f x)))))
 
(define (+ m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))





(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))





;;Exercise 2.7.  Alyssa's program is incomplete because she has not 
;;specified the implementation of the interval abstraction. Here is a 
;;definition of the interval constructor:

(define (make-interval a b) (cons a b))

;;Define selectors upper-bound and lower-bound to complete the implementation. 


(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))



;;Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the 
;;difference of two intervals may be computed. Define a corresponding 
;;subtraction procedure, called sub-interval. 

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))



;;Exercise 2.9.  The width of an interval is half of the difference between 
;;its upper and lower bounds. The width is a measure of the uncertainty of 
;;the number specified by the interval. For some arithmetic operations the 
;;width of the result of combining two intervals is a function only of the 
;;widths of the argument intervals, whereas for others the width of the 
;;combination is not a function of the widths of the argument intervals. 
;;Show that the width of the sum (or difference) of two intervals is a 
;;function only of the widths of the intervals being added (or subtracted). 
;;Give examples to show that this is not true for multiplication or division. 




(define (interval-width z)
  (/ (- (upper-bound z) (lower-bound z)) 2))

;;x - y = z
;;a - b = r
;;(x + a) - (y + b) = z + r
;;(x + a) - (y + b) = (x - y) + (a - b)
;;(x + a) - (y + b) = x - y + a - b
;;(x + a) - (y + b) = (x + a) - y - b
;;(x + a) - (y + b) = (x + a) - (y + b)




;;Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over 
;;Alyssa's shoulder and comments that it is not clear what it means to 
;;divide by an interval that spans zero. Modify Alyssa's code to check for 
;;this condition and to signal an error if it occurs. 




(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(define (div-interval x y)
  (if (or (= (lower-bound y) 0)
          (= (upper-bound y) 0)
          (and (< (lower-bound y) 0) (> (upper-bound y) 0)))
      (error "division by zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y)))))



;;Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing 
;;the signs of the endpoints of the intervals, it is possible to break 
;;mul-interval into nine cases, only one of which requires more than two 
;;multiplications. Rewrite this procedure using Ben's suggestion.




(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))




(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((positive? lx) 
	   (cond ((positive? ly) 
		  (make-interval (* lx ly) (* ux uy)))
		 ((negative? uy) 
		  (make-interval (* ux ly) (* lx uy)))
		 (else           
		  (make-interval (* ux ly) (* ux uy)))))

	  ((negative? ux) 
	   (cond ((positive? ly) 
		  (make-interval (* lx uy) (* ux ly)))
		 ((negative? uy) 
		  (make-interval (* ux uy) (* lx ly)))
		 (else           
		  (make-interval (* lx uy) (* lx ly)))))

	  (else          
	   (cond ((positive? ly) 
		  (make-interval (* lx uy) (* ux uy)))
		 ((negative? uy) 
		  (make-interval (* ux ly) (* lx ly)))
		 (else
		  (make-interval (min (* lx uy) (* ux ly))
				 (max (* lx ly) (* ux uy)))))))))







;;After debugging her program, Alyssa shows it to a potential user, who 
;;complains that her program solves the wrong problem. He wants a program 
;;that can deal with numbers represented as a center value and an additive 
;;tolerance; for example, he wants to work with intervals such as 3.5± 0.15 
;;rather than [3.35, 3.65]. Alyssa returns to her desk and fixes this 
;;problem by supplying an alternate constructor and alternate selectors:


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;;Unfortunately, most of Alyssa's users are engineers. Real engineering 
;;situations usually involve measurements with only a small uncertainty, 
;;measured as the ratio of the width of the interval to the midpoint of the 
;;interval. Engineers usually specify percentage tolerances on the parameters 
;;of devices, as in the resistor specifications given earlier.

;;Exercise 2.12.  Define a constructor make-center-percent that takes a center 
;;and a percentage tolerance and produces the desired interval. You must also 
;;define a selector percent that produces the percentage tolerance for a given 
;;interval. The center selector is the same as the 
;;one shown above.


(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

(define (interval-center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (/ (interval-width i) (interval-center i)) 100.0))




;;Exercise 2.13.  Show that under the assumption of small percentage 
;;tolerances there is a simple formula for the approximate percentage 
;;tolerance of the product of two intervals in terms of the tolerances of the 
;;factors. You may simplify the problem by assuming that all numbers are 
;;positive. 



(define (same-tol? i1 i2)
  (= (percent (mul-interval i1 i2)) (+ (percent i1) (percent i2))))


(define (find-min-tols)
  (define (iter i1 i2)
    (let ((p1 (percent i1))
	  (p2 (percent i2)))
      (if (same-tol? i1 i2)
	  (and (display p1) (display " <- p1 & p2 -> ") (display p2))
	  (iter 
	   (make-center-percent (center i1) (- p1 1))
	   (make-center-percent (center i2) (- p2 1))))))
  (iter (make-center-percent 100 100) (make-center-percent 100 100)))

	  

;;After considerable work, Alyssa P. Hacker delivers her finished system. 
;;Several years later, after she has forgotten all about it, she gets a 
;;frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has 
;;noticed that the formula for parallel resistors can be written in two 
;;algebraically equivalent ways:

;; (R1 * R2) / (R1 + R2)
;; and
;; 1 / ((1 / R1) + (1 / R2))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of 
;;the system on a variety of arithmetic expressions. Make some intervals A and 
;;B, and use them in computing the expressions A/A and A/B. You will get the 
;;most insight by using intervals whose width is a small percentage of the 
;;center value. Examine the results of the computation in center-percent form 
;;(see exercise 2.12).

(define A (make-center-percent 10 5))
(define B (make-center-percent 15 2))
(par1 foo bar)
(par2 foo bar)
(div-interval A A)
(div-interval B B)

;; we can see here where the error is. Algebraically (A / A), would be 1, 
;;however, since we're dealing with intervals, A could be any number between 
;;10 and 10.5, so it doesn't compute to 1


;;Exercise 2.15.  Eva Lu Ator, another user, has also noticed the different 
;;intervals computed by different but algebraically equivalent expressions. 
;;She says that a formula to compute with intervals using Alyssa's system will 
;;produce tighter error bounds if it can be written in such a form that no 
;;variable that represents an uncertain number is repeated. Thus, she says, 
;;par2 is a ``better'' program for parallel resistances than par1. Is she 
;;right? Why?


;; Eva is right, any operation dealing with intervals will increase the error 
;;tolerance.
;;In this case, par3 uses the r1 and r2 forumlas just once. (the interval one 
;;is used 3 times but since it's interval-width is 0, it does not affect the 
;;result.)


;;Exercise 2.16.  Explain, in general, why equivalent algebraic expressions 
;;may lead to different answers. Can you devise an interval-arithmetic package 
;;that does not have this shortcoming, or is this task impossible? (Warning: 
;;This problem is very difficult.) 


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;; this gives an error when trying to car a non list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;Exercise 2.17.  Define a procedure last-pair that returns the list that 
;;contains only the last element of a given (nonempty) list:


(define (last-pair l)
  (define (iter a item)
    (if (null? a)
	item
	(iter (cdr a) (car a))))
  (iter l (car l)))


;;Exercise 2.18.  Define a procedure reverse that takes a list as argument 
;;and returns a list of the same elements in reverse order:



(define (but-last l)
  (define (iter a b)
    (if (null? (cdr a))
	b
	(iter (cdr a) (append b (list (car a))))))
  (iter l (list)))
      


(define (reverse l)
  (define (iter a b)
    (if (null? a)
	b
	(iter (but-last a) (append b (list (last-pair a))))))
  (iter l (list)))
    




(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))



;;Exercise 2.19.  Consider the change-counting program of section 1.2.2. 
;;It would be nice to be able to easily change the currency used by the 
;;program, so that we could compute the number of ways to change a British 
;;pound, for example. As the program is written, the knowledge of the currency 
;;is distributed partly into the procedure first-denomination and partly into 
;;the procedure count-change (which knows that there are five kinds of U.S. 
;;coins). It would be nicer to be able to supply a list of coins to be used 
;;for making change.

;;We want to rewrite the procedure cc so that its second argument is a list 
;;of the values of the coins to use rather than an integer specifying which 
;;coins to use. We could then have lists that defined each kind of currency:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (except-first-denomination coins)
    (cdr coins))
  (define (first-denomination coins)
    (car coins))
  (define (no-more? coins)
    (= (length coins) 0))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;We could then call cc as follows:

(cc 100 us-coins)
292

;;To do this will require changing the program cc somewhat. It will still 
;;have the same form, but it will access its second argument differently, as 
;;follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;Define the procedures first-denomination, except-first-denomination, and 
;;no-more? in terms of primitive operations on list structures. Does the order 
;;of the list coin-values affect the answer produced by cc? Why or why not?



;;Exercise 2.20.  The procedures +, *, and list take arbitrary numbers of 
;;arguments. One way to define such procedures is to use define with 
;;dotted-tail notation. In a procedure definition, a parameter list that has 
;;a dot before the last parameter name indicates that, when the procedure is 
;;called, the initial parameters (if any) will have as values the initial 
;;arguments, as usual, but the final parameter's value will be a list of any 
;;remaining arguments. For instance, given the definition

(define (f x y . z) <body>)

;;the procedure f can be called with two or more arguments. If we evaluate

;;(f 1 2 3 4 5 6)

;;then in the body of f, x will be 1, y will be 2, and z will be the list 
;;(3 4 5 6). Given the definition

(define (g . w) <body>)

;;the procedure g can be called with zero or more arguments. If we evaluate

(g 1 2 3 4 5 6)

;;then in the body of g, w will be the list (1 2 3 4 5 6).11

;;Use this notation to write a procedure same-parity that takes one or more 
;;integers and returns a list of all the arguments that have the same even-odd 
;;parity as the first argument. For example,

(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)

(same-parity 2 3 4 5 6 7)
(2 4 6)


(define (same-par . nums)
  (if (even? (car nums))
      (par-list nums even?)
      (par-list nums odd?)))

(define (par-list elements type)
  (define (iter elems res)
    (if (null? elems)
	res
	(if (type (car elems))
	    (iter (cdr elems) (cons (car elems) res))
	    (iter (cdr elems) res))))
  (iter (reverse elements) null))

(define same-parity same-par)


;;Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a list 
;;of the squares of those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)

;;Here are two different definitions of square-list. Complete both of them by filling in the 
;;missing expressions:

(define (square-list items)
  (if (null? items)
      null
      (cons (sq (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map sq items))


;;Exercise 2.23.  The procedure for-each is similar to map. It takes as arguments a procedure and a 
;;list of elements. However, rather than forming a list of the results, for-each just applies the 
;;procedure to each of the elements in turn, from left to right. The values returned by applying 
;;the procedure to the elements are not used at all -- for-each is used with procedures that perform 
;;an action, such as printing. For example,

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
57
321
88

;;The value returned by the call to for-each (not illustrated above) can be something arbitrary, 
;;such as true. Give an implementation of for-each. 

(define (for-each f items)
  (cond ((null? items) (newline))
        (else
         (f (car items))
         (for-each f (cdr items)))))


;;Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse 
;;procedure that takes a list as argument and returns as its value the list with its elements 
;;reversed and with  all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))

(define (deep-reverse x)
  (define (go in out)
    (if (null? in)
        out
        (go (cdr in) (cons (deep-reverse (car in)) out))))
  (if (pair? x)
      (go x null)
      x))


;;Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list) 
;;and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. 
;;For example,

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)


 
(define (fringe tree) 
  (cond ((null? tree) '()) 
         ((not (pair? tree)) (list tree)) 
         (else (append (fringe (car tree)) (fringe (cdr tree)))))) 


;;Exercise 2.30.  Define a procedure square-tree analogous to the square-list procedure of 
;;exercise 2.21. That is, square-list should behave as follows:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
(1 (4 (9 16) 25) (36 49))


(define (square-tree tree) 
  (map (lambda (x) 
	 (cond ((null? x) '()) 
	       ((not (pair? x)) (square x)) 
	       (else (square-tree x)))) 
       tree)) 

;;Exercise 2.31.  Abstract your answer to exercise 2.30 to produce a procedure tree-map 
;;with the property that square-tree could be defined as

(define (square-tree tree) (tree-map square tree))


(define (tree-map proc tree) 
  (map (lambda (subtree) 
	 (cond ((null? subtree) '()) 
	       ((not (pair? subtree)) (proc subtree)) 
	       (else (tree-map proc subtree)))) 
       tree)) 
  
;;Exercise 2.32.  We can represent a set as a list of distinct elements, and we can represent 
;;the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then 
;;the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following 
;;definition of a procedure that generates the set of subsets of a set and give a clear explanation 
;;of why it works:

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))



;;Exercise 2.33.  Fill in the missing expressions to complete the following definitions of some 
;;basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))


(define (length sequence)
  (accumulate (lambda (first already-acc) (+ 1 already-acc)) 0 sequence))


;;Exercise 2.34.  Evaluating a polynomial in x at a given value of x can be formulated as an 
;;accumulation. We evaluate the polynomial...

;... using a well-known algorithm called Horner's rule, which structures the computation as

;;In other words, we start with an, multiply by x, add an-1, multiply by x, and so on, until we reach 
;;a0.16 Fill in the following template to produce a procedure that evaluates a polynomial using 
;;Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence, from 
;;a0 through an.

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
	  (accumulate op initial (cdr sequence))))) 
  



(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would evaluate

(horner-eval 2 (list 1 3 0 5 0 1))


;;Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

(define (enumerate-tree tree) 
  (cond ((null? tree) nil) 
	((not (pair? tree)) (list tree)) 
	(else (append (enumerate-tree (car tree)) 
		      (enumerate-tree (cdr tree)))))) 



(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))


;;Exercise 2.36.  The procedure accumulate-n is similar to accumulate except that it takes as 
;;its third argument a sequence of sequences, which are all assumed to have the same number of 
;;elements. It applies the designated accumulation procedure to combine all the first elements of 
;;the sequences, all the second elements of the sequences, and so on, and returns a sequence of 
;;the results. For instance, if s is a sequence containing four sequences, 
;;((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the 
;;sequence (22 26 30). Fill in the missing expressions in the following definition of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car sequence))
            (accumulate-n op init (map cdr sequence)))))


;;2.37

 
(define (dot-product v1 v2) 
  (accumulate + 0 (map * v1 v2))) 
  
    
(define (matrix-*-vector m v) 
  (map (lambda (m-row) (dot-product m-row v)) m)) 
  

(define (transpose m) 
  (accumulate-n cons '() m)) 
  

(define (matrix-*-matrix m n) 
  (let ((n-cols (transpose n))) 
    (map (lambda (m-row) (matrix-*-vector n-cols m-row)) m))) 
  
;;Exercise 2.39.   Complete the following definitions of reverse (exercise 2.18) in terms of 
;;fold-right and fold-left from exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))




;;Exercise 2.41.  Write a procedure to find all ordered triples of distinct positive 
;;integers i, j, and k less than or equal to a given integer n that sum to a given 
;;integer s. 

  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 
  
 
(define (unique-triples n) 
  (flatmap (lambda (i) 
	     (flatmap (lambda (j) 
			(map (lambda (k) (list i j k)) 
			     (enumerate-interval 1 (- j 1)))) 
		      (enumerate-interval 1 (- i 1)))) 
	   (enumerate-interval 1 n))) 
  



;;Exercise 2.54.  Two lists are said to be equal? if they contain equal elements 
;;arranged in the same order. For example,

;;(equal? '(this is a list) '(this is a list))

;;is true, but

;;(equal? '(this is a list) '(this (is a) list))

;;is false. To be more precise, we can define equal? recursively in terms of the 
;;basic eq? equality of symbols by saying that a and b are equal? if they are both 
;;symbols and the symbols are eq?, or if they are both lists such that (car a) is 
;;equal? to (car b) and (cdr a) is equal? to (cdr b). Using this idea, implement 
;;equal? as a procedure.36 



(define (equal? a b) 
   (if (and (pair? a) (pair? b)) 
       (cond ((null? a) (null? b)) 
             ((null? b) #f) 
             ((equal? (car a) (car b)) (equal? (cdr a) (cdr b))) 
             (else #f)) 
       (eq? a b))) 
           

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))



;;The variables are symbols. They are identified by the primitive predicate symbol?:

(define (variable? x) (symbol? x))

;;Two variables are the same if the symbols representing them are eq?:

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;Sums and products are constructed as lists:

(define (make-sum a1 a2) (list '+ a1 a2))



(define (make-product m1 m2) (list '* m1 m2))

;;A sum is a list whose first element is the symbol +:

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;;The addend is the second item of the sum list:

(define (addend s) (cadr s))

;;The augend is the third item of the sum list:

(define (augend s) (caddr s))


  



;;A product is a list whose first element is the symbol *:

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))





;The multiplier is the second item of the product list:

(define (multiplier p) (cadr p))

;;The multiplicand is the third item of the product list:

(define (multiplicand p) (caddr p))





(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;;Exercise 2.56.


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
 
(define (base x) (cadr x))
 
(define (exponent x) (caddr x))



(define (pow a b)
  (define (iter-pow a b res)
    (if (> b 0) (iter-pow a (- b 1) (* a res)) res))
  (iter-pow a b 1))


(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
	((and (number? b) (number? e)) (pow b e))
        (else (list '** b e))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
			 (multiplicand exp))))
	((exponentiation? exp)
	 (let 
	     ((n (exponent exp))
	      (u (base exp)))
	   (make-product
	    (make-product n
			  (make-exponentiation u
					       (make-sum n -1)))
            (deriv u var))))
        (else
         (error "unknown expression type -- DERIV" exp))))



;;Exercise 2.57.  Extend the differentiation program to handle sums and products of 
;;arbitrary numbers of (two or more) terms. Then the last example above could be 
;;expressed as

;;(deriv '(* x y (+ x 3)) 'x)

;;Try to do this by changing only the representation for sums and products, without 
;;changing the deriv procedure at all. For example, the addend of a sum would be the 
;first term, and the augend would be the sum of the rest of the terms. 



(define (augend s) 
  (if (pair? (cdddr s))
      (cons '+ (cddr s))
      (caddr s)))


(define (multiplicand p)
  (if (pair? (cdddr p))
      (cons '* (cddr p))
      (caddr p)))


;;Exercise 2.58.  Suppose we want to modify the differentiation program so that it works with 
;;ordinary mathematical notation, in which + and * are infix rather than prefix operators. 
;;Since the differentiation program is defined in terms of abstract data, we can modify it to 
;;work with different representations of expressions solely by changing the predicates, selectors, 
;;and constructors that define the representation of the algebraic expressions on which the 
;;differentiator is to operate.

;;a. Show how to do this in order to differentiate algebraic expressions presented in infix form, 
;;such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two 
;;arguments and that expressions are fully parenthesized.

;;b. The problem becomes substantially harder if we allow standard algebraic notation, such as 
;;(x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done 
;;before addition. Can you design appropriate predicates, selectors, and constructors for this notation 
;;such that our derivative program still works? 


(define (make-sum a1 a2) (list 'a1 + a2))


(define (make-product m1 m2) (list 'm1 * m2))


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))


(define (addend s) (car s))

(define (augend s) (caddr s))



(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))


(define (multiplier p) (car p))



(define (multiplicand p) (caddr p))




(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list 'a1 + a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list 'm1 * m2))))


;;b?

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;;Exercise 2.59.  Implement the union-set operation for the unordered-list 
;;representation of sets. 




(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))


;;Exercise 2.60.  We specified that a set would be represented as a list with no 
;;duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could 
;;be represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?, 
;;adjoin-set, union-set, and intersection-set that operate on this representation. 
;;How does the efficiency of each compare with the corresponding procedure for the 
;;non-duplicate representation? Are there applications for which you would use this 
;;representation in preference to the non-duplicate one? 


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


;; this is the only one that had to be modified
(define (adjoin-set x set)
  (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))




(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))



;;Exercise 2.61.  Give an implementation of adjoin-set using the ordered representation. 
;;By analogy with element-of-set? show how to take advantage of the ordering to produce a 
;;procedure that requires on the average about half as many steps as with the unordered 
;;representation.

;; for unordered case
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


;;Exercise 2.62.  Give a (n) implementation of union-set for sets represented as 
;;ordered lists. 

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      (else
                       (cons x2 (union-set set1 (cdr set2)))))))))




;; sets as binary trees


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))






;;Exercise 2.63.  Each of the following two procedures converts a binary 
;;tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))





;;Exercise 2.64.  The following procedure list->tree converts an ordered list to a 
;;balanced binary tree. The helper procedure partial-tree takes as arguments an 
;;integer n and list of at least n elements and constructs a balanced tree containing 
;;the first n elements of the list. The result returned by partial-tree is a pair 
;;(formed with cons) whose car is the constructed tree and whose cdr is the list of 
;;elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* 
	  ((left-size (quotient (- n 1) 2))
	   (left-result (partial-tree elts left-size))
	   (left-tree (car left-result))
	   (non-left-elts (cdr left-result))
	   (right-size (- n (+ left-size 1)))
	   (this-entry (car non-left-elts))
	   (right-result (partial-tree (cdr non-left-elts) right-size))
	   (right-tree (car right-result))
	   (remaining-elts (cdr right-result)))
	(cons (make-tree this-entry left-tree right-tree) remaining-elts))))
	   
	     

;;a. Write a short paragraph explaining as clearly as you can how partial-tree works. 
;;Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

;;b. What is the order of growth in the number of steps required by list->tree to convert 
;;a list of n elements? 


;answer

;; it divides the elements of elts into two groups (left and right), such that they have 
;;the same number of elements. It then creates trees from these groups wich are then cons-ed 
;;to the current node. The algorithm works in a recursive manner and it's order of growth is
;;O(n) as it mos transverse the whole list.





;;Exercise 2.65.  Use the results of exercises 2.63 and  2.64 to give O(n) implementations 
;;of union-set and intersection-set for sets implemented as (balanced) binary trees


(define (union-set-bintree tree1 tree2)
  (list->tree (union-set (tree-list1 tree1) (tree-list1 tree2))))


(define (intersection-set-bintree tree1 tree2)
  (list->tree (intersection-set (tree->list1 tree1) (tree->list1 tree2))))


;; tree-list1, union-set, intersection-set and list->tree, are all O(n) for a total 
;;of O(4n) = O(n) on each one.

 


;;Exercise 2.66.  Implement the lookup procedure for the case where the set of records 
;;is structured as a binary tree, ordered by the numerical values of the keys. 

(define (lookup given-key records)
  (let*
      ((record (entry records))
       (cur-key (key record)))
    (cond 
     [(null? records) '()]
     [(= given-key cur-key) record]
     [(< given-key cur-key) 
      (lookup given-key (left-branch records))]
     [else 
      (lookup given-key (right-branch records))])))



;; Huffman encoding trees


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;;If we make a tree in this way, we have the following selectors:

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



;;The following procedure implements the decoding algorithm. It takes as 
;;arguments a list of zeros and ones, together with a Huffman tree.

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


;;The following adjoin-set procedure for constructing sets is similar to the 
;;one described in exercise 2.61; however, items are compared by their weights, 
;;and the element being added to the set is never already in it.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))



;;The following procedure takes a list of symbol-frequency pairs such as 
;;((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of leaves, 
;;ready to be merged according to the Huffman algorithm:

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))



;;Exercise 2.67.  Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;Use the decode procedure to decode the message, and give the result. 

;;(decode sample-message sample-tree)
;;(A D A B B C A)







;;Exercise 2.68.  The encode procedure takes as arguments a message and a tree 
;;and produces the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;Encode-symbol is a procedure, which you must write, that returns the list of bits 
;;that encodes a given symbol according to a given tree. You should design encode-symbol 
;;so that it signals an error if the symbol is not in the tree at all. Test your 
;;procedure by encoding the result you obtained in exercise 2.67 with the sample tree and 
;;seeing whether it is the same as the original sample message. 









;;Exercise 2.69.  The following procedure takes as its argument a list of 
;;symbol-frequency pairs (where no symbol appears in more than one pair) 
;;and generates a Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;Make-leaf-set is the procedure given above that transforms the list of pairs 
;;into an ordered set of leaves. Successive-merge is the procedure you must 
;;write, using make-code-tree to successively merge the smallest-weight elements 
;;of the set until there is only one element left, which is the desired Huffman 
;;tree. (This procedure is slightly tricky, but not really complicated. If you 
;;find yourself designing a complex procedure, then you are almost certainly 
;;doing something wrong. You can take significant advantage of the fact that we 
;;are using an ordered set representation.) 



(define (successive-merge leaves)
  (cond
   [(null? leaves) '()]
   [(null? (cdr leaves)) (car leaves)]
   [else
    (successive-merge (adjoin-set 
		      (make-code-tree (car leaves) (cadr leaves))
		      (cddr leaves)))]))
   
   
    


;;Exercise 2.70.  The following eight-symbol alphabet with associated relative 
;;frequencies was designed to efficiently encode the lyrics of 1950s rock songs. 
;;(Note that the ``symbols'' of an ``alphabet'' need not be individual letters.)

;;A 	2 	NA 	16
;;BOOM 	1 	SHA 	3
;;GET 	2 	YIP 	9
;;JOB 	2 	WAH 	1

;;Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree, 
;;and use encode (exercise 2.68) to encode the following message:

;;Get a job

;;Sha na na na na na na na na

;;Get a job

;;Sha na na na na na na na na

;;Wah yip yip yip yip yip yip yip yip yip

;;Sha boom

;;How many bits are required for the encoding? What is the smallest number of bits that 
;;would be needed to encode this song if we used a fixed-length code for the eight-symbol 
;;alphabet? 



;;Exercise 2.71.  Suppose we have a Huffman tree for an alphabet of n symbols, and that the 
;;relative frequencies of the symbols are 1, 2, 4, ..., 2n-1. Sketch the tree for n=5; for n=10. 
;;In such a tree (for general n) how may bits are required to encode the most frequent symbol? 
;;the least frequent symbol?

;;Exercise 2.72.  Consider the encoding procedure that you designed in exercise 2.68. What is the 
;;order of growth in the number of steps needed to encode a symbol? Be sure to include the number 
;;of steps needed to search the symbol list at each node encountered. To answer this question in 
;;general is difficult. Consider the special case where the relative frequencies of the n symbols 
;;are as described in exercise 2.71, and give the order of growth (as a function of n) of the number 
;;of steps needed to encode the most frequent and least frequent symbols in the alphabet. 
