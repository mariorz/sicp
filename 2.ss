
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



;;Exercise 2.1.  Define a better version of make-rat that handles both positive 
;;and negative arguments. Make-rat should normalize the sign so that if the rational 
;;umber is positive, both the numerator and denominator are positive, and if the 
;;rational number is negative, only the numerator is negative. 


(define (reduce-frac n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (inv-sign x) (* x -1))

(define (make-rat n d)
  (if (< d 0) 
      (let ((n (inv-sign n)) (d (inv-sign d))) (reduce-frac n d))
      (reduce-frac n d)))
   


;;Exercise 2.2.  Consider the problem of representing line segments in a plane. 
;;Each segment is represented as a pair of points: a starting point and an ending point. 
;;Define a constructor make-segment and selectors start-segment and end-segment that 
;;define the representation of segments in terms of points. Furthermore, a point can be 
;;represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, 
;;specify a constructor make-point and selectors x-point and y-point that define this
;;representation. Finally, using your selectors and constructors, define a procedure 
;;midpoint-segment that takes a line segment as argument and returns its midpoint 
;;(the point whose coordinates are the average of the coordinates of the endpoints). 
;;To try your procedures, you'll need a way to print points:

 
     


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


;;Exercise 2.3.  Implement a representation for rectangles in a plane. (Hint: You may 
;;want to make use of exercise 2.2.) In terms of your constructors and selectors, create 
;;procedures that compute the perimeter and the area of a given rectangle. Now implement 
;;a different representation for rectangles. Can you design your system with suitable 
;;abstraction barriers, so that the same perimeter and area procedures will work using 
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
  


;;Exercise 2.4.  Here is an alternative procedural representation of pairs. For this 
;;representation, verify that (car (cons x y)) yields x for any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;;What is the corresponding definition of cdr? (Hint: To verify that this works, make 
;;use of the substitution model of section 1.1.5.) 


(define (cdr z)
  (z (lambda (p q) q)))


;;Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only 
;;numbers and arithmetic operations if we represent the pair a and b as the integer that 
;;is the product (2**a)*(3**b) Give the corresponding definitions of the procedures cons, 
;;car, and cdr. 


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


    
;;Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling enough, 
;;consider that, in a language that can manipulate procedures, we can get by without numbers 
;;(at least insofar as nonnegative integers are concerned) by implementing 0 and the operation 
;;of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


;;This representation is known as Church numerals, after its inventor, Alonzo Church, the 
;;logician who invented the calculus.

;;Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to 
;;evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms
;;of repeated application of add-1). 


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





;;Exercise 2.7.  Alyssa's program is incomplete because she has not specified the implementation
;;of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

;;Define selectors upper-bound and lower-bound to complete the implementation. 


(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))



;;Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two 
;;intervals may be computed. Define a corresponding subtraction procedure, called sub-interval. 

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))



;;Exercise 2.9.  The width of an interval is half of the difference between its upper and lower 
;;bounds. The width is a measure of the uncertainty of the number specified by the interval. 
;;For some arithmetic operations the width of the result of combining two intervals is a function 
;;only of the widths of the argument intervals, whereas for others the width of the combination is 
;;not a function of the widths of the argument intervals. Show that the width of the sum 
;;(or difference) of two intervals is a function only of the widths of the intervals being added 
;;(or subtracted). Give examples to show that this is not true for multiplication or division. 




(define (interval-width z)
  (/ (- (upper-bound z) (lower-bound z)) 2))

;;x - y = z
;;a - b = r
;;(x + a) - (y + b) = z + r
;;(x + a) - (y + b) = (x - y) + (a - b)
;;(x + a) - (y + b) = x - y + a - b
;;(x + a) - (y + b) = (x + a) - y - b
;;(x + a) - (y + b) = (x + a) - (y + b)




;;Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and 
;;comments that it is not clear what it means to divide by an interval that spans zero. 
;;Modify Alyssa's code to check for this condition and to signal an error if it occurs. 




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



;;Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing the signs of the endpoints 
;;of the intervals, it is possible to break mul-interval into nine cases, only one of which requires 
;;more than two multiplications.'' Rewrite this procedure using Ben's suggestion.




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
    (cond ((positive? lx) (cond ((positive? ly) (make-interval (* lx ly) (* ux uy)))
                                ((negative? uy) (make-interval (* ux ly) (* lx uy)))
                                (else           (make-interval (* ux ly) (* ux uy)))))

          ((negative? ux) (cond ((positive? ly) (make-interval (* lx uy) (* ux ly)))
                                ((negative? uy) (make-interval (* ux uy) (* lx ly)))
                                (else           (make-interval (* lx uy) (* lx ly)))))

          (else           (cond ((positive? ly) (make-interval (* lx uy) (* ux uy)))
                                ((negative? uy) (make-interval (* ux ly) (* lx ly)))
                                (else
                                   (make-interval (min (* lx uy) (* ux ly))
                                                  (max (* lx ly) (* ux uy)))))))))







;;After debugging her program, Alyssa shows it to a potential user, who complains that her program 
;;solves the wrong problem. He wants a program that can deal with numbers represented as a center 
;;value and an additive tolerance; for example, he wants to work with intervals such as 3.5± 0.15 rather 
;;than [3.35, 3.65]. Alyssa returns to her desk and fixes this problem by supplying an alternate 
;;constructor and alternate selectors:


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;;Unfortunately, most of Alyssa's users are engineers. Real engineering situations usually involve 
;;measurements with only a small uncertainty, measured as the ratio of the width of the interval to the 
;;midpoint of the interval. Engineers usually specify percentage tolerances on the parameters of devices, 
;;as in the resistor specifications given earlier.

;;Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage 
;;tolerance and produces the desired interval. You must also define a selector percent that 
;;produces the percentage tolerance for a given interval. The center selector is the same as the 
;;one shown above.


(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

(define (interval-center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (/ (interval-width i) (interval-center i)) 100.0))




;;Exercise 2.13.  Show that under the assumption of small percentage tolerances there is a simple 
;;formula for the approximate percentage tolerance of the product of two intervals in terms of the 
;;tolerances of the factors. You may simplify the problem by assuming that all numbers are positive. 



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

	  

;;After considerable work, Alyssa P. Hacker delivers her finished system. Several years later, after 
;;she has forgotten all about it, she gets a frenzied call from an irate user, Lem E. Tweakit. It seems 
;;that Lem has noticed that the formula for parallel resistors can be written in two algebraically 
;;equivalent ways:

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

;;Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a variety 
;;of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions 
;;A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of 
;;the center value. Examine the results of the computation in center-percent form (see exercise 2.12).

(define A (make-center-percent 10 5))
(define B (make-center-percent 15 2))
(par1 foo bar)
(par2 foo bar)
(div-interval A A)
(div-interval B B)

;; we can see here where the error is. Algebraically (A / A), would be 1, however, since we're dealing
;; with intervals, A could be any number between 10 and 10.5, so it doesn't compute to 1


;;Exercise 2.15.  Eva Lu Ator, another user, has also noticed the different intervals computed by 
;;different but algebraically equivalent expressions. She says that a formula to compute with intervals 
;;using Alyssa's system will produce tighter error bounds if it can be written in such a form that no 
;;variable that represents an uncertain number is repeated. Thus, she says, par2 is a ``better'' program 
;;for parallel resistances than par1. Is she right? Why?


;; Eva is right, any operation dealing with intervals will increase the error tolerance.
;; In this case, par3 uses the r1 and r2 forumlas just once. (the interval one is used 3
;; times but since it's interval-width is 0, it does not affect the result.)


;;Exercise 2.16.  Explain, in general, why equivalent algebraic expressions may lead to different 
;;answers. Can you devise an interval-arithmetic package that does not have this shortcoming, or is this 
;;task impossible? (Warning: This problem is very difficult.) 


