



;;Exercise 1.3.  
;;Define a procedure that takes three numbers as arguments and returns the sum of the 
;;squares of the two larger numbers. 

(define (square x ) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-2-larg-sqared a b c)
	(cond
	[(and (<= a b) (<= a c)) (sum-of-squares b c)] 
	[(and (<= b a) (<= b c)) (sum-of-squares a c)] 
	[(else (sum-of-squares a b))]))                       



  
;;Exercise 1.4. 
;;Observe that our model of evaluation allows for combinations whose operators are 
;;compound expressions. Use this observation to describe the behavior of the following procedure: 
(define (a-plus-abs-b a b) 
  ((if (> b 0) + -) a b)) 

;; if b is positive adds it to a, otherwise substract it (a plus absolute b)





;;Exercise 1.5.  
;;Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is 
;;using applicative-order evaluation or normal-order evaluation. He defines the following two procedures: 
;;(define (p) (p)) 
;;(define (test x y) 
;;  (if (= x 0) 
;;      0
;;      y)) 
;;Then he evaluates the expression 
;;(test 0 (p)) 
;;What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior 
;;will he observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that 
;;the evaluation rule for the special form if is the same whether the interpreter is using normal or 
;;applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate 
;;the consequent or the alternative expression.) 

;; applicative order loops on itself as the (p) operand never stops beeing re-evaluated





;;Exercise 1.6.  
;;Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I 
;;just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims 
;;this can indeed be done, and she defines a new version of if: 
;;(define (new-if predicate then-clause else-clause) 
;;  (cond (predicate then-clause) 
;;        (else else-clause))) 
;;Eva demonstrates the program for Alyssa: 
;;(new-if (= 2 3) 0 5) 
;;5 
;;(new-if (= 1 1) 0 5) 
;;0 
;;Delighted, Alyssa uses new-if to rewrite the square-root program: 
;;(define (sqrt-iter guess x) 
;;  (new-if (good-enough? guess x) 
;;          guess 
;;          (sqrt-iter (improve guess x) 
;;                     x)))
;;What happens when Alyssa attempts to use this to compute square roots? Explain. 


;; infinite loops! the interpreter uses applicative-order evaluation, which causes the function to recur
;; on itself even when the predicate evaluates to false. 







;;Exercise 1.7.  
;;The good-enough? test used in computing square roots will not be very effective for 
;;finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost 
;;always performed with limited precision. This makes our test inadequate for very large numbers. Explain 
;;these statements, with examples showing how the test fails for small and large numbers. An alternative 
;;strategy for implementing good-enough? is to watch how guess changes from one iteration to the 
;;next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that 
;;uses this kind of end test. Does this work better for small and large numbers? 

(define (square x) (* x x))

(define (improve guess x) 
  (average guess (/ x guess))) 


(define (average x y) 
  (/ (+ x y) 2)) 

;; original version

(define (orig-sqrt-iter guess x) 
  (if (orig-good-enough? guess x) 
      guess 
      (orig-sqrt-iter (improve guess x) 
                 x))) 

(define (orig-good-enough? guess x) 
  (< (abs (- (square guess) x)) .001)) 

(define (orig-sqrt x) 
  (orig-sqrt-iter 1.0 x)) 

;; alternate version 

(define (my-sqrt-iter guess x) 
  (if (my-good-enough? guess x) 
      guess 
      (my-sqrt-iter (improve guess x) 
                 x))) 

(define (my-good-enough? guess x) 
  (< (abs (- (square guess) x)) (abs (* x .0000000001)))) 

(define (my-sqrt x) 
  (my-sqrt-iter 1.0 x)) 


;; assigned version


(define (sqrt-iter guess old-guess x) 
  (if (good-enough? guess old-guess x) 
      guess 
      (sqrt-iter (improve guess x)
                 guess x))) 

(define (improve guess x) 
  (average guess (/ x guess))) 


(define (average x y) 
  (/ (+ x y) 2)) 

(define (good-enough? guess old-guess x) 
  (< (abs (- guess old-guess)) (abs (* guess 0.0000000001)))) 


(define (sqrt x) 
  (sqrt-iter 1.0 x x)) 




;;Exercise 1.8.  
;;Newton's method for cube roots is based on the fact that if y is an approximation to the cube 
;;root of x, then a better approximation is given by the value 
;;
;;(/ (+ (/ x (sq y)) (* y 2)) 3)
;;
;;Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In 
;;section 1.3.4 we will see how to implement Newton's method in general as an abstraction of these square- 
;;root and cube-root procedures.) 

(define (sq x) (* x x))

(define (cube x) (* x x x))

(define (cube-iter guess x) 
  (if (cube-good-enough? guess x) 
      guess 
      (cube-iter (cube-improve guess x) 
                 x))) 

(define (cube-improve guess x)
  (/ (+ (/ x (sq guess)) (* guess 2)) 3))
  
(define (cube-good-enough? guess x) 
  (< (abs (- (cube guess) x)) (abs (* x .0000000001)))) 

(define (cubert x) 
  (cube-iter 1.0 x)) 


;;Exercise 1.9.  Each of the following two procedures defines a method for adding two positive integers in terms 
;;of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))



;;Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5).
;; Are these processes iterative or recursive?

;; first one is recursive, the second is iterative




;;Exercise 1.10.  The following procedure computes a mathematical function called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;What are the values of the following expressions?

(A 1 10)

(A 2 4)

(A 3 3)

;;Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))

;; 2n

(define (g n) (A 1 n))

;; 2**n

(define (h n) (A 2 n))

;; (A 2 0)
 ((= 0 0) 0)
;; (A 2 1)
  2

;; (A 2 2)
   (A 1 2)
     4
  
;; (A 2 3)
   (A 1 4)
    16

;; (A 2 5)
    (A 1 16)
     65536
;; (A 2 6)
    (A 1 65536)


(define (k n) (* 5 n n))

;;Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive integer values 
;;of n. For example, (k n) computes 5n2. 
