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



  


;;Exercise 1.4.  Observe that our model of evaluation allows for combinations whose operators are 
;;compound expressions. Use this observation to describe the behavior of the following procedure: 
(define (a-plus-abs-b a b) 
  ((if (> b 0) + -) a b)) 

;; if b is positive adds it to a, otherwise substract it (a plus absolute b)




;;Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is 
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







;;Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I 
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






;;Now let's formalize the process in terms of procedures. We start with a value for the radicand (the number 
;;whose square root we are trying to compute) and a value for the guess. If the guess is good enough for our 
;;purposes, we are done; if not, we must repeat the process with an improved guess. We write this basic 
;;strategy as a procedure: 
(define (sqrt-iter guess x) 
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) 
                 x))) 


(define (sqrt-iter2 guess x) 
  (if (good-enough2? guess x) 
      guess 
      (sqrt-iter2 (improve guess x) 
                 x))) 

;;A guess is improved by averaging it with the quotient of the radicand and the old guess: 
(define (improve guess x) 
  (average guess (/ x guess))) 

;;where 

(define (average x y) 
  (/ (+ x y) 2)) 

;;We also have to say what we mean by ``good enough.'' The following will do for illustration, but it is not 
;;really a very good test. (See exercise 1.7.) The idea is to improve the answer until it is close enough so that 
;;its square differs from the radicand by less than a predetermined tolerance (here 0.001):22 

;; original version
(define (good-enough2? guess x) 
  (< (abs (- (square guess) x)) .001)) 



;;Finally, we need a way to get started. For instance, we can always guess that the square root of any 
;;number is 1:23
(define (sqrt x) 
  (sqrt-iter 1.0 x)) 





;;Exercise 1.7.  The good-enough? test used in computing square roots will not be very effective for 
;;finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost 
;;always performed with limited precision. This makes our test inadequate for very large numbers. Explain 
;;these statements, with examples showing how the test fails for small and large numbers. An alternative 
;;strategy for implementing good-enough? is to watch how guess changes from one iteration to the 
;;next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that 
;;uses this kind of end test. Does this work better for small and large numbers? 


;; My version just modifies good-enough?
(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) (abs (/ x 1000000000)))) 





;;Exercise 1.8.  Newton's method for cube roots is based on the fact that if y is an approximation to the cube 
;;root of x, then a better approximation is given by the value 
;;
;;(/ (+ (/ x (sq y)) (* y 2)) 3)
;;
;;Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In 
;;section 1.3.4 we will see how to implement Newton's method in general as an abstraction of these square- 
;;root and cube-root procedures.) 


(define (sqrt-iter guess x) 
  (if (good-enough? guess x) 
      guess 
      (sqrt-iter (improve guess x) 
                 x))) 

(define (improve guess x) 
  (average guess (/ x guess))) 


(define (average x y) 
  (/ (+ x y) 2)) 



(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) (abs (/ x 1000000000)))) 


(define (sqrt x) 
  (sqrt-iter 1.0 x)) 
