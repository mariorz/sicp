

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


(define (ack m n)
  (cond 
   ((= m 0) (+ n 1))
   ((= n 0) (ack (- m 1) 1))
   (else (ack (- m 1) (ack m (- n 1))))))


;; What are the values of the following expressions?

(A 1 10)

(A 2 4)

(A 3 3)

;; Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))

;; 2n

(define (g n) (A 1 n))

;; 2**n

(define (h n) (A 2 n))
   (A 1 (A 2 n-1)
   (A 1 (A 1 (A 2 n-2)))


;;  (A 2 1)
;;  2

;;   (A 2 2)
;;   (A 1 (A 2 1))
;;   (A 1 2)
;;   (A 0 (A 1 1))
;;   (A 0 2)
;;     4
  
;;   (A 2 3)
;;   (A 1 (A 2 2))
;;   (A 1 (A 1 (A 2 1)))
;;   (A 1 (A 1 2))
;;   (A 1 (A 0 (A 1 1)))
;;   (A 1 (A 0 2))
;;   (A 1 4)
;;   (A 0 (A 1 3))
;;   (A 0 (A 0 (A 1 2)))
;;   (A 0 (A 0 (A 0 (A 1 1))))
;;   (A 0 (A 0 (A 0 2)))
;;   (A 0 (A 0 4))    
;;   (A 0 8)
;;    16

;; (A 2 4)
;; (A 1 16)
;;  65536

;; (A 2 5)
;; (A 1 65536)
 


(define (k n) (* 5 n n))



;; Give concise mathematical definitions for the functions computed by the procedures f, g, and h for positive 
;; integer values of n. For example, (k n) computes 5n2. 

;; Exercise 1.11.  
;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3. 
;; Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an 
;; iterative process.  


(define (myf n)
  (cond 
   ((< n 3) n)
   (else (+ (myf (- n 1)) (* 2 (myf (- n 2))) (* 3 (myf (- n 3)))))))

(define (myf2 n)
  (myf2-iter n 0))

(define (myf2-iter counter sum)
  (cond
   ((<= counter 2) sum)
   (else (myf2-iter (- counter 1) (+ sum (- counter 1) (* 2 (- counter 2)) (* 3 (- counter 3)))))))
  

;; ex 1.11. Iterative implementation 
  
 (define (f n) 
   (define (iter a b c count) 
     (if (= count 0) 
       a 
       (iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))) 
   (iter 0 1 2 n)) 


;; Exercise 1.12.  

;; The following pattern of numbers is called Pascal's triangle.

;; ...

;; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two
;; numbers above it.35 Write a procedure that computes elements of Pascal's triangle by means of a recursive process. 

(define (pasc x y)
  (cond
   ((> x y) 0)
   ((or (= x 0) (= y 0)) 1)
   (else (+ (pasc (- x 1) (- y 1)) (pasc x (- y 1))))))
  


;; 1.22 to 1.28



(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2) 3 (+ n 2))) 

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))



(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        



(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (timed-prime-test-fast n)
  (start-prime-test-fast n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time) n)))

(define (start-prime-test-fast n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (current-milliseconds) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))


(define (search-for-primes base ciel)
  (define (iter x y)
    (timed-prime-test x)
    (search-for-primes (+ x 2) y))
  (if (< base ciel)
      (if (even? base) 
	  (search-for-primes (+ base 1) ciel)
	  (iter base ciel))
      (newline)))




(define (search-for-primes-fast base ciel)
  (define (iter x y)
    (timed-prime-test-fast x)
    (search-for-primes-fast (+ x 2) y))
  (if (< base ciel)
      (if (even? base) 
	  (search-for-primes-fast (+ base 1) ciel)
	  (iter base ciel))
      (newline)))




(define sfp search-for-primes)
(define sfpf search-for-primes-fast)



(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))



(define (simp-integral f a b n)
  (define h (/ (- b a) n))
  (define (geth x) (+ x (* h n)))
  (+ (/ h 3)
     (sum f (+ a (* 0 h)) geth b)))
   
     
  

 (define (round-to-next-even x)
   (+ x (remainder x 2)))
 
 (define (simpson f a b n)
   (define fixed-n (round-to-next-even n))
   (define h (/ (- b a) fixed-n))
   (define (simpson-term k)
     (define y (f (+ a (* k h))))
     (if (or (= k 0) (= k fixed-n))
         (* 1 y)
         (if (even? k)
             (* 2 y)
             (* 4 y))))
   (* (/ h 3) (sum simpson-term 0 inc fixed-n)))





(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))



(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))




(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner 
       (term a) 
       (accumulate combiner null-value term (next a) next b))))


(define (newsum term a next b)
  (accumulate + 0 term a next b))

(define (newprod term a next b)
  (accumulate * 1 term a next b))
  
(define (newfactorial n)
  (newprod identity 1 inc n))


(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))



(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner 
			(if (filter a) 
			    (term a)
			    null-value)
			    result))))
  (iter a null-value))




(define (prime-squared-sum a b)
  (filtered-accumulate + 0 square a inc b prime?))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


(define (factorial n)
  (product identity 1 inc n))



(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


(define (foo n)
  (if (odd? n)
      (- n 1)
      n))

(define (bar n)
  (if (even? n)
      (- n 1)
      n))

(define form-p 
  (* 
   (/
    (product foo 3 inc 100000)
    (product bar 3 inc 100000))
   4.0))
