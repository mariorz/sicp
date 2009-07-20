

;;Exercise 3.1.  An accumulator is a procedure that is called repeatedly with a single 
;;numeric argument and accumulates its arguments into a sum. Each time it is called, 
;;it returns the currently accumulated sum. Write a procedure make-accumulator that 
;;generates accumulators, each maintaining an independent sum. The input to 
;;make-accumulator should specify the initial value of the sum; for example



(define (make-accumulator accum)
  (lambda (n)
    (begin (set! accum (+ accum n))
	   accum)))



(define A (make-accumulator 5))
(A 10)
15
(A 10)
25




;;Exercise 3.2.  In software-testing applications, it is useful to be able to count 
;;the number of times a given procedure is called during the course of a computation. 
;;Write a procedure make-monitored that takes as input a procedure, f, that itself takes 
;;one input. The result returned by make-monitored is a third procedure, say mf, that 
;;keeps track of the number of times it has been called by maintaining an internal counter. 
;;If the input to mf is the special symbol how-many-calls?, then mf returns the value of 
;;the counter. If the input is the special symbol reset-count, then mf resets the counter 
;;to zero. For any other input, mf returns the result of calling f on that input and 
;;increments the counter. For instance, we could make a monitored version of the sqrt 
;;procedure:


(define (make-monitored f)
  (let ((counter 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?) counter)
	    ((eq? m 'reset-count) (set! counter 0))
	    (else 
	     (set! counter (+ 1 counter))
	     (f m))))))


(define s (make-monitored sqrt))

(s 100)
10

(s 'how-many-calls?)
1


;;Exercise 3.3.  Modify the make-account procedure so that it creates password-protected 
;;accounts. That is, make-account should take a symbol as an additional argument, as in



;;The resulting account object should process a request only if it is accompanied by the 
;;password with which the account was created, and should otherwise return a complaint:


(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass passwd)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
	(error "wrong password")))
  dispatch)




(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
60

((acc 'some-other-password 'deposit) 50)
"Incorrect password"





;;Exercise 3.4.  Modify the make-account procedure of exercise 3.3 by adding another local 
;;state variable so that, if an account is accessed more than seven consecutive times with 
;;an incorrect password, it invokes the procedure call-the-cops. 


(define (make-account balance passwd)
  (let ((crack-count 1))
    (define (call-the-cops x)
      (display "the pigs are coming for you"))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (if (eq? pass passwd)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  (if (>= 7 crack-count)
	      (begin (set! crack-count (+ 1 crack-count))
		     (error "wrong password"))
	      call-the-cops)))
  dispatch))



(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


;;Exercise 3.5.  Monte Carlo integration is a method of estimating definite integrals by 
;;means of Monte Carlo simulation. Consider computing the area of a region of space described 
;;by a predicate P(x, y) that is true for points (x, y) in the region and false for points not 
;;in the region. For example, the region contained within a circle of radius 3 centered at (5, 7) 
;;is described by the predicate that tests whether (x - 5)^2 + (y - 7)^2 < 32. To estimate the area 
;;of the region described by such a predicate, begin by choosing a rectangle that contains the 
;;region. For example, a rectangle with diagonally opposite corners at (2, 4) and (8, 10) contains 
;;the circle above. The desired integral is the area of that portion of the rectangle that lies in 
;;the region. We can estimate the integral by picking, at random, points (x,y) that lie in the 
;;rectangle, and testing P(x, y) for each point to determine whether the point lies in the region. 
;;If we try this with many points, then the fraction of points that fall in the region should give 
;;an estimate of the proportion of the rectangle that lies in the region. Hence, multiplying this 
;;fraction by the area of the entire rectangle should produce an estimate of the integral.

;;Implement Monte Carlo integration as a procedure estimate-integral that takes as arguments a 
;;predicate P, upper and lower bounds x1, x2, y1, and y2 for the rectangle, and the number of trials 
;;to perform in order to produce the estimate. Your procedure should use the same monte-carlo 
;;procedure that was used above to estimate . Use your estimate-integral to produce an estimate of 
;;by measuring the area of a unit circle.

;;You will find it useful to have a procedure that returns a number chosen at random from a given 
;;range. The following random-in-range procedure implements this in terms of the random procedure 
;;used in section 1.2.6, which returns a nonnegative number less than its input.8

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (rect-area x1 x2 y1 y2)
  (* (- x2 x1) (y2 y1))

(define (reg-test p x1 x2 y1 y2)
  (p (rand-in-range x1 x2) (rand-in-range y1 y2)))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (rect-area x1 x2 y1 y2) 
     (monter-carlo trials (reg-test p x1 x2 y1 y2))))
  


;;Exercise 3.6.  It is useful to be able to reset a random-number generator to produce a sequence 
;;starting from a given value. Design a new rand procedure that is called with an argument that is 
;;either the symbol generate or the symbol reset and behaves as follows: (rand 'generate) produces 
;;a new random number; ((rand 'reset) <new-value>) resets the internal state variable to the 
;;designated <new-value>. Thus, by resetting the state, one can generate repeatable sequences. 
;;These are very handy to have when testing and debugging programs that use random numbers. 



(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))



(define rand 
  (let ((x (random 2147483647)))
    (lambda (c)
      (cond ((eq? c 'generate)
	     (begin
	       (random-seed x)
	       (set! x (random 2147483647))
	       x))
	    ((eq? c 'reset)
	     (lambda (y)
	       (set! x y)))))))
	

;;Exercise 3.7.  Consider the bank account objects created by make-account, with the password 
;;modification described in exercise 3.3. Suppose that our banking system requires the ability to 
;;make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take 
;;three arguments. The first is a password-protected account. The second argument must match the 
;;password with which the account was defined in order for the make-joint operation to proceed. The 
;;third argument is a new password. Make-joint is to create an additional access to the original 
;;account using the new password. For example, if peter-acc is a bank account with password 
;;open-sesame, then

(define paul-acc
  (make-joint acc 'secret-password 'rosebud))

;;will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. 
;;You may wish to modify your solution to exercise 3.3 to accommodate this new feature. 


(define (make-account balance passwd)
  (let ((root-pass passwd)
	(altern-pass null))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (add-user pass)
      (set! altern-pass pass))
    (define (dispatch pass m)
      (if (or (eq? pass root-pass) 
	      (and (eq? pass altern-pass) (not (eq? pass null))))
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
	((eq? m 'join) add-user)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  (error "wrong password")))
    dispatch))


(define (make-joint account passwd newpasswd)
  ((account passwd 'join) newpasswd))
  


;;Exercise 3.8.  When we defined the evaluation model in section 1.1.3, we said that the first step 
;;in evaluating an expression is to evaluate its subexpressions. But we never specified the order in 
;;which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce 
;;assignment, the order in which the arguments to a procedure are evaluated can make a difference to the 
;;result. Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments 
;;to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left. 


(define f
  (let ((flag 0))
    (lambda (x)
      (if (= flag 0)
	  (begin 
	    (set! flag x)
	    0)
	  flag))))





;;Exercise 3.12.  The following procedure for appending lists was introduced in section 2.2.1:

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;;Append forms a new list by successively consing the elements of x onto y. The procedure append! 
;;is similar to append, but it is a mutator rather than a constructor. It appends the lists by splicing 
;;them together, modifying the final pair of x so that its cdr is now y. (It is an error to call append! 
;;with an empty x.)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;;Here last-pair is a procedure that returns the last pair in its argument:

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;;Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(a b c d)
(cdr x)
;h: (b)
(define w (append! x y))
w
(a b c d)
(cdr x)
;h (b c d)

;;What are the missing <response>s? Draw box-and-pointer diagrams to explain your answer.

;;Exercise 3.13.  Consider the following make-cycle procedure, which uses the last-pair 
;;procedure defined in exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;Draw a box-and-pointer diagram that shows the structure z created by

(define z (make-cycle (list 'a 'b 'c)))

;What happens if we try to compute (last-pair z)?

; h: infinite recurssion.


;;Exercise 3.14.  The following procedure is quite useful, although obscure:

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


;;Loop uses the ``temporary'' variable temp to hold the old value of the cdr of x, 
;;since the set-cdr! on the next line destroys the cdr. Explain what mystery does in 
;;general. Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer 
;;diagram that represents the list to which v is bound. Suppose that we now evaluate 
;;(define w (mystery v)). Draw box-and-pointer diagrams that show the structures v and w 
;;after evaluating this expression. What would be printed as the values of v and w ? 

;; answer:
;; it recursively sets the cdr of a cell in the list to the cell which originally points to it
;; setting the first element cdr to point to null, so the list gets inversed. After the evaluations 
;; v still points to 'a' but the cdr of that cell now points to null.  


;;Exercise 3.16.  Ben Bitdiddle decides to write a procedure to count the number of pairs in any 
;;list structure. ``It's easy,'' he reasons. ``The number of pairs in any structure is the number 
;;in the car plus the number in the cdr plus one more to count the current pair.'' So Ben writes 
;;the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing 
;;list structures made up of exactly three pairs for which Ben's procedure would return 3; 
;;return 4; return 7; never return at all. 

; (count-pairs (list 1 2 3))
; => 3
; (define x '(1))
; (set-cdr! x (list 2 3 x))
; (count-pairs x)
; => infinte recurssion
; (define x (list 1 2))
; (define x (cons x (cdr x)))  
; (count-pairs x)
; => 4


;;Exercise 3.17.  Devise a correct version of the count-pairs procedure of exercise 3.16 that returns the 
;;number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data 
;;structure that is used to keep track of which pairs have already been counted.) 

(define (count-pairs x)
  (let ((counted '()))
    (define (count z)
      (if (not (pair? z))
	  0
	  (if (memq z counted)
	      0
	      (begin
		(append! counted (list z))
		(+ (count (car z))
		   (count (cdr z))
		   1)))))
    (count x)))




;;Exercise 3.18.  Write a procedure that examines a list and determines whether it contains a cycle, that is, 
;;whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite 
;;loop. Exercise 3.13 constructed such lists.


; inimino's sulution
(define (cyclical? x)
  (define (go x seen)
    (cond ((memq x seen) true)
          ((null? x) false)
          (else (go (cdr x) (cons x seen)))))
  (go x '()))

			 

;;Exercise 3.19.  Redo exercise 3.18 using an algorithm that takes only a constant amount of space. 
;;(This requires a very clever idea.) 

;inimino's
; nice one
(define (cyclical? x)
  (define (go n m ancestor x)
    (cond ((null? x) false)
          ((eq? ancestor x) true)
          ((= m n) (go (* 2 n) (+ 1 m) x        (cdr x)))
          (else    (go n       (+ 1 m) ancestor (cdr x)))))
  (go 1 1 '(unused) x))

;aamar's
(define (find-loop obj)
  (define (check x y)
    (if (eq? x y)
  #t
  (if (or (not (pair? y)) (not (pair? (cdr y))) (not (pair? (cddr y))))
   #f
   (check (cdr x) (cddr y)))))
  (check obj (cdr obj)))


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 


;Exercise 3.21.  Ben Bitdiddle decides to test the queue implementation described above. 
;He types in the procedures to the Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))
(insert-queue! q1 'a)
((a) a)
(insert-queue! q1 'b)
((a b) b)
(delete-queue! q1)
((b) b)
(delete-queue! q1)
(() b)

;``It's all wrong!'' he complains. ``The interpreter's response shows that the last 
;item is inserted into the queue twice. And when I delete both items, the second b is 
;still there, so the queue isn't empty, even though it's supposed to be.'' Eva Lu Ator 
;suggests that Ben has misunderstood what is happening. ``It's not that the items are 
;going into the queue twice,'' she explains. ``It's just that the standard Lisp printer 
;doesn't know how to make sense of the queue representation. If you want to see the queue 
;printed correctly, you'll have to define your own print procedure for queues.'' 
;Explain what Eva Lu is talking about. In particular, show why Ben's examples produce the 
;printed results that they do. Define a procedure print-queue that takes a queue as input 
;and prints the sequence of items in the queue. 


(define (print-queue q)
  (front-ptr q))


;;Exercise 3.22.  Instead of representing a queue as a pair of pointers, we can build a queue 
;;as a procedure with local state. The local state will consist of pointers to the beginning and 
;;the end of an ordinary list. Thus, the make-queue procedure will have the form




(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT! called with empty queue")
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))
	front-ptr)) 
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))
	    front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    (else (error "Undefined operation -- Queue" m))))
    dispatch))



;;Exercise 3.23.  A deque (``double-ended queue'') is a sequence in which items can be inserted 
;;and deleted at either the front or the rear. Operations on deques are the constructor 
;;make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators 
;;front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how 
;;to represent deques using pairs, and give implementations of the operations.23 All operations 
;;should be accomplished in (1) steps. 


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty dequeue" deque)
      (caar (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty dequeue" deque)
      (caar (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) (cons (rear-ptr deque) '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-rear-ptr! deque new-pair)
           deque)))) 

(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item '()) (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque)))) 

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque) (error "DELETE! called with an empty deque" deque))
        (else
	 (begin
	   (set-front-ptr! deque (cddr (front-ptr deque)))
	   (set-car! (cdr (front-ptr deque)) '()))
         deque))) 


(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque) (error "DELETE! called with an empty deque" deque))
        (else
	 (begin
	   (set-rear-ptr! deque (cadr (rear-ptr deque)))
	   (set-cdr! (cdr (rear-ptr deque)) '()))
         deque))) 


(define (print-deque deque)
  (define (inner head) 
    (if (null? head)
	(display "")
	(begin
	  (display (caar head))
	  (inner (cddr head)))))
  (inner (car deque)))


; node in list: ((val) (prev . next))
;--------

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

;To construct a new table, we simply create a list containing the symbol *table*:

(define (make-table)
  (list '*table*))

; 2D tables


(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))


(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)




(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;;Exercise 3.24.  In the table implementations above, the keys are tested for 
;;equality using equal? (called by assoc). This is not always the appropriate test. 
;;For instance, we might have a table with numeric keys in which we don't need an 
;;exact match to the number we're looking up, but only a number within some tolerance 
;;of it. Design a table constructor make-table that takes as an argument a same-key? 
;;procedure that will be used to test ``equality'' of keys. Make-table should return 
;;a dispatch procedure that can be used to access appropriate lookup and insert! 
;;procedures for a local table. 
