

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
	   ;(set-cdr! (cdr (front-ptr deque) new-pair))
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

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key table)
      (cond ((null? table) #f)
	    ((same-key? key (caar table)) (car table))
	    (else (assoc key (cdr table)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
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



(define (test-table)
  (let ((foo (make-table eq?)))
    ((foo 'insert-proc!) 'x 'y 10)
    ((foo 'lookup-proc) 'x 'y)))

;;Exercise 3.25.  Generalizing one- and two-dimensional tables, show how to 
;;implement a table in which values are stored under an arbitrary number of keys 
;;and different values may be stored under different numbers of keys. The lookup 
;;and insert! procedures should take as input a list of keys used to access the table. 
  
; not finished. lookup doesnt work

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key table)
      (cond ((null? table) #f)
	    ((same-key? key (caar table)) (car table))
	    (else (assoc key (cdr table)))))
    (define (lookup keys)
      (define (iter-look this-keys table)
	(let ((subtable (assoc (car this-keys) (cdr local-table))))
	  (if subtable
	      (if (null? (cdr this-keys))
		  (cdr subtable)
		  (iter-look (cdr this-keys) subtable))
	      #f)))
      (iter-look keys local-table))
    (define (insert! key-list value)
      (define (make-record keys)
	(if (null? (cdr keys)) 
	    (cons (car keys) value)
	    (list (car keys) (make-record (cdr keys)))))
      (define (iter-insert! keys table)
	(let ((subtable (assoc (car keys) (cdr table))))
	  (if subtable
	      (if (null? (cdr keys))
		  (set-cdr! subtable value)
		(iter-insert! (cdr keys) subtable))
	      (set-cdr! table (cons (make-record keys) (cdr table))))))
      (iter-insert! key-list local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))











(define (make-table)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup1 keys table)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (cdr subtable)
                  (lookup1 (cdr keys) subtable))
              #f)))
      (lookup1 key-list local-table))
    (define (insert! key-list value)
      (define (make-entry keys)
        (if (null? (cdr keys))
            (cons (car keys) value)
            (list (car keys) (make-entry (cdr keys)))))
      (define (insert1 keys table)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (set-cdr! subtable value)
                  (insert1 (cdr keys) subtable))
              (set-cdr! table
                        (cons (make-entry keys)
                              (cdr table))))))
      (insert1 key-list local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))





;;Exercise 3.26.  To search a table as implemented above, one needs to scan through 
;;the list of records. This is basically the unordered list representation of section 
;;2.3.3. For large tables, it may be more efficient to structure the table in a different
;; manner. Describe a table implementation where the (key, value) records are organized 
;;using a binary tree, assuming that keys can be ordered in some way 
;;(e.g., numerically or alphabetically). (Compare exercise 2.66 of chapter 2.) 


;; this can be acomplished using something like lookup implementation for binary trees of 
;;exercise 2.66, ofc course it would need to be acompanied by its counterpart insert! function.





;;Exercise 3.27.  Memoization (also called tabulation) is a technique that enables a 
;;procedure to record, in a local table, values that have previously been computed. 
;;This technique can make a vast difference in the performance of a program. A memoized 
;;procedure maintains a table in which values of previous calls are stored using as keys 
;;the arguments that produced the values. When the memoized procedure is asked to compute a value, 
;;it first checks the table to see if the value is already there and, if so, just returns 
;;that value. Otherwise, it computes the new value in the ordinary way and stores this in 
;;the table. As an example of memoization, recall from section 1.2.2 the exponential 
;;process for computing Fibonacci numbers:



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))



(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;;;where the memoizer is defined as

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))



;;Draw an environment diagram to analyze the computation of (memo-fib 3). 
;;Explain why memo-fib computes the nth Fibonacci number in a number of steps 
;;proportional to n. Would the scheme still work if we had simply defined memo-fib 
;;to be (memoize fib)? 





;...

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;;An and-gate is a little more complex. The action procedure must be run if either of 
;;the inputs to the gate changes. It computes the logical-and (using a procedure 
;;analogous to logical-not) of the values of the signals on the input wires and sets
;;up a change to the new value to occur on the output wire after one and-gate-delay.

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;;Exercise 3.28.  Define an or-gate as a primitive function box. Your or-gate constructor 
;;should be similar to and-gate.

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;Exercise 3.29.  Another way to construct an or-gate is as a compound digital logic device, 
;;built from and-gates and inverters. Define a procedure or-gate that accomplishes this. 
;;What is the delay time of the or-gate in terms of and-gate-delay and inverter-delay?



(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((a1-inv (make-wire))
          (a2-inb (make-wire))
          (b (make-wire)))
      (inverter a1 a1-inv)
      (inverter a2 a2-inv)
      (and-gate a1-inv a2-inv b)
      (inverter b output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


;;Exercise 3.30.  Figure 3.27 shows a ripple-carry adder formed by stringing together n 
;;full-adders. This is the simplest form of parallel adder for adding two n-bit binary numbers. 
;;The inputs A1, A2, A3, ..., An and B1, B2, B3, ..., Bn are the two binary numbers to be added 
;;(each Ak and Bk is a 0 or a 1). The circuit generates S1, S2, S3, ..., Sn, the n bits of the 
;;sum, and C, the carry from the addition. Write a procedure ripple-carry-adder that generates 
;;this circuit. The procedure should take as arguments three lists of n wires each -- the Ak, 
;;the Bk, and the Sk -- and also another wire C. The major drawback of the ripple-carry adder 
;;is the need to wait for the carry signals to propagate. What is the delay needed to obtain 
;;the complete output from an n-bit ripple-carry adder, expressed in terms of the delays for 
;;and-gates, or-gates, and inverters? 


(define (ripple-carry-adder A B S C)
  (let ((c-in (make-wire)))
    (if (null? (cdr A))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-in))
    (full-adder (car A) (car B) c-in (car S) C)))




;;A wire in our simulation will be a computational object with two local state variables: 
;;a signal-value (initially taken to be 0) and a collection of action-procedures to be run 
;;when the signal changes value. We implement the wire, using message-passing style, as a 
;;collection of local procedures together with a dispatch procedure that selects the 
;;appropriate local operation, just as we did with the simple bank-account object in 
;;section  3.1.1:

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;;The local procedure set-my-signal! tests whether the new signal value changes the signal 
;;on the wire. If so, it runs each of the action procedures, using the following procedure 
;;call-each, which calls each of the items in a list of no-argument procedures:

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))



;;With the local dispatch procedure set up as specified, we can provide the following 
;;procedures to access the local operations on wires:27

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))




;;The particular agenda that we use is denoted by the-agenda. 
;;The procedure after-delay adds new ele;;ments to the-agenda:

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;;The simulation is driven by the procedure propagate, which operates on the-agenda, 
;;executing each procedure on the agenda in sequence. In general, as the simulation 
;;runs, new items will be added to the agenda, and propagate will continue the simulation 
;;as long as there are items on the agenda:

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))



;;The following procedure, which places a ``probe'' on a wire, shows the simulator in 
;;action. The probe tells the wire that, whenever its signal changes value, it should 
;;print the new signal value, together with the current time and a name that identifies 
;;the wire:



(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))




;;We begin by initializing the agenda and specifying delays for the primitive 
;;function boxes:

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)



;;Now we define four wires, placing probes on two of them:

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
sum 0  New-value = 0
(probe 'carry carry)
carry 0  New-value = 0


;;Next we connect the wires in a half-adder circuit (as in figure 3.25), 
;;set the signal on input-1 to 1, and run the simulation:

(half-adder input-1 input-2 sum carry)
ok
(set-signal! input-1 1)
done
(propagate)
sum 8  New-value = 1
done

;;The sum signal changes to 1 at time 8. We are now eight time units from the 
;;beginning of the simulation. At this point, we can set the signal on input-2 
;;to 1 and allow the values to propagate:

(set-signal! input-2 1)
done
(propagate)
carry 11  New-value = 1
sum 16  New-value = 0
done

;;The carry changes to 1 at time 11 and the sum changes to 0 at time 16.

;;Exercise 3.31.   The internal procedure accept-action-procedure! defined in make-wire 
;;specifies that when a new action procedure is added to a wire, the procedure is immediately 
;;run. Explain why this initialization is necessary. In particular, trace through the 
;;half-adder example in the paragraphs above and say how the system's response would differ 
;;if we had defined accept-action-procedure! as


;; if the precedure is not run the first time it is added to the wire, the agenda table
;; will remain empty and the simulation will never begin.


(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures)))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))





(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))



(define (empty-agenda? agenda)
  (null? (segments agenda)))






;;To add an action to an agenda, we first check if the agenda is empty. 
;;If so, we create a time segment for the action and install this in the agenda. 
;;Otherwise, we scan the agenda, examining the time of each segment. If we find a 
;;segment for our appointed time, we add the action to the associated queue. If we 
;;reach a time later than the one to which we are appointed, we insert a new time 
;;segment into the agenda just before it. If we reach the end of the agenda, we must 
;;create a new time segment at the end.

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

;;The procedure that removes the first item from the agenda deletes the item at the 
;;front of the queue in the first time segment. If this deletion makes the time segment 
;;empty, we remove it from the list of segments

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

;;The first agenda item is found at the head of the queue in the first time segment. 
;;Whenever we extract an item, we also update the current time:30

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;Exercise 3.32.  The procedures to be run during each time segment of the agenda are 
;;kept in a queue. Thus, the procedures for each segment are called in the order in which 
;;they were added to the agenda (first in, first out). Explain why this order must be used. 
;;In particular, trace the behavior of an and-gate whose inputs change from 0,1 to 1,0 in the 
;;same segment and say how the behavior would differ if we stored a segment's procedures in 
;;an ordinary list, adding and removing procedures only at the front (last in, first out). 




;; the order of evaluation of items in the agenda is important becuase each procedure it
;; it contains depends on the current state of the system at the time it was added to the agenda.








(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))








(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)




(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)




(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)





(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
		 (set! value false)
		 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))




(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))





(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))





;;Exercise 3.33.  Using primitive multiplier, adder, and constant constraints, 
;;define a procedure averager that takes three connectors a, b, and c as inputs and 
;;establishes the constraint that the value of c is the average of the values of a and b. 


(define (averager a b c)
  (let 
      ((absum (make-connector))
       (n (make-connector)))
    (adder a b absum)
    (multiplier n c absum)
    (constant 2 n)
    'ok))





;;Exercise 3.34.  Louis Reasoner wants to build a squarer, a constraint device with two 
;;terminals such that the value of connector b on the second terminal will always be the 
;;square of the value a on the first terminal. He proposes the following simple device made 
;;from a multiplier:

(define (squarer a b)
  (multiplier a a b))

;;There is a serious flaw in this idea. Explain. 


;; this will work for getting the sqaure of a, yet getting the inverse (square root)
;; of b will fail.




;;Exercise 3.35.  Ben Bitdiddle tells Louis that one way to avoid the trouble in exercise 
;;3.34 is to define a squarer as a new primitive constraint. Fill in the missing portions in 
;;Ben's outline for a procedure to implement such a constraint:

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a) 
	    (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value) 
    (forget-value! a me)
    (forget-value! b me))
  (define (me request) 
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)




;;Exercise 3.36.  Suppose we evaluate the following sequence of expressions in the global 
;;environment:

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

;;At some time during evaluation of the set-value!, the following expression from the 
;;connector's local procedure is evaluated:

(for-each-except setter inform-about-value constraints)

;;Draw an environment diagram showing the environment in which the above expression is evaluated. 



;; setter---> 'user
;; inform-about-value -> 'I have a value message
;; constraints --> constraint list is empty




;;Exercise 3.37.  The celsius-fahrenheit-converter procedure is cumbersome when compared 
;;with a more expression-oriented style of definition, such as

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

;;Here c+, c*, etc. are the ``constraint'' versions of the arithmetic operations. 
;;For example, c+ takes two connectors as arguments and returns a connector that is 
;;related to these by an adder constraint:

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;;Define analogous procedures c-, c*, c/, and cv (constant value) that enable us to define 
;;compound constraints as in the converter example above

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))
