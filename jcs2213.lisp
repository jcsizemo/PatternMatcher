;;; AI Assignment One: Patterns
;;; Author: John Sizemore


(defun match (pattern fact) 			; Match function. Takes 2 arguments: the pattern and the fact.
	   (setf pattern (list pattern))	; Turn both into lists, even if atoms
	   (setf fact (list fact)) 
	   (let 
	       (
		(hash (make-hash-table))	; create local variables for the binding list and the fail bit
		(match t)
		)
	     
	     (defun set-vars (pattern fact hash)	; this function sweeps the pattern/fact and assigns variables within the hash table
	       (if (and (listp pattern) (listp fact))		
		   (loop for i in pattern for j in fact do 	; go through each item in the pattern/fact if both are lists. i is for pattern elements, j for fact elements
			(if (listp i)
			    (if (equal (car i) '&)		; check for & operator
				(loop for k in i do
				     (if (listp k)		
					 (set-vars (list k) j hash)	; if there is an embedded list within the & operator, recursively call the function
					 (cond
					   ((equal ">" (subseq (symbol-name k) 0 1))	; check for greater than predicate
					    (progn
					      (let
						  (
						   (assigned nil)
						   )
						(loop for key being the hash-keys of hash do	; check to see if predicate precedes assignment
						     (if (equal key (subseq (symbol-name k) 1))
							 (setf assigned t)
							 ()
							 )
						     )
						(if (null assigned)	; if it does, the match fails
						    (setf match nil)
						    ()
						    )
						(if (and (symbolp k) (numberp (read-from-string (subseq (symbol-name k) 1))))	; if the predicate is paired with a number
						    (setf match nil)								; then the match fails as well
						    ()
						    )
						)
					      )
					    ()
					    )
					   ((equal "<" (subseq (symbol-name k) 0 1))	; similar case for less than as above. 
					    (progn
					      (let
						  (
						   (assigned nil)
						   )
						(loop for key being the hash-keys of hash do
						     (if (equal key (subseq (symbol-name k) 1))
							 (setf assigned t)
							 ()
							 )
						     )
						(if (null assigned)
						    (setf match nil)
						    ()
						    )
						(if (and (symbolp k) (numberp (read-from-string (subseq (symbol-name k) 1))))
						    (setf match nil)
						    ()
						    )
						)
					      )
					    ()
					    )
					   ((equal "!" (subseq (symbol-name k) 0 1))	; similar to > and < for the ! predicate.
					    (progn
					      (let
						  (
						   (assigned nil)
						   )
						(loop for key being the hash-keys of hash do
						     (if (equal key (subseq (symbol-name k) 1))
							 (setf assigned t)
							 ()
							 )
						     )
						(if (null assigned)
						    (setf match nil)
						    ()
						    )
						(if (and (symbolp k) (numberp (read-from-string (subseq (symbol-name k) 1))))
						    (setf match nil)
						    ()
						    )
						)
					      )
					    ()
					    )
					   ((equal "=" (subseq (symbol-name k) 0 1))	
					    (if (and (symbolp k) (symbolp (read-from-string (subseq (symbol-name k) 1))))
						(setf (gethash (subseq (symbol-name k) 1) hash) j)
						()
						)
					    ()					; the equal sign assigns to a variable. Checks to see if the variable is a number.
					    )					; If it is, no assignment happens.
					   )
					 )
				     )
				(set-vars i j hash)		; Recursively call the function if the element in the pattern is a list that does not begin with '&'.
				)
			    (cond			; This cond block is entered if the element in the pattern is not a list. Checks all predicates/assignments.
			      ((and (symbolp i) (equal ">" (subseq (symbol-name i) 0 1)))	; The behavior for predicates is identical to the previous section.
			       (progn
				 (let
				     (
				      (assigned nil)
				      )
				   (loop for key being the hash-keys of hash do
					(if (equal key (subseq (symbol-name i) 1))
					    (setf assigned t)
					    ()
					    )
					)
				   (if (null assigned)
				       (setf match nil)
				       ()
				       )
				   (if (and (symbolp i) (numberp (read-from-string (subseq (symbol-name i) 1))))
				       (setf match nil)
				       ()
				       )
				   )
				 )
			       ()
			       )
			      ((and (symbolp i) (equal "<" (subseq (symbol-name i) 0 1)))
			       (progn
				 (let
				     (
				      (assigned nil)
				      )
				   (loop for key being the hash-keys of hash do 
					(if (equal key (subseq (symbol-name i) 1))
					    (setf assigned t)
					    ()
					    )
					)
				   (if (null assigned)
				       (setf match nil)
				       ()
				       )
				   (if (and (symbolp i) (numberp (read-from-string (subseq (symbol-name i) 1))))
				       (setf match nil)
				       ()
				       )
				   )
				 )
			       ()
			       )
			      ((and (symbolp i) (equal "!" (subseq (symbol-name i) 0 1)))
			       (progn  
				 (let
				     (
				      (assigned nil)
				      )
				   (loop for key being the hash-keys of hash do
					(if (equal key (subseq (symbol-name i) 1))
					    (setf assigned t)
					    ()
					    )
					)
				   (if (null assigned)
				       (setf match nil)
				       ()
				       )
				   (if (and (symbolp i) (numberp (read-from-string (subseq (symbol-name i) 1))))
				       (setf match nil)
				       ()
				       )
				   )
				 )
			       ()
			       )
			      ((and (symbolp i) (equal "=" (subseq (symbol-name i) 0 1)))
			       (if (and (symbolp i) (symbolp (read-from-string (subseq (symbol-name i) 1))))
				   (setf (gethash (subseq (symbol-name i) 1) hash) j)
				   ()
				   )
			       ()
			       )
			      )
			    )
			)
		   )
	       hash		; This function returns the hash table with all assigned variables. Per the instructions, a variable is not assigned if it is a number.
	       )
	     
	     (defun check-logic (pattern fact hash)		; This function evaluates the logic within the pattern to check for a match.
	       (if (and (listp pattern) (listp fact))
		   (loop for i in pattern for j in fact do	; The structure of this function is very similar to that seen in "set-vars"
			(if (listp i)
			    (if (equal (car i) '&)		; Check for '&' block
				(loop for k in (cdr i) do
				     (if (listp k)
					 (check-logic (list k) j hash)
					 (cond						; Cond block evaluates all predicates within the '&' block.
					   ((equal (subseq (symbol-name k) 0 1) ">")
					    (loop for key being the hash-keys of hash do
						 (if (equal (subseq (symbol-name k) 1) key)
						     (if (or (> (gethash key hash) j) (equal (gethash key hash) j))	; pull value from hash table
							 (setf match nil)
							 ()
							 )
						     ())
						 )
					    )
					   ((equal (subseq (symbol-name k) 0 1) "<")
					    (loop for key being the hash-keys of hash do
						 (if (equal (subseq (symbol-name k) 1) key)
						     (if (or (< (gethash key hash) j) (equal (gethash key hash) j))
							 (setf match nil)
							 ()
							 )
						     ())
						 )
					    )
					   ((equal (subseq (symbol-name k) 0 1) "!")
					    (loop for key being the hash-keys of hash do
						 (if (equal (subseq (symbol-name k) 1) key)
						     (if (equal (gethash key hash) j)
							 (setf match nil)
							 ()
							 )
						     ())
						 )
					    )
					   ((equal (subseq (symbol-name k) 0 1) "=")
					    (loop for key being the hash-keys of hash do
						 (if (equal (subseq (symbol-name k) 1) key)
						     (if (equal (gethash key hash) j)
							 ()
							 (setf match nil)
							 )
						     ())
						 )
					    )
					   ) 
					 )
				     )
				(check-logic i j hash)	; recursively call the function if the pattern element is a list not starting with '&'.
				)
			    (if (symbolp i) (cond	; this if statement is entered if the pattern element is a symbol.
					      ((equal (subseq (symbol-name i) 0 1) ">")
					       (loop for key being the hash-keys of hash do
						    (if (equal (subseq (symbol-name i) 1) key)
							(if (or (> (gethash key hash) j) (equal (gethash key hash) j))
							    (setf match nil)
							    ()
							    )
							())					; all predicates evaluated similar to previous section
						    )
					       )
					      ((equal (subseq (symbol-name i) 0 1) "<")
					       (loop for key being the hash-keys of hash do
						    (if (equal (subseq (symbol-name i) 1) key)
							(if (or (< (gethash key hash) j) (equal (gethash key hash) j))
							    (setf match nil)
							    ()
							    )
							())
						    )
					       )
					      ((equal (subseq (symbol-name i) 0 1) "!")
					       (loop for key being the hash-keys of hash do
						    (if (equal (subseq (symbol-name i) 1) key)
							(if (equal (gethash key hash) j)
							    (setf match nil)
							    ()
							    )
							())
						    )
					       )
					      ((equal (subseq (symbol-name i) 0 1) "=")
					       (loop for key being the hash-keys of hash do
						    (if (equal (subseq (symbol-name i) 1) key)
							(if (equal (gethash key hash) j)
							    ()
							    (setf match nil)
							    )
							())
						    )
					       )
					      )
				)
			    )
		        )
		   )
	       match	; returns whether or not there is a match.
	       )
	     
	     
	     ; After defining the 'set-vars' and 'check-logic' functions, this part of the 'match' function is where the actual
	     ; processing takes place. First the hash table is created by a call to 'set-vars'. Then the logic is evaluated in
	     ; 'check-logic'. If there is a match, that function returns 'true'. If the function returns 'false', the function
	     ; simply exits with 'nil'. If there is a match, a new hash table is created where no variable has a duplicate
             ; entry. Duplicates occur if there are predicates checking for equality in the pattern. Once the new hash table
             ; is built, the return list is built and finally returned when finished.		

	     (setf hash (set-vars pattern fact hash))	
	     (if (check-logic pattern fact hash)
		 (progn
		   (let (
			 (returnhash (make-hash-table))
			 (returnlist nil)
			 )
		     (loop for key being the hash-keys of hash do
			  (let (
				(has-key nil)
				)
			    (loop for return-key being the hash-keys of returnhash do
				 (if (equal key return-key)
				     (setf has-key t)
				     ()
				     )
				 )
			    (if (null has-key)
				(setf (gethash key returnhash) (gethash key hash))
				()
				)
			    )
			  )
		     (loop for key being the hash-keys of returnhash do
			  (setf returnlist (append returnlist (cons (cons (intern (concatenate 'string "=" key)) (gethash key hash)) nil)))
			  )
		     (if (null returnlist)
			 t
			 returnlist
			 )
		     )
		   )
		 nil
		 )
	     )
	   )