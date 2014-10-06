(setq cci-set-of-stacks-max 2)

(defun cci-set-of-stacks-create ()
  (cons -1 (cons (make-vector cci-set-of-stacks-max nil) nil)))

(defun cci-set-of-stacks-push (stack value)
  (if (< (car stack) (1- cci-set-of-stacks-max))
      (progn (setcar stack (1+ (car stack)))
	     (aset (car (cdr stack)) (car stack) value))
    (progn (setcdr stack (cons (make-vector cci-set-of-stacks-max nil)
			       (cdr stack)))
	   (setcar stack 0)
	   (aset (car (cdr stack)) 0 value))))

(defun cci-set-of-stacks-pop (stack)
  (when (and (<= 0 (car stack)) (< (car stack) cci-set-of-stacks-max))
    (let ((result (aref (car (cdr stack)) (car stack))))
      (if (< 0 (car stack))
	  (setcar stack (1- (car stack)))
	(if (cdr (cdr stack))
	    (progn (setcar stack (1- cci-set-of-stacks-max))
		   (setcdr stack (cdr (cdr stack))))
	  (setcar stack -1)))
      result)))

(defun cci-set-of-stacks-peek (stack)
  (when (and (<= 0 (car stack)) (< (car stack) cci-set-of-stacks-max))
    (aref (car (cdr stack)) (car stack))))

(let ((stack (cci-set-of-stacks-create)))
  (cci-set-of-stacks-push stack 0)
  (cci-set-of-stacks-push stack 1)
  (cci-set-of-stacks-push stack 2)
  (cci-set-of-stacks-push stack 3)
  (and (eq (cci-set-of-stacks-peek stack) 3)
       (eq (cci-set-of-stacks-pop stack) 3)

       (eq (cci-set-of-stacks-peek stack) 2)
       (eq (cci-set-of-stacks-pop stack) 2)

       (eq (cci-set-of-stacks-peek stack) 1)
       (eq (cci-set-of-stacks-pop stack) 1)

       (eq (cci-set-of-stacks-peek stack) 0)
       (eq (cci-set-of-stacks-pop stack) 0)

       (eq (cci-set-of-stacks-peek stack) nil)
       (eq (cci-set-of-stacks-pop stack) nil)))
