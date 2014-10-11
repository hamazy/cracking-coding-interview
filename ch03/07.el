(defun cci-queue-create ()
  (cons nil nil))

(defun cci-queue-enqueue (queue value)
  (if (car queue)
      (let ((new-item (cons value nil)))
	(setcdr (cdr queue) new-item)
	(setcdr queue new-item))
    (progn
      (setcar queue (cons value nil))
      (setcdr queue (car queue)))))

(defun cci-queue-dequeue (queue)
  (when (car queue)
    (let ((result (car (car queue))))
      (setcar queue (cdr (car queue)))
      result)))

(defun cci-queue-peek (queue)
  (when (car queue)
    (car (car queue))))

(defun cci-dog-create (value)
  (cons 'dog value))

(defun cci-cat-create (value)
  (cons 'cat value))

(defun cci-animal-kind (animal)
  (car animal))

(defun cci-animal-value (animal)
  (cdr animal))

(defun cci-animal-queue-create ()
  (list (cci-queue-create)			; dog queue
	(cci-queue-create)			; cat queue
	-1))

(defun  cci-animal-enqueue (queue animal)
  (let ((dog-queue (car queue))
	(cat-queue (car (cdr queue)))
	(index (car (cddr queue))))
    (let ((target-queue (if (eq (cci-animal-kind animal) 'dog)
			    dog-queue
			  cat-queue)))
      (cci-queue-enqueue target-queue
			 (cons index animal)))
    (setcar (cddr queue) (1+ index))))

(defun cci-animal-dequeue-any (queue)
  (let ((dog-queue (car queue))
	(cat-queue (car (cdr queue))))
    (let ((dog (cci-queue-peek dog-queue))
	  (cat (cci-queue-peek cat-queue)))
      (let ((target-queue (if (and dog cat (car dog) (car cat)
				   (< (car dog) (car cat)))
			      dog-queue
			    cat-queue)))
	(cdr (cci-dequeue target-queue))))))

(defun cci-animal-dequeue-dog (queue)
  (cdr (cci-dequeue (car queue))))

(defun cci-animal-dequeue-cat (queue)
  (cdr (cci-dequeue (car (cdr queue)))))

(let ((queue (cci-animal-queue-create)))
  (cci-animal-enqueue queue (cci-dog-create 'dog1))
  (cci-animal-enqueue queue (cci-dog-create 'dog2))
  (cci-animal-enqueue queue (cci-cat-create 'cat1))
  (cci-animal-enqueue queue (cci-dog-create 'dog3))
  (cci-animal-enqueue queue (cci-cat-create 'cat2))
  (cci-animal-enqueue queue (cci-dog-create 'dog4)) ; dog1 dog2 cat1 dog3 cat2 dog4
  (let ((expect-dog1 (cci-animal-dequeue-any queue)) ; dog2 cat1 dog3 cat2 dog4
	(expect-cat1 (cci-animal-dequeue-cat queue)) ; dog2 dog3 cat2 dog4
	(expect-dog2 (cci-animal-dequeue-dog queue)) ; dog3 cat2 dog4
	(expect-dog3 (cci-animal-dequeue-any queue)) ; cat2 dog4
	(expect-dog4 (cci-animal-dequeue-dog queue)) ; cat2
	(expect-cat2 (cci-animal-dequeue-any queue))) ; (empty)
    (and (eq (cci-animal-value expect-dog1) 'dog1)
	 (eq (cci-animal-value expect-dog2) 'dog2)
	 (eq (cci-animal-value expect-dog3) 'dog3)
	 (eq (cci-animal-value expect-dog4) 'dog4)
	 (eq (cci-animal-value expect-cat1) 'cat1)
	 (eq (cci-animal-value expect-cat2) 'cat2)
	 (not (cci-animal-dequeue-any queue)))))
