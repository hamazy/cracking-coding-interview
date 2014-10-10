(defun cci-stack-create ()
  (cons nil nil))

(defun cci-stack-push (stack value)
  (setcar stack (cons value (car stack))))

(defun cci-stack-pop (stack)
  (when (car stack)
    (let ((result (car (car stack))))
      (setcar stack (cdr (car stack)))
      result)))

(defun cci-stack-peek (stack)
  (when (car stack)
    (car (car stack))))

(defun cci-stack-emptyp (stack)
  (not (car stack)))

(defun cci-stack-as-list (stack)
  (car stack))

(let ((stack (cci-stack-create)))
  (let ((empty-at-first (cci-stack-emptyp stack)))
    (cci-stack-push stack 0)
    (cci-stack-push stack 1)
    (and empty-at-first
	 (eq (cci-stack-peek stack) 1)
	 (eq (cci-stack-pop stack) 1)
	 (eq (cci-stack-peek stack) 0)
	 (eq (cci-stack-pop stack) 0)
	 (cci-stack-emptyp stack))))

(defun cci-hanoi-move (n src-stack dst-stack buf-stack)
  (cond ((= 0 n) nil)
	((= 1 n) (cci-stack-push dst-stack (cci-stack-pop src-stack)))
	(t       (cci-hanoi-move (1- n) src-stack buf-stack dst-stack)
		 (cci-stack-push dst-stack (cci-stack-pop src-stack))
		 (cci-hanoi-move (1- n) buf-stack dst-stack src-stack))))

(let ((src-stack (cci-stack-create))
      (dst-stack (cci-stack-create))
      (buf-stack (cci-stack-create)))
  (cci-stack-push src-stack 5)
  (cci-stack-push src-stack 4)
  (cci-stack-push src-stack 3)
  (cci-stack-push src-stack 2)
  (cci-stack-push src-stack 1)
  (cci-stack-push src-stack 0)

  (and (equal (cci-stack-as-list src-stack) '(0 1 2 3 4 5))
       (equal (cci-stack-as-list dst-stack) nil))

  (cci-hanoi-move 6 src-stack dst-stack buf-stack)

  (and (equal (cci-stack-as-list dst-stack) '(0 1 2 3 4 5))
       (equal (cci-stack-as-list src-stack) nil)))
