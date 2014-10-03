(setq cci-3stack-max 256)

(defun cci-3stack-create ()
  (cons -3 (cons -2 (cons -1 (make-vector cci-3stack-max nil)))))

(defun cci-3stack-pushn (stack n object)
  (let ((array (nthcdr 3 stack))
	(index (nthcdr n stack)))
    (when (< (car index) (- cci-3stack-max 3))
      (setcar index (+ 3 (car index)))
      (aset array (car index) object))))

(defun cci-3stack-popn (stack n)
  (let ((array (nthcdr 3 stack))
	(index (nthcdr n stack)))
    (when (> (car index) -1)
      (let ((result (aref array (car index))))
	(setcar index (- (car index) 3))
	result))))

(defun cci-3stack-peekn (stack n)
  (let ((array (nthcdr 3 stack))
	(index (nthcdr n stack)))
    (when (<= 0 (car index))
	(aref array (car index)))))

(defun cci-3stack-push1 (stack object)
  (cci-3stack-pushn stack 0 object))

(defun cci-3stack-push2 (stack object)
  (cci-3stack-pushn stack 1 object))

(defun cci-3stack-push3 (stack object)
  (cci-3stack-pushn stack 2 object))

(defun cci-3stack-pop1 (stack)
  (cci-3stack-popn stack 0))

(defun cci-3stack-pop2 (stack)
  (cci-3stack-popn stack 1))

(defun cci-3stack-pop3 (stack)
  (cci-3stack-popn stack 2))

(defun cci-3stack-peek1 (stack)
  (cci-3stack-peekn stack 0))

(defun cci-3stack-peek2 (stack)
  (cci-3stack-peekn stack 1))

(defun cci-3stack-peek3 (stack)
  (cci-3stack-peekn stack 2))

(let ((stack (cci-3stack-create)))
  (cci-3stack-push1 stack 0)
  (cci-3stack-push1 stack 1)
  (cci-3stack-push1 stack 2)
  (cci-3stack-push1 stack 3)

  (cci-3stack-push2 stack 10)
  (cci-3stack-push2 stack 11)
  (cci-3stack-push2 stack 12)
  (cci-3stack-push2 stack 13)

  (cci-3stack-push3 stack 20)
  (cci-3stack-push3 stack 21)
  (cci-3stack-push3 stack 22)
  (cci-3stack-push3 stack 23)

  (and (eq (cci-3stack-peek1 stack) 3)
       (eq (cci-3stack-pop1 stack) 3)
       (eq (cci-3stack-peek1 stack) 2)
       (eq (cci-3stack-pop1 stack) 2)
       (eq (cci-3stack-peek1 stack) 1)
       (eq (cci-3stack-pop1 stack) 1)
       (eq (cci-3stack-peek1 stack) 0)
       (eq (cci-3stack-pop1 stack) 0)
       (eq (cci-3stack-peek1 stack) nil)
       (eq (cci-3stack-pop1 stack) nil)

       (eq (cci-3stack-peek2 stack) 13)
       (eq (cci-3stack-pop2 stack) 13)
       (eq (cci-3stack-peek2 stack) 12)
       (eq (cci-3stack-pop2 stack) 12)
       (eq (cci-3stack-peek2 stack) 11)
       (eq (cci-3stack-pop2 stack) 11)
       (eq (cci-3stack-peek2 stack) 10)
       (eq (cci-3stack-pop2 stack) 10)
       (eq (cci-3stack-peek2 stack) nil)
       (eq (cci-3stack-pop2 stack) nil)

       (eq (cci-3stack-peek3 stack) 23)
       (eq (cci-3stack-pop3 stack) 23)
       (eq (cci-3stack-peek3 stack) 22)
       (eq (cci-3stack-pop3 stack) 22)
       (eq (cci-3stack-peek3 stack) 21)
       (eq (cci-3stack-pop3 stack) 21)
       (eq (cci-3stack-peek3 stack) 20)
       (eq (cci-3stack-pop3 stack) 20)
       (eq (cci-3stack-peek3 stack) nil)
       (eq (cci-3stack-pop3 stack) nil)))
