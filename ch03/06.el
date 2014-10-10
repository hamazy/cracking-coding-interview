(defun cci-sort-stack (stack)
  (if (cci-stack-emptyp stack) stack
    (let ((buf (cci-stack-create))
	  (max (cci-stack-pop stack))
	  (count 0))
      (while (not (cci-stack-emptyp stack))
	(if (< (cci-stack-peek stack) max)
	    (cci-stack-push buf (cci-stack-pop stack))
	  (progn
	    (cci-stack-push buf max)
	    (setq max (cci-stack-pop stack))))
	(setq count (1+ count)))
      (cci-stack-push stack max)
      (cci-sort-stack-helper count buf stack))))

(defun cci-sort-stack-helper (n buf stack)
  (when (< 0 n)
    ;; n elements in buf. pick one from buf as temporary max.
    (let ((max (cci-stack-pop buf)))
      (while (not (cci-stack-emptyp buf))
	(if (< (cci-stack-peek buf) max)
	    ;; if max is larger, move the element from buf to stack
	    (cci-stack-push stack (cci-stack-pop buf))
	  ;; otherwise, put max into stack and update max.
	  (progn
	    (cci-stack-push stack max)
	    (setq max (cci-stack-pop buf)))))
      ;; at this point, n - 1 unsorted elements are in the stack and
      ;; the largest value is stored as max. So, put the unsorted
      ;; element back to the buf.
      (let ((counter 0))
	(while (< counter (1- n))
	  (cci-stack-push buf (cci-stack-pop stack))
	  (setq counter (1+ counter))))
      ;; put the largest value into the stack
      (cci-stack-push stack max)
      ;; at this point, there're 
      (cci-sort-stack-helper (1- n) buf stack))))

(let ((stack (cci-stack-create)))
  (cci-stack-push stack 2)
  (cci-stack-push stack 0)
  (cci-stack-push stack 1)
  (cci-stack-push stack 3)
  (cci-sort-stack stack)
  (cci-stack-as-list stack))
