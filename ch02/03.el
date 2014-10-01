(defun cci-list-remove-middle (middle)
  (when middle
    (let ((next (cdr middle)))
      (when next
	    (setcar middle (car next))
	    (setcdr middle (cdr next))))))

(let ((list '(0 1 2 3 4)))
  (cci-list-remove-middle (nthcdr 0 list))
  list)
;; => (1 2 3 4)

(let ((list '(0 1 2 3 4)))
  (cci-list-remove-middle (nthcdr 2 list))
  list)
;; => (0 1 3 4)

(let ((list '(0 1 2 3 4)))
  (cci-list-remove-middle (nthcdr 3 list))
  list)
;; => (0 1 2 4)

(let ((list '(0 1 2 3 4)))
  (cci-list-remove-middle (nthcdr 4 list))
  list)
;; => (0 1 2 3 4)
;; This can't be done.
