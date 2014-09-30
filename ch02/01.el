(defun cci-remove-dup-element (element list)
  (if list
      (let ((first (car list))
	    (rest (cdr list)))
	(if (= element first)
	    (cci-remove-dup-element element rest)
	  (cons first (cci-remove-dup-element element rest))))
    list))

(defun cci-remove-dup (list)
  (if list
      (let ((first (car list))
	    (rest (cdr list)))
	(cons first (cci-remove-dup (cci-remove-dup-element first rest))))
    list))

(cci-remove-dup '(0 1 2 0 3 4 5 6))
;; => (0 1 2 3 4 5 6)

(cci-remove-dup '(0 1 2 3 1 4 5 6))
;; => (0 1 2 3 4 5 6)

(cci-remove-dup '(0 1 2 3 4 5 6 1))
;; => (0 1 2 3 4 5 6)

(cci-remove-dup '(0 1 0 2 1 3 4 5 6 1 5))
;; => (0 1 2 3 4 5 6)
