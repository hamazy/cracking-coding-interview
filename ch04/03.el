(defun cci-node-set-right-tree (node tree)
  (setcar node
	  (cons (when (car node)
		  (caar node))
		(cons tree nil))))

(defun cci-node-set-left-tree (node tree)
  (setcar node
	  (cons tree (when (car node)
		       (cdar node)))))

(defun cci-bst-create (array)
  (when (< 0 (length array))
    (let ((middle (/ (length array) 2)))
      ;; create a node with the middle element
      (let ((node (cci-node-create (aref array middle)))
	    (left-tree)
	    (right-tree))
	;; left sub-tree
	(when (<= 0 (1- middle))
	  (let ((left-array (make-vector middle nil))
		(i 0))
	    ;; copy array to left-array
	    (while (< i middle)
	      (aset left-array i (aref array i))
	      (setq i (1+ i)))
	    (setq left-tree (cci-bst-create left-array))))
	;; right sub-tree
	(when (< (1+ middle) (length array))
	  (let ((right-array (make-vector (- (1- (length array)) middle) nil))
		(i (1+ middle)))
	    ;; copy array to right-array
	    (while (< i (length array))
	      (aset right-array (- i (1+ middle)) (aref array i))
	      (setq i (1+ i)))
	    (setq right-tree (cci-bst-create right-array))))
	(cci-node-set-right-tree node right-tree)
	(cci-node-set-left-tree node left-tree)
	node))))

(cci-bst-create [0 1 2 3 4])

(let ((tree (cci-bst-create [0 1 2])))
  (and (eq (car (cdr tree)) 1)
       (eq (car (cdr (car (cdr (car tree))))) 2)
       (eq (car (cdr (car (car tree)))) 0)))
