(defun cci-tree-length (tree)
  (if tree
      (let ((right (cdr tree))
	    (left (car tree)))
	(let ((rlength (cci-tree-length right))
	      (llength (cci-tree-length left)))
	  (+ 1 (if (< llength rlength) rlength
		 llength))))
    0))

(cci-tree-length
 (cons (cons nil
	     (cons nil
		   nil))
       (cons (cons nil
		   nil)
	     nil)))

(defun cci-tree-balancedp (tree)
  (if tree
      (let ((left (car tree))
	    (right (cdr tree)))
	(let ((llength (cci-tree-length left))
	      (rlength (cci-tree-length right)))
	  (let ((diff (if (< llength rlength) (- rlength llength)
			(- llength rlength))))
	    (when (<= diff 1)
	      (and (cci-tree-balancedp left)
		   (cci-tree-balancedp right))))))
    t))

(cci-tree-balancedp nil)

(cci-tree-balancedp
 (cons nil nil))

(cci-tree-balancedp
 (cons nil
       (cons (cons nil
		   nil)
	     nil)))
