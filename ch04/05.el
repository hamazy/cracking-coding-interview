(defun cci-tree-right-tree (tree)
  (when (car tree) (cadar tree)))

(defun cci-tree-left-tree (tree)
  (when (car tree) (caar tree)))

(defun cci-btree-max (tree)
  (when tree
    (let ((right-tree (cci-tree-right-tree tree)))
      (if right-tree (cci-btree-max right-tree)
	tree))))

(defun cci-btree-min (tree)
  (when tree
    (let ((left-tree (cci-tree-left-tree tree)))
      (if left-tree (cci-btree-min left-tree)
	tree))))

(defun cci-bstp (tree)
  "Return non-nil if a given binary TREE is a binary search tree or nil
otherwise."
  (if tree
      (let ((left-tree (cci-tree-left-tree tree))
	    (right-tree (cci-tree-right-tree tree))
	    (my-value (cci-node-value tree)))
	(let ((left-max (cci-node-value (cci-btree-max left-tree)))
	      (right-min (cci-node-value (cci-btree-min right-tree))))
	  (and (if left-tree (<= left-max my-value) t)
	       (if right-tree (<= my-value right-min) t)
	       (cci-bstp left-tree)
	       (cci-bstp right-tree))))
    t))

(let ((tree (cci-bst-create [0 1 2 3 4 5 6])))
  (cci-bstp tree))
