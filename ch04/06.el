(defun cci-node-create (value)
  (plist-put nil 'value value))

(defun cci-node-value (node)
  (plist-get node 'value))

(defun cci-node-right-tree (tree)
  (plist-get tree 'right))

(defun cci-node-set-right-tree (tree subtree)
  (when subtree (plist-put subtree 'parent tree))
  (plist-put tree 'right subtree))

(defun cci-node-left-tree (tree)
  (plist-get tree 'left))

(defun cci-node-set-left-tree (tree subtree)
  (when subtree (plist-put subtree 'parent tree))
  (plist-put tree 'left subtree))

(defun cci-node-parent (node)
  (plist-get node 'parent))

(defun cci-bst-next (node)
  (let ((parent (cci-node-parent node))
        (my-right (cci-node-right-tree node)))
    (let ((parent-left (cci-node-left-tree parent))
          (grand-parent (cci-node-parent parent)))
      (let ((grand-parent-left (cci-node-left-tree grand-parent)))
        (if (equal node parent-left)
            ;; the node is parent's left sub-tree
            (if my-right (cci-btree-min my-right) parent)
          ;; the node is parent's right sub-tree
          (if my-right
              (cci-btree-min my-right)
            (if (equal parent grand-parent-left)
                grand-parent
              nil)))))))

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1))
      (n2 (cci-node-create 2))
      (n3 (cci-node-create 3))
      (n4 (cci-node-create 4))
      (n5 (cci-node-create 5))
      (n6 (cci-node-create 6)))
  (cci-node-set-left-tree n2 n1)
  (cci-node-set-right-tree n0 n2)
  (cci-node-set-left-tree n3 n0)
  (cci-node-set-right-tree n3 n4)
  (cci-node-set-right-tree n4 n5)
  (cci-node-set-right-tree n5 n6)
  (and (eq (cci-node-value (cci-bst-next n0)) 1)
       (eq (cci-node-value (cci-bst-next n1)) 2)
       (eq (cci-node-value (cci-bst-next n2)) 3)
       (eq (cci-node-value (cci-bst-next n4)) 5)
       (eq (cci-node-value (cci-bst-next n5)) 6)))
