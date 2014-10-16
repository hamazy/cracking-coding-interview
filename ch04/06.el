(defun cci-node-create (value)
  (plist-put nil 'value value))

(defun cci-node-value (node)
  (plist-get node 'value))

(defun cci-node-visitedp (node)
  (plist-get node 'visited))

(defun cci-node-visit (node)
  (plist-put node 'visited t))

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
            ;; if the current node is the root of parent's left
            ;; sub-tree, then the next node resides in the right
            ;; sub-tree of the current node. But if the current node
            ;; doesn't have a right sub-tree, the parent node is the
            ;; next one.
            (if my-right (cci-btree-min my-right) parent)
          ;; if the node is the root of parent's right sub-tree, then
          ;; the next node resides in the right sub-tree of the
          ;; current node.
          (if my-right
              (cci-btree-min my-right)
            ;; But if the right sub-tree is missing, the next is its
            ;; grand parent only when the parent node resides in the
            ;; left sub-tree of the grand parent. Otherwise, there's
            ;; no next node.
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

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1))
      (n2 (cci-node-create 2)))
  (cci-node-set-left-tree n2 n0)
  (cci-node-set-right-tree n0 n1)
  (and (cci-bstp n2)
       (eq (cci-node-value (cci-bst-next n0)) 1)
       (eq (cci-node-value (cci-bst-next n1)) 2)))

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1)))
  (cci-node-set-left-tree n1 n0)
  (and (cci-bstp n1)
       (eq (cci-node-value (cci-bst-next n0)) 1)))

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1))
      (n2 (cci-node-create 2)))
  (cci-node-set-right-tree n0 n1)
  (cci-node-set-right-tree n1 n2)
  (and (cci-bstp n0)
       (eq (cci-node-value (cci-bst-next n1)) 2)
       (not (cci-bst-next n2))))

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1)))
  (cci-node-set-right-tree n0 n1)
  (and (cci-bstp n0)
       (not (cci-bst-next n1))))
