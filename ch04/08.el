(defun cci-btree-identical (tree1 tree2)
  (cond ((and (not tree1) (not tree2)) t)
        ((or (not tree1) (not tree2)) nil)
        (t (and (cci-btree-identical (cci-node-left-tree tree1)
                                     (cci-node-left-tree tree2))
                (cci-btree-identical (cci-node-right-tree tree1)
                                     (cci-node-right-tree tree2))))))

(cci-btree-identical nil nil)
(cci-btree-identical (cci-node-create t) (cci-node-create t))
(not (cci-btree-identical (cci-node-create t) nil))
(not (cci-btree-identical nil (cci-node-create t)))

(defun cci-btree-containp (tree subtree)
  (if (cci-btree-identical tree subtree) t
    (or (cci-btree-containp (cci-node-left-tree tree) subtree)
        (cci-btree-containp (cci-node-right-tree tree) subtree))))

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1))
      (n2 (cci-node-create 2))
      (n3 (cci-node-create 3))
      (n4 (cci-node-create 4))
      (m0 (cci-node-create 0))
      (m1 (cci-node-create 1))
      (m2 (cci-node-create 2)))

  (cci-tree-set-left n0 n1)
  (cci-tree-set-right n0 n2)
  (cci-tree-set-left n1 n3)
  (cci-tree-set-right n1 n4)

  (cci-tree-set-left m0 m1)
  (cci-tree-set-right m0 m2)

  (cci-btree-containp n0 m0))
