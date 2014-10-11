(defun cci-node-create (value)
  (cons nil (cons value nil)))

(defun cci-node-connect (from to)
  (if (car from)
      (setcar from (cons to (car from)))
    (setcar from (cons to nil))))

(defun cci-node-visit (node)
  (setcdr (cdr node) t))

(defun cci-node-visitedp (node)
  (cddr node))

(defun cci-node-adjacent-nodes (node)
  (car node))

(defun cci-node-connectedp (src dst)
  (cci-node-visit src)
  (let ((adjacents (cci-node-adjacent-nodes src))
	(found))
    (while (and adjacents (not found))
      (let ((node (car adjacents)))
	(if (eq node dst) (setq found t)
	  (when (not (cci-node-visitedp node))
	    (cci-node-visit node)
	    (setq found (cci-node-connectedp node dst)))))
      (setq adjacents (cdr adjacents)))
    found))

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1)))
  (cci-node-connect n0 n1)
  (cci-node-connectedp n0 n1))
;; => t

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1))
      (n2 (cci-node-create 2)))
  (cci-node-connect n0 n1)
  (cci-node-connect n0 n2)
  (cci-node-connectedp n1 n2))
;; => nil

(let ((n0 (cci-node-create 0))
      (n1 (cci-node-create 1))
      (n2 (cci-node-create 2))
      (n3 (cci-node-create 3)))
  (cci-node-connect n0 n1)
  (cci-node-connect n1 n2)
  (cci-node-connect n2 n0)
  (cci-node-connect n2 n3)
  (cci-node-connectedp n0 n3))
;; => t
