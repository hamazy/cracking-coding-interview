(defun cci-find-circle-beginning (list)
  (let ((current list)
	(set)
	(found))
  (while (and current (not found))
    (if (memq current set)
	(setq found t)
      (progn
	(push current set)
	(setq current (cdr current))))
    )
  (when found current)))


;; 0 -> 1 -> 2 -+
;; ^            |
;; +------------+
(let ((list
       (let ((beg (cons 0 nil)))
	 (setcdr beg (cons 1 (cons 2 beg)))
	 beg)))
  (let ((result (cci-find-circle-beginning list)))
    (and (eq result (nthcdr 0 list))
	 (not (eq result (nthcdr 1 list)))
	 (not (eq result (nthcdr 2 list)))
	 (eq result (nthcdr 3 list))
	 (not (eq result (nthcdr 4 list))))))
;; => t

;; 0 -> 1 -> 2 -+
;;      ^       |
;;      +-------+
(let ((list (let ((beg (cons 1 nil)))
	      (setcdr beg (cons 2 beg))
	      (cons 0 beg))))
  (let ((result (cci-find-circle-beginning list)))
    (and (not (eq result (nthcdr 0 list)))
	 (eq result (nthcdr 1 list))
	 (not (eq result (nthcdr 2 list)))
	 (eq result (nthcdr 3 list))
	 (not (eq result (nthcdr 4 list)))
	 (eq result (nthcdr 5 list)))))
;; => t
