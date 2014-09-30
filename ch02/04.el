(defun cci-partition-list (list value)
  (let ((lows-last)
	(lows)
	(previous list)
	(current (cdr list)))
    (while current
      (if (< (car current) value)
	  (progn (setq lows (cons (car current) lows))
		 (when (not lows-last) (setq lows-last lows))
		 (setcdr previous (cdr current))
		 (setq current (cdr current)))
	(progn (setq previous current)
	       (setq current (cdr current)))))
    (if lows-last (progn (setcdr lows-last list)
			       lows)
      list)))

(cci-partition-list '(1 -2 3 -4) 0)
;; => (-4 -2 1 3)

(cci-partition-list '(-1 -2 -3 -4) 0)
;; => (-4 -3 -2 -1)

(cci-partition-list '(1 2 3 4) 0)
;; => (1 2 3 4)

(cci-partition-list () 0)
;; => nil

