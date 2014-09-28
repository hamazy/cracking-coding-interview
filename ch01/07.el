(defun cci-row-fill-zero (matrix row)
  (let ((j 0)
	(n (length (aref matrix 0))))
    (while (< j n)
      (cci-matrix-set matrix row j 0)
      (setq j (1+ j)))))

(defun cci-col-fill-zero (matrix col)
  (let ((i 0)
	(m (length matrix)))
    (while (< i m)
      (cci-matrix-set matrix i col 0)
      (setq i (1+ i)))))

(defun cci-matrix-find-fill-zero (matrix)
  (let ((m (length matrix))
	(n (length (aref matrix 0)))
	(i 0)
	(zero-containing-cols))
    (while (< i m)
      (let ((j 0)
	    (zero-found))
	(while (and (< j n) (not zero-found))
	  (when (= 0 (cci-matrix-ref matrix i j))
	    (setq zero-found t)
	    (setq zero-containing-cols (cons j zero-containing-cols)))
	  (setq j (1+ j)))
	(when zero-found (cci-row-fill-zero matrix i)))
      (setq i (1+ i)))
    (while zero-containing-cols
      (cci-col-fill-zero matrix (car zero-containing-cols))
      (setq zero-containing-cols (cdr zero-containing-cols)))
    matrix))

(cci-matrix-find-fill-zero [[1]])
;; => [[1]]

(cci-matrix-find-fill-zero [[0]])
;; => [[0]]

(cci-matrix-find-fill-zero [[0 1] [1 1]])
;; => [[0 0] [0 1]]

(cci-matrix-find-fill-zero [[1 0] [1 1]])
;; => [[0 0] [1 0]]

(cci-matrix-find-fill-zero [[1 1] [0 1]])
;; => [[0 1] [0 0]]

(cci-matrix-find-fill-zero
 [[1 1 1 1]
  [1 1 1 1]
  [1 1 1 0]])
;; => [[1 1 1 0] [1 1 1 0] [0 0 0 0]]
