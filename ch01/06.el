(defun cci-matrix-ref (matrix i j)
  (aref (aref matrix i) j))

(defun cci-matrix-set (matrix i j value)
  (aset (aref matrix i) j value))

(defun cci-rotate-matrix (matrix)
  "Rotate N x N MATRIX 90 degree in place."
  (let ((n (length matrix))
	(i 0)
	(j 0))
    (while (< i (/ n 2.0))
      (while (< j (/ n 2.0))
       (let ((tmp (cci-matrix-ref matrix i j)))
	(progn
	  (cci-matrix-set matrix i         j         (cci-matrix-ref matrix j         (- n i 1)))
	  (cci-matrix-set matrix j         (- n i 1) (cci-matrix-ref matrix (- n i 1) (- n j 1)))
	  (cci-matirx-set matrix (- n i 1) (- n j 1) (cci-matrix-ref matrix (- n j 1) i))
	  (cci-matirx-set matrix (- n j 1) i         tmp)))
       (setq j (1+ j)))
      (setq i (1+ i)))
    matrix))

(cci-rotate-matrix [[a]])
;; => [[a]]

(cci-rotate-matrix [[aa ab] [ba bb]])
;; => [[ab bb] [aa ba]]

(cci-rotate-matrix [[aa ab ac] [ba bb bc] [ca cb cc]])
;; => [[ac bc cc] [ab bb cb] [aa ba ca]]

(cci-rotate-matrix [[aa ab ac ad] [ba bb bc bd] [ca cb cc cd] [da db dc dd]])
;; => [[ad bd cd dd] [ac bc cc dc] [ab bb cb db] [aa ba ca da]]
