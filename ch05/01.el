(defun cci-bitwise-insert (m n i j)
  "Insert 32bit number M into N so that bit pattern of M starts
at j and ends at i in N."
  (logior (lsh m i)
          (logand n
                  (lognot (lsh (1- (lsh 1 (1+ (- j i)))) i)))))

(defun cci-format-in-binary (i)
  (when (integerp i)
    (let ((current i)
          (length 0)
          (l))
      (while (< 0 current)
        (setq l (cons (mod current 2) l))
        (setq length (1+ length))
        (setq current (lsh current -1)))
      (let ((a (make-string length ?0))
            (index 0))
        (while l
          (aset a index (if (= (car l) 0) ?0 ?1))
          (setq l (cdr l))
          (setq index (1+ index)))
        a))))

(cci-format-in-binary (cci-bitwise-insert #b10011 #b10000000000 2 6))
;; => "10001001100"
