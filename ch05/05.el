(defun cci-count-bits-to-convert (a b)
  (let ((difference (logxor a b))
        (count 0))
    (while (< 0 difference)
      (when (= (logand difference 1) 1)
        (setq count (1+ count)))
      (setq difference (lsh difference -1)))
    count))

(cci-count-bits-to-convert #b111 #b111)
;; => 0

(cci-count-bits-to-convert #b111 #b110)
;; => 1

(cci-count-bits-to-convert #b101 #b110)
;; => 2
