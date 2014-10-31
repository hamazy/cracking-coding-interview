(defun cci-find-missing-number (a)
  (let ((n (1+ (length a)))
        (bit-length 0)
        (current-bit 0)
        (missing 0))
    (let ((q (/ n (lsh 1 (1+ current-bit))))
          (r (mod n (lsh 1 (1+ current-bit))))) ; 0 <= r < 2^{i + 1}
      (let ((rr (if (< (lsh 1 current-bit) r)
                    ;; if 2^i < r, formar 2^i bits are all 0s and
                    ;; latter r - 2^i bits are all 1s.
                    (- r (lsh 1 current-bit))
                  ;; if 0 < r <= 2^i, all bits are 0s.
                  0)))
        (while (< 0 (lsh n (* -1 current-bit)))
          (let ((expected-sum (+ (* q (lsh 1 current-bit)) rr)))
            ;; sum of i-t bits should be the expected value
            ;; (print (format "sum of %d-th bits should be %d" current-bit expected-sum))
            ;; (print (format "cause q = %d, r = %d, rr = %d" q r rr))
            (when (not (= (let ((sum 0)
                                (j 0))
                            (while (< j (length a))
                              (setq sum (+ sum (cci-array-bit-of-element a j current-bit)))
                              (setq j (1+ j)))
                            ;; (print (format "sum of %d-th bits is %d" current-bit sum))
                            sum)
                          expected-sum))
              ;; otherwise, i-th bit of missing number is 1.
              ;; (print (format "%d-th bit is 1" current-bit))
              (setq missing (logior missing (lsh 1 current-bit))))

            (setq current-bit (1+ current-bit))
            (setq q (/ n (lsh 1 (1+ current-bit))))
            (setq r (mod n (lsh 1 (1+ current-bit))))
            (setq rr (if (< (lsh 1 current-bit) r) (- r (lsh 1 current-bit)) 0))))))
    missing))

(defun cci-array-bit-of-element (a i j)
  "Return J-th bit value of I-th element in a given array A."
  (logand (lsh (aref a i) (* j -1)) 1))

(cci-find-missing-number [0 1 2 3 4 5 7 8 9])
;; => 6

(cci-find-missing-number [0 1 2 3 5 6 7 8 9 10 11 12 13])
;; => 4

(cci-find-missing-number [0 1 2 3 5 6 7 8 9])
;; => 4

(cci-find-missing-number
 [#b0000
  #b0001
  #b0010
  #b0011
; #b0100
  #b0101
  #b0110
  #b0111
  #b1000
  #b1001
  #b1010
  #b1011
  #b1100])
;; => 4

(cci-find-missing-number
 [#b0000
  #b0001
  #b0010
  #b0011
  #b0100
  #b0101
  #b0110
  #b0111
  #b1000
  #b1001
; #b1010
  #b1011
  #b1100
  #b1101])
;; 10
