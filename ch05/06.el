(defun cci-swap-even-odd-bits (n)
  (let ((even-mask #b010101010101010101010101010101)
        (odd-mask    #b1010101010101010101010101010))
    (logior (lsh (logand n even-mask) 1)
            (lsh (logand n odd-mask) -1))))

(cci-format-in-binary (cci-swap-even-odd-bits #b1111))
;; => "1111"

(cci-format-in-binary (cci-swap-even-odd-bits #b0101))
;; => "1010"

(cci-format-in-binary (cci-swap-even-odd-bits #b1001101))
;; => "10001110"
