(defun cci-next-largest (n)
  (let ((zero-run-length 0)
        (one-run-length 0)
        (shifted n))
    ;; count how many trailing 0s are there.
    (while (= (logand shifted 1) 0)
      (setq shifted (lsh shifted -1))
      (setq zero-run-length (1+ zero-run-length)))
    ;; count how many trailing 1s are there.
    (while (= (logand shifted 1) 1)
      (setq shifted (lsh shifted -1))
      (setq one-run-length (1+ one-run-length)))
    (when (< 0 one-run-length)
      ;; ex)
      ;; when n = 1101111000,
      ;; adder  =   10000000
      ;; mask   = 1110000000
      ;; ones   = 0000000111
      (let ((adder (lsh 1 (+ zero-run-length one-run-length)))
            (mask (lognot (1- (lsh 1 (+ zero-run-length one-run-length)))))
            (ones (1- (lsh 1 (1- one-run-length)))))
        (setq n (logior n adder))
        (setq n (logand n mask))
        (setq n (logior n ones))))))

(cci-format-in-binary (cci-next-largest #b1101111000))
;; => "1110000111"

(cci-format-in-binary (cci-next-largest #b11011111))
;; => "11101111"

(defun cci-previous-largest (n)
  (let ((zero-run-length 0)
        (one-run-length 0)
        (shifted n))
    (while (= (logand shifted 1) 1)
      (setq shifted (lsh shifted -1))
      (setq one-run-length (1+ one-run-length)))
    (while (= (logand shifted 1) 0)
      (setq shifted (lsh shifted -1))
      (setq zero-run-length (1+ zero-run-length)))
    (when (< 0 zero-run-length)
      ;; ex)
      ;; When n =      10000111,
      ;;   mask = 11..100000000
      ;;   ones =       1111000
      (let ((mask (lognot (1- (lsh 1 (+ zero-run-length one-run-length 1)))))
            (ones (lsh (1- (lsh 1 (1+ one-run-length))) (1- zero-run-length))))
        (setq n (logand n mask))
        (setq n (logior n ones))))))

(cci-format-in-binary (cci-previous-largest #b10000111))
;; => "1111000"

(cci-format-in-binary (cci-previous-largest #b1001110000111))
;; => "1001101111000"
