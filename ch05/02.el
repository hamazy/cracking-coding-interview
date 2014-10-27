(defun cci-print-in-binary (r)
  (when (and (<= 0 r) (< r 1))
    (let ((result "."))
      (while (and (not (= r 0))
                  (<= (length result) 32))
        (let ((current-bit (if (<= 1 (* r 2)) 1 0)))
          (setq result (concat result (format "%d" current-bit)))
          (setq r (- (* r 2) current-bit))))
      (if (= r 0) result "ERROR"))))

(cci-print-in-binary 0.875)
;; => ".111"
