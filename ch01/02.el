(defun cci-reverse-string (string)
  "Return reversed STRING."
  (if (= 0 (string-width string))
      string
    (let ((char (aref string 0))
	  (rest-string (substring string 1)))
      (concat (cci-reverse-string rest-string) (char-to-string char)))))

(cci-reverse-string "abc")
;; => "cba"
(cci-reverse-string "")
;; => ""
(cci-reverse-string "a")
;; => "a"
