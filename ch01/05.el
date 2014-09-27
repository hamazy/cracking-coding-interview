(defun cci-count-succeeding-chars (char string current)
  (if (= 0 (string-width string))
      current
    (if (= char (aref string 0))
	(cci-count-succeeding-chars char (substring string 1) (1+ current))
      current)))

(defun cci-compress-string (string)
  (if (>= 1 (string-width string))
      string
    (let* ((char (aref string 0))
	   (rest (substring string 1))
	   (succeeding (cci-count-succeeding-chars char rest 0)))
      (if (= 0 succeeding)
	  (concat (char-to-string char) (cci-compress-string rest))
	(concat (char-to-string char)
		(number-to-string (1+ succeeding))
		(cci-compress-string (substring rest succeeding)))))))

(cci-compress-string "aaaa")
;; => "a4"

(cci-compress-string "")
;; => ""

(cci-compress-string "a")
;; => "a"

(cci-compress-string "aa")
;; => "a2"

(cci-compress-string "aaabccddde")
;; => "a3bc2d3e"
