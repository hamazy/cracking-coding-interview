(defun cci-count-spaces (string count)
  "Return the number of white spaces in STRING."
  (if (= 0 (string-width string))
      count
    (if (= 32 (aref string 0))
	(cci-count-spaces (substring string 1) (1+ count))
      (cci-count-spaces (substring string 1) count))))

(defun cci-encode-spaces-recur (string length index num-spaces)
  (cond ((< index 0) string)
	((< length 0) string)
	((= num-spaces 0) string)
	(t (let ((last-char (aref string (1- length))))
	     (if (= last-char 32)
		 (progn (aset string index ?0)
			(aset string (- index 1) ?2)
			(aset string (- index 2) ?%)
			(cci-encode-spaces-recur string (1- length) (- index 3) (1- num-spaces)))
	       (progn
		 (aset string index last-char)
		 (cci-encode-spaces-recur string (1- length) (1- index) num-spaces)))))))

(defun cci-encode-spaces (string length)
  "Modify given STRING so that all white spaces in it are
replaced with %20."
  (cci-encode-spaces-recur string
			   length
			   (1- (string-width string))
			   (cci-count-spaces string 0)))

(cci-encode-spaces "a" 1)
;; => "a"

(cci-encode-spaces "ab" 2)
;; => "ab"

(cci-encode-spaces "" 0)
;; => ""

(cci-encode-spaces "a .." 2)
;; => "a%20"

(cci-encode-spaces "a b  c......" 6)
;; => "a%20b%20%20c"
