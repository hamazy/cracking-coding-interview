(defun cci-make-occurrence-table (string table)
  "Return a hash TABLE that contains characters in STRING as keys
and the numbers of their occurrence as values."
  (if (= 0 (string-width string))
      table
    (let* ((char (aref string 0))
	   (rest-string (substring string 1))
	   (current-occurrence (gethash char table 0)))
      (progn
	(puthash char (1+ current-occurrence) table)
	(cci-make-occurrence-table rest-string table)))))

(defun cci-contains-chars-in-tablep (string table)
  "Return non-nil if all characters in STRING occurr as the same
number of times as defined in TABLE."
  (if (= 0 (string-width string))
      t
    (let* ((char (aref string 0))
	   (rest-string (substring string 1))
	   (current-occurrence (gethash char table 0)))
      (if (= 0 current-occurrence)
	  nil
	(progn
	  (puthash char (1- current-occurrence) table)
	  (cci-contains-chars-in-tablep rest-string table))))))

(defun cci-permutation-p (s1 s2)
  "Return non-nil if S1 is permutation of S2 or vice versa, and nil otherwise."
  (let (short long)
    (if (< (string-width s1) (string-width s2))
	(setq short s1 long s2)
      (setq short s2 long s1))
    (let ((table (cci-make-occurrence-table short (make-hash-table))))
    (cci-contains-chars-in-tablep long table))))

(cci-permutation-p "a" "aa")
;; => nil

(cci-permutation-p "a" "a")
;; => t

(cci-permutation-p "b" "a")
;; => nil

(cci-permutation-p "b" "ab")
;; => nil

(cci-permutation-p "ab" "a")
;; => nil

(cci-permutation-p "abcdef" "bceafd")
;; => t

(cci-permutation-p "abcdef" "bcead")
;; => nil

(cci-permutation-p "abcde" "bceafd")
;; => nil
