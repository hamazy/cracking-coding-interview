;; naive implementation
(defun cci-char-in-stringp (char string)
  "Return non-nil if CHAR is in STRING and nil otherwise."
  (if (= (string-width string) 0)
      nil
    (let ((first (aref string 0)))
      (if (= first char)
	  t
	(cci-char-in-stringp char (substring string 1))))))

(defun cci-unique-charsp (string)
  "Return non-nil if give STRING has all unique characters and nil otherwise."
  (if (= 0 (string-width string))
      t
    (let ((char (aref string 0))
	  (rest-string (substring string 1)))
      (if (cci-char-in-stringp char rest-string)
	  nil
	(cci-unique-charsp rest-string)))))

;; using hash table. this should be faster.
(defun cci-unique-charsp-helper (string table)
  "Return non-nil if none of chars in STRING is in TABLE's
and all chars in STRING are unique."
  (if (= 0 (string-width string))
      t
    (let ((char (aref string 0))
	  (rest-string (substring string 1)))
      (if (gethash char table nil)
	  nil
	(progn
	  (puthash char t table)
	  (cci-uniquep-helper rest-string table))))))

(defun cci-unique-charsp (string)
  "Return non-nil if give STRING has all unique characters and nil otherwise."
  (if (= 0 (string-width string))
      t
    (cci-uniquep-helper string (make-hash-table))))

(cci-unique-charsp "abcde")
;; => t
(cci-unique-charsp "abcdea")
;; => nil
(cci-unique-charsp "")
;; => t
(cci-unique-charsp "abcdefga")
;; => nil
