(defun cci-list-palindrome-p (list)
  (let ((original list)
	(reversed))
    (while list
      (setq reversed (cons (car list) reversed)
	    list (cdr list)))
    (let ((diff-found))
      (while (and original reversed (not diff-found))
	(setq diff-found (not (eq (car original) (car reversed)))
	      original (cdr original)
	      reversed (cdr reversed)))
      (not diff-found))))

(cci-list-palindrome-p '(0 1 2 1 0))
;; => t

(cci-list-palindrome-p '(0 1))
;; => nil
