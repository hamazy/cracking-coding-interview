(defun cci-nth-last-element (list n)
  (let ((distance 0)
	(last list)
	(nth-last list))
    (while (and (cdr last) (< distance (1- n)))
      (setq last (cdr last))
      (setq distance (1+ distance)))
    (when (= distance (1- n))
      (while (cdr last)
	(setq last (cdr last))
	(setq nth-last (cdr nth-last)))
      (car nth-last))))

(cci-nth-last-element '(0 1 2 3 4) 1)
;; => 4

(cci-nth-last-element '(0 1 2 3 4) 2)
;; => 3

(cci-nth-last-element '(0 1 2 3 4) 5)
;; => 0

(cci-nth-last-element '(0 1 2 3 4) 6)
;; => nil

(cci-nth-last-element '(0 1 2 3 4) 0)
;; => nil

(cci-nth-last-element () 1)
;; => nil

(cci-nth-last-element '(0 1 2 3 4) 7)
;; => nil
