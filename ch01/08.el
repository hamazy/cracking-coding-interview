(defun cci-string-rotationp (s1 s2)
  (if (= (string-width s1) (string-width s2))
      (string-match s1 (concat s2 s2))
    nil))

(cci-string-rotationp "waterbottle" "erbottlewat")

(cci-string-rotationp "waterbottle" "erbott")
;; => nil

(cci-string-rotationp "erbott" "waterbottle")
;; => nil
