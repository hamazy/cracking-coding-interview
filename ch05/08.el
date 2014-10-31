(defun cci-draw-horizontal-line (screen width x1 x2 y )
  (let ((current-pixel (+ (* y width) x1))
        (dest-pixel (+ (* y width) x2)))
    (while (<= current-pixel dest-pixel)
      (let ((array-index (/ current-pixel 8))
            (current-bit (mod current-pixel 8)))
        (let ((current-element (aref screen array-index)))
          (aset screen array-index (logior current-element (lsh 1 (- 7 current-bit))))))
      (setq current-pixel (1+ current-pixel))))
  screen)

(cci-draw-horizontal-line [0 0 0 0 0 0 0 0] 8 0 3 0)
;; => [240 0 0 0 0 0 0 0]
;; == [#b11110000 #b00000000 ... #b00000000]

(cci-draw-horizontal-line [0 0 0 0 0 0 0 0] 16 0 9 0)
;; => [255 192 0 0 0 0 0 0]
;; == [#b11111111 #b11000000 0 ... 0]

(cci-draw-horizontal-line [0 0 0 0 0 0 0 0] 24 9 20 0)
;; => [0 127 248 0 0 0 0 0]
;; == [0 #b01111111 #11111000 0 ... 0]

(defun cci-display-screen (screen width)
  (let ((i 0)
        (length (length screen))
        (result))
    (while (< i length)
      (let ((elem (aref screen i)))
        (let ((str (cci-format-in-binary elem)))
          (setq result (concat result (concat (make-string (- 8 (length str)) ?0) str)))
          (when (= 0 (mod (* i 8) width)) (setq result (concat result "\n")))))
      (setq i (1+ i)))
    result))

(cci-display-screen (cci-draw-horizontal-line [0 0 0 0 0 0 0 0] 8 0 3 0) 8)
;; "11110000
;;  00000000
;;  00000000
;;  00000000
;;  00000000
;;  00000000
;;  00000000
;;  00000000
;; "

(cci-display-screen (cci-draw-horizontal-line [0 0 0 0 0 0 0 0] 16 0 3 0) 16)
"11110000
0000000000000000
0000000000000000
0000000000000000
00000000"
