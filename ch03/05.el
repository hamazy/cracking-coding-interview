(defun cci-queue-create ()
  (cons (cci-stack-create) (cci-stack-create)))

(defun cci-queue-enqueue (queue value)
  (let ((in (car queue))
	(out (cdr queue)))
    (cci-stack-push in value)))

(defun cci-queue-dequeue (queue)
  (let ((in (car queue))
	(out (cdr queue)))
    (when (cci-stack-emptyp out)
	(while (not (cci-stack-emptyp in))
	  (cci-stack-push out (cci-stack-pop in))))
    (cci-stack-pop out)))

(let ((queue (cci-queue-create)))
  (cci-queue-enqueue queue 0)
  (and (= (cci-queue-dequeue queue) 0)
       (not (cci-queue-dequeue queue))))

(let ((queue (cci-queue-create)))
  (cci-queue-enqueue queue 0)
  (cci-queue-enqueue queue 1)
  (cci-queue-enqueue queue 2)
  (and (= (cci-queue-dequeue queue) 0)
       (= (cci-queue-dequeue queue) 1)
       (= (cci-queue-dequeue queue) 2)
       (not (cci-queue-dequeue queue))))