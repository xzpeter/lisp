(in-package :xzlib)

(defmacro strcat (&rest slist)
  `(concatenate 'string ,@slist))

;; (range) => nil
;; (range 10) => '(0 1 2 .. 9)
;; (range 3 8) => '(3 4 5 .. 7)
;; (range 2 10 3) => '(2 5 8)
(defun range (&optional (start 0 start-p) (end 0 end-p) (step 1))
  "Get a list of range from `start' to `end' with `step'"
  (cond ((null start-p) nil)
		((null end-p) (range 0 start))
		((= step 0) (error "step cannot be zero"))
		(t (loop for i from start below end by step
			  collect i))))
