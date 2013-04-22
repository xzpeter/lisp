(in-package :xzlib)

;;; 
;;; strings
;;; 
(defmacro strcat (&rest slist)
  "Join all the strings into one."
  `(concatenate 'string ,@slist))
(defun strstr (string sub)
  "Try to find string `sub' in `string'. Return the first index of the
`sub' found in `string', or NIL if cannot find any. "
  (let ((big (length string))
		(small (length sub)))
	(cond
	  ((< big small) nil)
	  ((= big small)
	   (if (string= string sub) 0 nil))
	  (t (loop for start from 0 upto (- big small)
			when (string= (subseq string start (+ start small)) sub)
			return start)))))
(defun split (string &optional (delimiter ""))
  "Split the `string' into list using the `delimiter'"
  (if (string= delimiter "")
	  (coerce string 'list)
	  (loop with result = nil and index = nil
		 do (flet ((cut (n)
					 (push (subseq string 0 n) result)
					 (setf string (subseq string n))))
			  (setf index (strstr string delimiter))
			  (if (null index)
				  (cut (length string))
				  (progn
					(cut index)
					(setf string (subseq string (length delimiter)))))
			  (when (string= string "") (return (nreverse result)))))))
(defun mkstr (&rest args)
  (with-output-to-string (s)
	(dolist (var args) (princ var s))))

;;; 
;;; sequences
;;; 
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
(defun in (element array)
  (find-if #'(lambda (x) (equal x element)) array))
;;; ((1) 2 (3 4 (5))) => (1 2 3 4 5)
(defun flattern (lst)
  (cond ((null lst) nil)
		((atom lst) (list lst))
		(t (nconc (flattern2 (car lst)) (flattern2 (cdr lst))))))

;;; 
;;; dates
;;; 
(defun leap-year-p (year)
  "return T if is leap year, or NIL"
  (flet ((div-ok (v) (= (mod year v) 0)))
	(or (div-ok 400) (and (div-ok 4) (not (div-ok 100))))))
(defun days-in-month (year month)
  "Return how many days in the month of the year"
  (+ (nth month (list 31 28 31 30 31 30 31 31 30 31 30 31))
	 (if (leap-year-p year) 1 0)))

;;;
;;; math
;;;
(defmacro loop-prime-numbers (var &body body)
  "loop over prime numbers which are smaller than limit"
  (let ((primes (gensym))
		(is-prime (gensym)))
	`(let ((,primes nil))
	   (flet ((,is-prime (x)
				(dolist (v ,primes t)
				  (when (= (mod x v) 0)
					(return nil)))) ) 
		 (loop for ,var from 2
			do (when (or (= ,var 2)
						 (and (not (= (mod ,var 2) 0))
							  (,is-prime ,var)))
				 (setf ,primes (cons ,var ,primes))
				 ,@body))))))
(defmacro loop-fibonacci-numbers (var &body body)
  "Loop over all the fibonacci numbers with limit"
  `(loop with ,var = 1 and big = 1
	  do (progn
		   ,@body
		   (shiftf ,var big (+ ,var big)))))


;;;
;;; misc
;;;
(defun make-cache-function (func)
  "This function will renew a function works just the same as `func' but
will do cache for same args. "
  (let ((cache (make-hash-table :test #'equal)))
	#'(lambda (&rest args)
		(multiple-value-bind (val cached) (gethash args cache)
		  (if cached
			  val
			  (setf (gethash args cache) (apply func args)))))))
