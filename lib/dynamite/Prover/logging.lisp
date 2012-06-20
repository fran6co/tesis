(defconstant *user-log* "proof-logging.csv")

(defun qread (prompt)
  (format t "~%~a"  prompt)
  (force-output)
  (let ((input (ignore-errors (read))))
    (when *user-log* (with-open-file
     (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
     (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
      (format stream "~% ~2,'0d:~2,'0d:~2,'0d, ~d/~2,'0d/~d, ~a, ~a, "
	      hour
	      minute
	      second
	      month
	      date
	      year
	      (label *ps*)
	      (string-trim '(#\Newline) 
			   (if (stringp input) 
			       input 
			     (format nil "(~{~s~#[~:; ~]~})" input)))))))
    (cond ((and (consp input)
		(eq (car input) 'lisp))
	   (progn (when *user-log* (with-open-file
		   (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
		   (format stream "evaluating lisp expression")))
		  (format t "~%~s~%"  (eval (cadr input))))
	   (qread prompt))
	  ((member input '(quit q exit (quit)(exit)(q))
		   :test #'equal)
	   (if (pvs-y-or-n-p "~%Do you really want to quit?  ")
	       (progn (when *user-log* (with-open-file
		       (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
		       (format stream "quiting proof")))
		      (throw 'quit nil))
	     (qread prompt)))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
	       (progn (when *user-log* (with-open-file
		       (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
		       (format stream "aborting proof")))
		      (throw 'abort nil))
	     (qread prompt)))
	  ((eq input :reset) ;; from M-x reset-pvs 
	   (progn (when *user-log* (with-open-file
		   (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
		   (format stream "quiting proof (and reseting PVS)")))
		  (throw 'quit 'pvs-reset)))
	  (t (auto-save-proof)
	     (if (consp input)
		 (if (check-arguments input)
		     (progn (when *user-log* (with-open-file
			     (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
			     (format stream "applying proof command ")))
			    input)
		   (progn (when *user-log* (with-open-file
			   (stream *user-log* :direction :output :if-exists :append :if-does-not-exist :create)
			   (format stream "check-arguments failed... command not applyed")))
			  (qread prompt)))
	       input)))))
