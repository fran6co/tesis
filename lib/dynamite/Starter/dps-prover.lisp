(let ((module-pathname 
       (merge-pathnames (make-pathname 
			 :directory '(:relative "Prover")) +dps-path+)))
  (require (namestring (merge-pathnames #p"match.lisp" module-pathname)))
  (require (namestring (merge-pathnames #p"logging.lisp" module-pathname)))
  (require (namestring (merge-pathnames #p"cmds-status.lisp" module-pathname))))