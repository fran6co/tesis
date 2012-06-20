;(require (format nil "~A/ilisp/.apvs-cmds.lisp" DPSPATH))
;(require (format nil "~A/ilisp/.apvs-pp.lisp" DPSPATH))

(let ((module-pathname 
       (merge-pathnames (make-pathname 
			 :directory '(:relative "Starter")) +dps-path+)))
  ;; Cargo el módulo intermediario.
  (require (namestring (merge-pathnames #p"dps-intermediaries.lisp" module-pathname)))
  ;; Cargo las rutinas de la interfaz de usuario.
  (require (namestring (merge-pathnames #p"dps-userinterface.lisp" module-pathname)))
  ;; Cargo las rutinas del demostrador.
  (require (namestring (merge-pathnames #p"dps-prover.lisp" module-pathname))))