(let 
    ((module-pathname 
      (merge-pathnames (make-pathname 
       :directory '(:relative "UserInterface")) +dps-path+)))
  (require (namestring (merge-pathnames #p"DPSProverUI.commands.lisp" module-pathname)))
  (require (namestring (merge-pathnames #p"DPSProverUI.prettyprinter.lisp" module-pathname)))
  (require (namestring (merge-pathnames #p"DPSProverUI.feedbacker.lisp" module-pathname))))


