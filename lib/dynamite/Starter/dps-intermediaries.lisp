(defvar
  *current-als-file-name*
  nil
  "Nombre del archivo alloy que contiene al módulo
donde se encuentra la propiedad que se está 
demostrando en este momento.")

(let 
    ((module-pathname 
      (merge-pathnames (make-pathname 
       :directory '(:relative "Intermediaries")) +dps-path+)))
  ;; Cargo las rutinas que median con Alloy.
  (require (namestring (merge-pathnames #p"DynamiteTranslatorIntermediary.lisp" module-pathname)))
  ;; Cargo las rutinas que median con el traductor.
  (require (namestring (merge-pathnames #p"SkolemizedASTElementProcessor.lisp" module-pathname))))
