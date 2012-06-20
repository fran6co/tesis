(defvar current-alloy-file-name 'unbound
  "Nombre del archivo alloy cargado en el buffer activo.")
(make-local-variable 'current-alloy-file-name)

(defun spec-lemmas-theory-name (alloy-module-name)
  "Devuelve el nombre de la teoría pdocfa correspondiente a los lemas del módulo alloy pasado como parámetro."
  (concat alloy-module-name "Lemmas"))

(defun spec-theorems-theory-name (alloy-module-name)
  "Devuelve el nombre de la teoría pdocfa correspondiente a los teoremas del módulo alloy pasado como parámetro."
  (concat alloy-module-name "Theorems"))

(defun spec-axioms-theory-name (alloy-module-name)
  "Devuelve el nombre de la teoría pdocfa correspondiente a los axiomas del módulo alloy pasado como parámetro."
  (concat alloy-module-name "Axioms"))

(defun current-alloy-file-name (&optional no-error)
  "Devuelve el nombre del archivo alloy (sin la extensión) cargado en el buffer activo. 
Si la variable \"current-alloy-file-name\" no estaba cargada, la carga."
  (interactive)
  (if (and no-error
	   (not (eq current-alloy-file-name 'unbound)))
      current-alloy-file-name
      (pvs-current-directory)
      (cond ((or (not (buffer-file-name))
		 (not (member-equal (pathname-type (buffer-file-name))
				    *alloy-file-extensions*)))
	     (unless no-error
	       (error "%s is not a valid Alloy file" (buffer-name))))
	    
	    ((file-equal (buffer-file-name)
			 (format "%s%s"
			     pvs-current-directory
			   (file-name-nondirectory (buffer-file-name))))
	     (setq current-alloy-file-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
	    (t (unless no-error
		 (error "%s is not in the current context"
			(buffer-file-name)))))))

(defun x-prove-alloy-assert (alloy-assert-name)
  "Lanza la demostración del lema subyacente al assert indicado (si existe)."
  (interactive (complete-alloy-assert-name "Prove Alloy assertion named: "))
  (cond ((wish-possible-p)
	 (let ((pvs-x-show-proofs t))
	   (prove-alloy-assert alloy-assert-name)))
	(t (message "DISPLAY variable not set, cannot popup proof display")
	   (beep)
	   (sleep-for 1)
	   (prove-alloy-assert alloy-assert-name))))

(defun prove-alloy-assert (alloy-assert-name)
  "Lanza la demostración del lema subyacente al assert indicado (si existe)."
  (interactive (complete-alloy-assert-name "Prove Alloy assertion named: "))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let ((lemma-line (get-underlying-lemma-line 
		     alloy-assert-name 
		     (current-alloy-file-name))))
    (unless lemma-line
      (error "Couldn't find the corresponding theorem."))
    (save-some-pvs-buffers)
    (let((pvs-theory-name(spec-theorems-theory-name (current-alloy-file-name))))
      (pvs-send-and-wait 
       (format "(typecheck-file \"%s\" nil nil nil t)" pvs-theory-name)
       nil 'tc 'dont-care)
      ;; código extraído de pvs-prove-formula
      (let((rerun(pvs-send-and-wait
		  (format "(rerun-proof-at? \"%s\" nil %d \"pvs\")"
			  pvs-theory-name lemma-line)
		  nil nil "t\\|T\\|nil\\|NIL")))
	(if rerun
	    (ilisp-send
	     (format "(prove-file-at \"%s\" nil %d %s \"pvs\" \"%s\" 0 nil %s)"
		     pvs-theory-name 
		     lemma-line 
		     (when (member rerun '(t T)) t) 
		     (buffer-name)
		     pvs-x-show-proofs)
	     nil 'pr t)
	  (when (boundp 'pvs-error)
	    (setq pvs-error t)))))))

(defun step-proof-alloy-assert (alloy-assert-name)
  "Lanza la demostración del lema subyacente al assert indicado (si existe)."
  (interactive (complete-alloy-assert-name "Prove Alloy assertion named: "))
  (confirm-not-in-checker)
  (delete-other-windows)
  (let ((lemma-line (get-underlying-lemma-line 
		     alloy-assert-name 
		     (current-alloy-file-name))))
    (unless lemma-line
      (error "Couldn't find the corresponding theorem."))
    (save-some-pvs-buffers)
    (let((pvs-theory-name(spec-theorems-theory-name (current-alloy-file-name))))
      (pvs-send-and-wait 
       (format "(typecheck-file \"%s\" nil nil nil t)" pvs-theory-name)
       nil 'tc 'dont-care)
      (when (get-buffer "Proof") (kill-buffer "Proof"))
      (set-proof-script-font-lock-keywords)
      (pvs-send-and-wait
	   (format "(edit-proof-at \"%s\" nil %d \"pvs\" \"%s\" 0 nil)"
	       pvs-theory-name 
	       lemma-line
	       (buffer-name))
	   nil 'EditProof 'dont-care)
      (when (get-buffer "Proof")
	(pop-to-buffer (get-buffer "Proof"))
	(fix-edit-proof-comments)
	(setq buffer-modified-p nil)
	(goto-char (point-min))
	(pvs-prover-goto-next-step)
	(hilit-next-prover-command))
      (ilisp-send
       (format "(lisp (prove-file-at \"%s\" nil %d nil \"pvs\" \"%s\" 0 nil %s))"
	       pvs-theory-name 
	       lemma-line 
	       (buffer-name)
	       pvs-x-show-proofs)
       nil 'pr t))))

(defun x-step-proof-alloy-assert (alloy-assert-name)
  "Lanza la demostración del lema subyacente al assert indicado (si existe)."
  (interactive (complete-alloy-assert-name "Prove Alloy assertion named: "))
  (cond ((wish-possible-p)
	 (let ((pvs-x-show-proofs t))
	   (step-proof-alloy-assert alloy-assert-name)))
	(t (message "DISPLAY variable not set, cannot popup proof display")
	   (beep)
	   (sleep-for 1)
	   (step-proof-alloy-assert alloy-assert-name))))

(defun get-underlying-lemma-line (alloy-assert-name alloy-module-name)
  "Devuelve el número de línea donde se encuentra el lema correspondiente al assert indicado. Si no lo encuentra, devuelve nil.
- alloy-assert-name es el nombre del \"assert\" en cuestión.
- alloy-module-name es el nombre del módulo alloy en el que dicho \"assert\" se encuentra."
  (interactive "sAssert: \nsModule: \n")
  (let ((original-buffer (current-buffer)))
    (find-pvs-file (spec-theorems-theory-name alloy-module-name))
    (message (spec-theorems-theory-name alloy-module-name) )
    (goto-char 0)
    (let* ((search-re-string
	    (format "^\\s-+*%s\\s-+*:\\s-+*\\n*\\s-+*THEOREM" 
		    (regexp-quote alloy-assert-name)))
	   (point (re-search-forward search-re-string nil t)))
      (let ((result (if (null point) nil
		     (current-line-number))))
	    (switch-to-buffer original-buffer)
	    result))))

(defun complete-alloy-assert-name (prompt)
  (list 
   (completing-read prompt (collect-all-asserts) nil '1 nil)))

(defun collect-all-asserts ()
  (unless (string= (buffer-name) *dps-actual-alloy-file*) 
    (error "%s is not the current Alloy file." (buffer-name)))
  (when (not *dps-ready-to-prove*) (generate-pdocfa-theories))
  (save-excursion
    (goto-char 0)
    (let ((result '()))
      (while (re-search-forward "assert" nil t)
	(re-search-forward "[a-zA-Z]+[a-zA-Z0-9_]*" nil t)
	(setq result (append (list (match-string 0)) result)))
      result)))

;; Piso la función de pvs, para agregar el caso cuando se invocó una 
;; demostración desde un archivo Alloy.
(defun pvs-locate (output)
  (let* ((message (parse-pvs-message output))
	 (dir (car message))
	 (file (cadr message))
	 ;; si se invocó la demostración desde el als, quiero que vuelva donde estaba.
	 (pos (if (string= file *dps-actual-alloy-file*)
		  nil
		(pvs-get-place (caddr message)))))
    (pvs-display-file file dir pos)))
