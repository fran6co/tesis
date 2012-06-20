;; carga los archivos emacs de DPS. 
;; Carga las rutinas ilisp para DPS. 
;; Muestra el DPS welcome buffer.

;; No quiero el "startup message" de emacs
(setq inhibit-startup-message t)

;; Declaro nombres de buffers.
(defconst DPSLOGBUFFERNAME "DPS Log")
(defconst DPSWELCOMEBUFFERNAME "DPS Welcome")


;; Declaro constantes de rutas de acceso ("path").
(if (getenv "DPSPATH")
    (defconst DPSPATH (if (string-match "/$" (getenv "DPSPATH"))
			(substring (getenv "DPSPATH") 0 -1)
			(getenv "DPSPATH")))
    (error "DPSPATH environment variable must be set"))

(if (getenv "ALLOYPATH")
    (defconst ALLOY (concat  
    	(if (string-match "/$" (getenv "ALLOYPATH"))
			(substring (getenv "ALLOYPATH") 0 -1)
			(getenv "ALLOYPATH")) "/alloy4.jar"))
    (error "ALLOYPATH environment variable must be set"))

;; Si no está definida la variable de entorno JAVAPATH, se asume que 
;; el ejecutable correspondiente a la máquina virtual de Java 6.0 o superior
;; está en el "path" del sistema.
(if (getenv "JAVAPATH")
	(defconst JAVA 
		(concat 
			(if (string-match "/$" (getenv "JAVAPATH"))
				(substring (getenv "JAVAPATH") 0 -1)
				(getenv "JAVAPATH")) 
			"/java"))
	(defconst JAVA "java"))

(defconst DYNAMITETRANSLATOR (concat DPSPATH "/lib/dynamite-translator.jar"))
(defconst +alloy-entry-point+ "alloy.cli.AlloyCLI")
(defconst +dynamite-translator-postulator-entry-point+ 
  "ar.uba.dc.dynamite.api.ExistencialCandidatePostulator")
(defconst +dynamite-translator-synthesized-formula-processor-entry-point+ 
  "ar.uba.dc.dynamite.api.SynthesizedFormulaProcessor")
(defconst +dynamite-translator-synthesized-expression-processor-entry-point+ 
  "ar.uba.dc.dynamite.api.SynthesizedExpressionProcessor")
(defconst +dynamite-translator-counterexample-vizualizator-entry-point+
  "ar.uba.dc.dynamite.api.VizGUI")
(defconst +dynamite-translator-specification-processor-entry-point+
  "ar.uba.dc.dynamite.api.SpecificationProcessor")
(defconst +dynamite-translator-synthesized-goal-processor-entry-point+
  "ar.uba.dc.dynamite.api.SynthesizedGoalProcessor")
(defconst +core-file-name+ "core")
(defconst +suggestions-buffer-name+ "*Use suggestions*")
(defconst +sugs-file-name+ "sugs")
(defconst +synthesized-goal-file-name+ "goal.als"
  "Nombre del archivo donde se escribe la sintetización de 'goals' como módulos.")

;; Creo un buffer para LOG
(defun dps-log-message (kind msg)
  (let ((buf (current-buffer)))
    (unwind-protect
	 (let* ((cpoint (point))
		(at-end (= cpoint (point-max))))
	   (set-buffer (get-buffer-create DPSLOGBUFFERNAME))
	   (goto-char (point-max))
	   (insert (format "%s(%s): %s\n"
		       kind
		     (substring (current-time-string) 4 19)
		     msg))
	   (unless at-end
	     (goto-char cpoint)))
      (set-buffer buf))))

(dps-log-message 'LOG "Connections between components.")
(dps-log-message 'LOG (concat "dps path: " DPSPATH))
(dps-log-message 'LOG (concat "alloy jar: " ALLOY))
(dps-log-message 'LOG (concat "(" +alloy-entry-point+ ")"))
(dps-log-message 'LOG (concat "jvm: " JAVA))
(dps-log-message 'LOG (concat "dynamite translator: " DYNAMITETRANSLATOR))
(dps-log-message 'LOG (concat "(proof-step: " +dynamite-translator-synthesized-formula-processor-entry-point+ ")"))
(dps-log-message 'LOG (concat "(sk-exp: " +dynamite-translator-synthesized-expression-processor-entry-point+ ")"))
(dps-log-message 'LOG (concat "(counterexample-vizGUI: " +dynamite-translator-counterexample-vizualizator-entry-point+ ")"))
(dps-log-message 'LOG (concat "(synthesized goal processor: " +dynamite-translator-synthesized-goal-processor-entry-point+ ")"))

(dps-log-message 'LOG "Started loading Emacs files")

(load (format "%s/Starter/dps-loader" DPSPATH) nil noninteractive)

(dps-log-message 'LOG "Emacs files loaded")

(defvar alloypvs-pp-activated nil)
(defvar *dps-actual-alloy-file* nil)
(defvar *dps-ready-to-prove* nil)

(defvar *alloy-file-extensions* '("als")
  "extensiones válidas para archivos Alloy.")



(defun dps-welcome (&optional display)
  (let ((cbuf (current-buffer))
	(buf (get-buffer-create "DPS Welcome"))
	(cdir pvs-current-directory)
	(vers (get-pvs-version-information))
	(cpoint (point-min)))
    (set-buffer buf)
    (setq fill-column (window-width))
    (if buffer-read-only (toggle-read-only))    
    (erase-buffer)
    (setq pvs-welcome-point (point))
    (insert "\n\n Alloy + PVS ")
    (insert "\n Son buenos por separado; juntos... ¡son dinamita! ")
    (put-text-property pvs-welcome-point (point) 'face 'blue)
    (setq pvs-welcome-point (point))
    (insert "\n\nYour current working directory is\n" cdir)
    (put-text-property pvs-welcome-point (point) 'face 'red)

    (setq pvs-welcome-point (point))
    (insert "\n\n Powered by ")
    (put-text-property pvs-welcome-point (point) 'face 'blue)

    (setq pvs-welcome-point (point))
    (insert "\n\n - PVS -")
    (put-text-property pvs-welcome-point (point) 'face 'green)

    (setq pvs-welcome-point (point))
    (insert "\n Version " (car vers))
    (insert "\n" (cadr (cdddr vers)) "\n" (cadr (cddddr vers)))
    (put-text-property pvs-welcome-point (point) 'face 'blue)

    (setq pvs-welcome-point (point))
    (insert "\n\n - Alloy -")
    (put-text-property pvs-welcome-point (point) 'face 'green)

    (setq pvs-welcome-point (point))
    (insert "\n Version 4.1.10")
    (put-text-property pvs-welcome-point (point) 'face 'blue)

    (setq pvs-welcome-point (point))
    (condition-case ()
	(center-region cpoint (point))
      (error nil))
    (set-buffer-modified-p nil)
    (text-mode)
    (cd cdir)
    (toggle-read-only)
    (goto-char (point-min))
    (if display
       (switch-to-buffer buf)
       (set-buffer cbuf))
    buf))

(defpvs dps environment ()
  "Starts the DPS process"
  (interactive)
      (unless noninteractive
	(message "Initializing DPS: please wait..."))
      (unless (equal (process-status (ilisp-process)) 'run)
	(switch-to-buffer "*pvs*")
	(error "Could not run PVS"))

      ;; Declaro constantes de rutas de acceso ("path") en el lisp interno.
      (pvs-send-and-wait
	 (format 
	  "(progn 
            (defconstant +dps-path+ #p\"%s/\" \"path de dps\")
            (defconstant +java+ \"%s\" \"JVM a utilizar (path global).\")
            (defconstant +alloy+ \"%s\" \"Jar de Alloy (path global).\")
            (defconstant +alloy-entry-point+ \"%s\" \"Clase de punto de entrada para Alloy.\")
            (defconstant +dynamite-translator+ \"%s\" \"Jar del traductor.\")
            (defconstant +dynamite-translator-synthesized-expression-processor-entry-point+ \"%s\" \"Clase de punto de entrada para el traductor de expresiones.\")
            (defconstant +dynamite-translator-synthesized-formula-processor-entry-point+ \"%s\" \"Clase de punto de entrada para el traductor de pasos de demostración.\")
            (defconstant +dynamite-translator-counterexample-vizualizator-entry-point+ \"%s\" \"Clase de punto de entrada para el visualizador de contraejemplos.\" )
            (defconstant +dynamite-translator-specification-processor-entry-point+ \"%s\" \"Clase de punto de entrada para el analizador de especificaciones.\")
            (defconstant +dynamite-translator-synthesized-goal-processor-entry-point+ \"%s\" \"Clase de punto de entrada para el analizador de secuentes sintetizados como especificaciones.\")
            (defconstant +dynamite-translator-postulator-entry-point+ \"%s\" \"Clase de punto de entrada para el postulador de candidatos para instanciaciones existenciales.\")
            (defconstant +core-file-name+ \"%s\" \"Nombre del archivo donde se almacenará el 'core' de los análisis.\")
            (defconstant +sugs-file-name+ \"%s\" \"Nombre del archivo donde se almacenará las sugerencias de los análisis de los secuentes sintetizados.\")
            (defconstant +synthesized-goal-file-name+ \"%s\" \"Nombre del archivo donde se almacenará la sintetización del goal actual.\")
            (defvar *dps-pp-activated* %s \"Indica si está activado el 'pretty printing' de las demostraciones.\"))"
	  DPSPATH JAVA ALLOY +alloy-entry-point+ DYNAMITETRANSLATOR +dynamite-translator-synthesized-expression-processor-entry-point+ +dynamite-translator-synthesized-formula-processor-entry-point+ +dynamite-translator-counterexample-vizualizator-entry-point+ +dynamite-translator-specification-processor-entry-point+ +dynamite-translator-synthesized-goal-processor-entry-point+ +dynamite-translator-postulator-entry-point+ +core-file-name+ +sugs-file-name+ +synthesized-goal-file-name+ "t")
	 nil nil 'dont-care)
      (unless noninteractive
	(message "DPS: constants loaded"))
      ; cargo las rutinas de dynamite para el demostrador (lisp interno).
      (pvs-send-and-wait
      	 (format "(progn (require \"%s/Starter/dps-starter.lisp\") )"  DPSPATH)
      	 nil nil 'dont-care)
      (unless noninteractive
	(message "DPS: internal lisp rutines loaded"))


      (unless noninteractive
	(pvs-auto-set-linelength (selected-frame))
	(dps-welcome (equal (buffer-name) "*scratch*")))
      (switch-to-buffer (get-buffer-create DPSWELCOMEBUFFERNAME))
      (unless noninteractive
	(message "Ready")) )

(dps)
