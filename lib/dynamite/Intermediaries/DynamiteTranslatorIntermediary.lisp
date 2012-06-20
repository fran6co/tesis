(defvar *counterexample-file-name*
  "counterexample.xml"
  "Nombre del archivo donde el analizador escribe los contraejemplos.")

(defvar *translation-log-file-name* 
  "dps-trans.log" 
  "Nombre del archivo utilizado para \"loggear\" las traducciones.")

(defvar *dt-log-file-name* 
  "dt.log" 
  "Nombre del archivo de \"log\" del Dynamite Translator.")

(defvar *global-scope*
  "3"
  "Valor del scope que se utilizará en las validaciones realizadas con el Alloy Analyzer.")

(defvar *assertion-file*
  "assertion.als"
  "Nombre del archivo para chequear assertions.")

(defun read-core-suggestions (sugs-file-name)
  "Lee las sugerencias que hace el core sobre la sintetización de un core. Devuelve una lista de números de fórmulas del secuente analizado (fnums). Si no hay sugerencias, devuelve nil."
  (let ((sugs-file (open sugs-file-name :if-does-not-exist nil))) 
    (when sugs-file
      (loop 
       for fnum = (read sugs-file nil) 
       while fnum 
       collect fnum 
       finally (close sugs-file)))))

(defun analyze-file (alloy-module-file-name)
  "Ejecuta el analizador Alloy sobre el archivo indicado. El traductor escribirá un archivo con información sobre el 'core' de ser posible."
  (run-translator-and-collect-result 
   (format nil "~A -cp ~A:~A ~A ~A -log \"~A\" -ce \"~A\" -core \"~A\" " 
     +java+ +dynamite-translator+ +alloy+
     +dynamite-translator-specification-processor-entry-point+
     alloy-module-file-name *dt-log-file-name* *counterexample-file-name* +core-file-name+ )))

(defun analyze-synthesized-goal (alloy-module-file-name)
  "Analiza el secuente sintetizado en la especificación Alloy ubicada en el archivo indicado. Se proveerán sugerencias en el archivo indicado."
  (run-translator-and-collect-result 
   (format nil "~A -cp ~A:~A ~A ~A -log \"~A\" -ce \"~A\" -sugs \"~A\" -core \"~A\"" 
     +java+ +dynamite-translator+ +alloy+
     +dynamite-translator-synthesized-goal-processor-entry-point+
     alloy-module-file-name *dt-log-file-name* *counterexample-file-name* +sugs-file-name+ +core-file-name+)))

(defun translate-synthesized-formula (synthesized-formula)
  "Traduce la fórmula resultante de la sintetización de una fórmula skolemizada DPS
a la fórmula pdocfa correspondiente."
  (run-translator-and-collect-result 
   (format nil "~A -cp ~A:~A ~A ~A.als \"~A\" -log \"~A\" " 
     +java+ +dynamite-translator+ +alloy+
     +dynamite-translator-synthesized-formula-processor-entry-point+
     *current-als-file-name* synthesized-formula *dt-log-file-name*)))

(defun validate-then-translate-synthesized-formula (synthesized-formula)
  "Traduce la fórmula resultante de la sintetización de una fórmula skolemizada DPS
a la fórmula pdocfa correspondiente, si no halla contraejemplos."
  (run-translator-and-collect-result 
   (format nil "~A -cp ~A:~A ~A ~A.als \"~A\" -v -for ~A -ce \"~A\" -log \"~A\" " 
     +java+ +dynamite-translator+ +alloy+
     +dynamite-translator-synthesized-formula-processor-entry-point+
     *current-als-file-name* synthesized-formula *global-scope* *counterexample-file-name* *dt-log-file-name* )))

(defun translate-synthesized-expression (synthesized-expression)
  "Traduce la expresión resultante de la sintetización de una lista de expresiones skolemizadas 
DPS a las expresiones pdocfa correspondientes (devuelve una lista)."
(let ((trans-result (run-translator-and-collect-result 
   (format nil "~A -cp ~A:~A ~A ~A.als \"~A\" -log \"~A\" " 
     +java+ +dynamite-translator+ +alloy+
     +dynamite-translator-synthesized-expression-processor-entry-point+
     *current-als-file-name* synthesized-expression *dt-log-file-name*))))  
  (cons (first trans-result) (butlast (split-by-one-newline (second trans-result))))))

(defun run-translator-and-collect-result (command-line-to-execute)
  "Ejecuta el DynamiteTranslator y procesa el resultado.
Los posibles resultados son:

'(:counterexample-found) cuando se halla un contraejemplo; 
'(:translation-error <mensaje de error> <tipo de error>) cuando se produce un error en la traducción; donde <tipo de error> puede ser :type (error de typecheck) :other (otro tipo de error)
'(:translation-ok <resultado de la traducción>) cuando la traducción se realiza correctamente.
"
  (multiple-value-bind 
   (out err pid)
   (excl:run-shell-command command-line-to-execute
			   :wait nil :output :stream :error-output :stream)
   (let((error-msg 
	 (with-output-to-string 
	   (err-str)
	   (with-open-stream 
	    (err err)
	    (do ((char (read-char err nil) (read-char err nil)))
		((null char))
		(princ char err-str)))))
	(result-msg 
	 (with-output-to-string 
	   (result)
	   (with-open-stream 
	    (out out)
	    (do ((char (read-char out nil) (read-char out nil)))
		((null char))
		(princ char result))))))
     (unless (null *translation-log-file-name*)
       (with-open-file
	(stream *translation-log-file-name* :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format stream "Traslating command:~%~A~@[~%~%Translation result:~%~A~]~@[~%~%Translation failed~%~A~]~%" 
		command-line-to-execute (if (string= "" result-msg) nil result-msg) (if (string= "" error-msg) nil error-msg))))
     (if (string= result-msg "")
	 (if (string= error-msg "")
	     (list ':counterexample-found)
	   (list ':translation-error error-msg
		 (cond((search "--TYPE ERROR--" error-msg) ':type)
		      (t ':other))))
       (when (string= error-msg "")
	 (list ':translation-ok result-msg))))))

(defun show-last-counterexample ()
  "Ejecuta el \"display\" de contraejemplos."
  (excl:run-shell-command 
   (format nil "~A -cp ~A:~A ~A \"~A\" " 
	   +java+ +dynamite-translator+ +alloy+
	   +dynamite-translator-counterexample-vizualizator-entry-point+ 
	   *counterexample-file-name*) :wait nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Postulador de candidatos existenciales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-postulator (validation-scope generation-scope max-rank terms elems ban)
  "Ejecuta una consulta al traductor, devuelve los streams de output y de error."
  (let((command-line-to-execute
	(format nil "~a -cp ~a:~a ~a ~a ~a -vs ~a -gs ~a -mr ~a ~@[-t ~a~] ~@[-e ~a~] ~@[~{-ban ~a ~}~]"
		+java+ +dynamite-translator+ +alloy+
		+dynamite-translator-postulator-entry-point+
		*postulation-filename* *postulation-assert-name*
		validation-scope generation-scope max-rank terms elems ban)))
  (unless (null *translation-log-file-name*)
       (with-open-file
	(stream *translation-log-file-name* :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format stream "Traslating command:~%~A~%" command-line-to-execute)))
  (multiple-value-bind
      (out-stream error-stream pid)
      (excl:run-shell-command command-line-to-execute
       :wait nil
       :output :stream
       :error-output :stream)
    (values out-stream error-stream))))

;; Información sobre el último candidato procesado
(defvar *last-candidate-is-one* nil)
(defvar *last-candidate-expr* nil)

(defun process-postulator-result (candidate-stream)
  "Procesa el resultado (output) de una corrida del Postulador"
  (let*((candidate-info-as-char-list
	 (with-open-stream 
	  (strm candidate-stream)
	  (loop while (setq char (read-char strm nil)) collect char)))
	(candidate-info(format nil "~{~A~}" candidate-info-as-char-list))
	(candidate-info(string-trim '(#\Space #\Newline) candidate-info)))
    (setq *last-candidate-is-one* (let((is-one(search "ONE" candidate-info)))(and is-one (= 0 is-one))))
    (setq *last-candidate-expr* nil)
    (cond (*last-candidate-is-one*
	    (setq *last-candidate-expr* (subseq candidate-info 4)))
	   ((let((is-some(search "SOME" candidate-info)))(and is-some (= 0 is-some)))
	    (setq *last-candidate-expr* (subseq candidate-info 5))))
    (values *last-candidate-expr*)))
