(defun translate-skolemized-formulas (skfmlas &optional validate?)
  (let((it-result-tag nil)(it-result-value nil))
    (loop for fml in skfmlas
	  do(let((translation-result
		  (cond((null validate?)
			(translate-skolemized-formula fml :without-validating))
		       (t (translate-skolemized-formula fml :validating)))))
	      (setq it-result-tag (first translation-result))
	      (setq it-result-value (second translation-result)))
	  if(equal it-result-tag :counterexample-found)
	  return(progn(show-last-counterexample) (error-format-if "Counterexample found for ~a. The formula is invalid.~%" fml))
	  else if(equal it-result-tag :translation-error)
	  return(error-format-if "There were problems in the translation of the formula \"~a\". ~%~a(See dt.log for details.)~%"
				 fml it-result-value)
	  else collect it-result-value)))

(defun translate-skolemized-formula (dps-formula &optional (validating 'hola))
  "Traduce a PDOCFA la f'ormula DPS pasada como par'ametro. Devuelve una lista con dos 
elementos: mensajes del traductor y el resultado. Si \"validating\" no es \":without-validating\" 
s'olo traduce la fÃ³rmula si no se encuentran contraejemplos."
  (case validating 
    (:without-validating 
        (translate-synthesized-formula (synthesize-formula dps-formula)))
    (t (validate-then-translate-synthesized-formula (synthesize-formula dps-formula)))))

(defun get-ordered-skolem-constants ()
  (let* ((sforms (s-forms (current-goal *ps*)))
	(skolem-constants
	  (collect-subterms sforms
	 		   #'skolem-constant?)))
    (sort skolem-constants
	       #'(lambda (x y)
		   (member (type x)
			   (loop for sb in (collect-subterms y #'skolem-constant?)
				 collect (type sb)))))))

(defun synthesize-formula (skolemized-formula)
  "Construye una f'ormula DPS sintetizada a partir de la f'ormula DPS 'skolemizada' 
y las constantes de Skolem disponibles en la demostraci'on actual."
  (let ((skoconstants (get-ordered-skolem-constants)))
    (if skoconstants 
	(format nil " { all ~{~A~#[~:;, ~]~} | let expr = ~A | expr } "
		(loop for sc in skoconstants
		      collect (format nil "~a: ~a" (pretty-print-name (string (id sc))) (type sc))) 
		skolemized-formula)
      (format nil " { let expr = ~A | expr } " skolemized-formula))))

(defun synthesize-expressions (skolemized-expressions-list)
  "Construye una f'ormula DPS sintetizada a partir de las expresiones DPS 'skolemizadas' 
y las constantes de Skolem disponibles en la demostraci'on actual."
  (let ((skoconstants (get-ordered-skolem-constants)))
    (if skoconstants 
	(format nil 
		" { all ~{~A~#[~:;, ~]~} | ~{let expr = ~A | expr + expr = expr~#[~:; && ~]~}} " 
		(loop for sc in skoconstants 
		      collect (format nil "~a: ~a" (pretty-print-name (string (id sc))) (type sc)))
		skolemized-expressions-list)
      (format nil " { ~{let expr = ~A | expr + expr = expr~#[~:; && ~]~}} " skolemized-expressions-list))))

(defun translate-skolemized-expressions (skolemized-expressions-list)
  "Traduce a PDOCFA las expresiones pasadas como parámetro. Recibe y devuelve listas."
  (translate-synthesized-expression (synthesize-expressions skolemized-expressions-list)))

(defun split-by-one-newline (string)
    "Returns a list of substrings of string divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty string between them.
Source: http://cl-cookbook.sourceforge.net/strings.html#process "
    (loop for i = 0 then (1+ j)
          as j = (position #\Newline string :start i)
          collect (subseq string i j)
        while j))

(defun new-sko-symbol (name context &optional counter symbols &key keep-underscore?)
  (unless counter (newcounter *skofun-counter*))
  (let* ((symb (if keep-underscore?
		   (concatenate 'string
		     (string name)
		      "_"
		     (princ-to-string (funcall *skofun-counter*)))
		   (gen-symbol name #\_ *skofun-counter*)))
	 (isymb (intern symb)))
    (if (or (declared? isymb context)
	    (member symb symbols :test #'same-id))
	(new-sko-symbol name context *skofun-counter* symbols
			:keep-underscore? keep-underscore?)
	symb)))

(defun skolem-rule-fun (&optional sformnum terms skolem-typepreds? dont-simplify?)
  #'(lambda (ps)
	(modified-skolem-step sformnum ps terms skolem-typepreds? dont-simplify?)))

(defun no-bangs (terms)
  "Devuelve T sólo si '!' no aparece en terms, que puede ser un string o una lista de strings."
  (let ((constants (if (listp terms) terms (list terms))))
    (loop for c in constants 
          never (find #\! c :test #'equal))))

(defun modified-skolem-step (sformnum ps &optional terms skolem-typepreds? dont-simplify?)
  (if (no-bangs terms)
      (skolem-step sformnum ps terms skolem-typepreds? dont-simplify?)
    (error-format-if "The symbol '!' is not allowed.")))

(defun de-conj (expr)
  "Dada una expresión que es una conjunción de cosas devuelve una lista con ellas"
  (if (conjunction? expr) 
      (append (de-conj (args1 expr)) (de-conj (args2 expr)))
    (list expr)))

(defun synthesize-skolem-constant-as-signature (skolem-constant)
  "Sintetiza la información de la constante de Skolem pasada como parámetro como definiciones de un módulo Alloy (signaturas y 'facts')."
  (let* ((type-as-string (format nil "~a" (type skolem-constant)))
	 (type-pred (predicate (type skolem-constant)))
	 (restrictions (if (lambda-expr? type-pred)
			   (if (conjunction? (expression type-pred))
			       (de-conj (expression type-pred))
			     (expression type-pred))
			 type-pred))
	 (type-expr (if (listp restrictions) (first restrictions) restrictions)))
    (cond
     ((name-expr? type-expr)
      (format nil "one sig ~a in ~a {}" (pretty-print-name (string (id skolem-constant))) 
	      (if (string= type-as-string "atom") "univ" type-as-string)))
					; Si es una lambda-expr tiene que tener la forma
					; LAMBDA (<var>: Carrier): <expr>
					; donde <expr> es Leq(<var>, <expr2>)
					; Primero me voy a fijar que <expr> tenga la forma que espero.
     ((and (application? type-expr) (eq (id (operator type-expr)) '|Leq|))
					; Si <expr2> es una name-expr, la constante de Skolem tiene aridad 1.
					; Si no, tiene aridad dos o mayor.
      (if (name-expr? (args2 type-expr))
	  (format nil "~asig ~a in ~a {}~a" 
	      (if(listp restrictions)
		  (cond ((loop for rest in restrictions 
			       thereis (and (application? rest)
					    (eq (id (get-pp-operator* (operator rest))) '|One|))) "one ")
			((loop for rest in restrictions 
			       thereis (and (application? rest)
					    (eq (id (get-pp-operator* (operator rest))) '|Lone|))) "lone ")
			((loop for rest in restrictions 
			       thereis (and (application? rest)
					    (eq (id (get-pp-operator* (operator rest))) '|Some|))) "some ")
			(t ""))
		"")
	      (pretty-print-name (string (id skolem-constant))) 
	      (if (string= type-as-string "atom") "univ" (args2 type-expr))
	      (if (and (listp restrictions) 
		       (and (not (eq (id (get-pp-operator* (operator (second restrictions)))) '|Some|))
			    (and (not (eq (id (get-pp-operator* (operator (second restrictions)))) '|One|))
				 (not (eq (id (get-pp-operator* (operator (second restrictions)))) '|Lone|)))))
		  (format nil "~%fact{ ~a in ~a }"
			  (pretty-print-name (string (id skolem-constant))) (args2 (second restrictions))) ""))
      (format nil "sig ~a'~a in ~a { ~a : ~a }~a" 
	 (pretty-print-name (string (id skolem-constant))) (random 1024) ; hay que tener mucha mala suerte para pegarle justo
	 (args1 (args2 type-expr))
	 (pretty-print-name (string (id skolem-constant))) 
	 (let((field-type (args2 (args2 type-expr))))
	   (if(application? field-type) field-type (format nil "set ~a" field-type)))
	 (if (and (listp restrictions) 
		       (and (not (eq (id (get-pp-operator* (operator (second restrictions)))) '|Some|))
			    (and (not (eq (id (get-pp-operator* (operator (second restrictions)))) '|One|))
				 (not (eq (id (get-pp-operator* (operator (second restrictions)))) '|Lone|)))))
		  (format nil "~%fact{ ~a in ~a }"
			  (pretty-print-name (string (id skolem-constant)))
			  (let ((*show-arrow-multiplicity-annotations* t))
			    (infer-dps-representation* (args2 (second restrictions)))))
		"")))))))

(defun synthesize-seq-forms-to-module (positive-forms negative-forms)
  "Sintetiza las fórmulas pasadas como parámetros en un módulo analizable por Alloy4. Este módulo contiene un único comando que busca una instancia de la especificación original que cumpla con las fórmulas positivie-forms y no cumpla con las negative-forms. Devuelve un String con el módulo Alloy resultante." 
  (format nil 
	  "open ~a~%~{~a~^~%~}~%run {} for ~a expect 0~%~{~a~^~%~}~%~%~{~a~^~%~}" 
					; El módulo de la especificación del usuario
	  *current-als-file-name*
					; Los 'facts' que representan las fórmulas que conservan su polaridad.
	  (loop for pform in positive-forms
		collect (format nil "fact { ~a }" pform))
					; El 'scope'.
	  *global-scope*
					; Los 'facts' que representan las fórmulas que deben negarse.
	  (loop for nform in negative-forms
		collect (format nil "fact { !~a }" nform))
					; Las declaraciones que resultan de sintetizar las constantes de Skolem.
	  (loop for sc in (get-ordered-skolem-constants) collect (synthesize-skolem-constant-as-signature sc))))

(defun synthesize-goal-to-module ()
  "Sintetiza el goal como un módulo analizable por Alloy4. Devuelve un String con el módulo Alloy resultante." 
  (synthesize-seq-forms-to-module 
   ;; select-seq sobre el antecedente devuelve fórmulas negadas
   (loop for sform in (select-seq (s-forms *goal*) '-)
	 collect (argument(formula sform)))
   (loop for sform in (select-seq (s-forms *goal*) '+)
	 collect (formula sform))))


(defun synthesize-goal-to-file (file-name)
  "Sintetiza el goal como un módulo analizable por Alloy4. Almacena el módulo Alloy resultante en un archivo, cuyo nombre debe pasarse como parámetro."
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
		  (format stream "~a" (synthesize-goal-to-module))))

(defun synthesize-seq-forms-to-file (file-name positive-forms negative-forms)
  "Sintetiza las fórmulas (como synthesize-seq-forms-to-module) en un archivo."
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
		  (format stream "~a" (synthesize-seq-forms-to-module positive-forms negative-forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build Checking Assertion Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-checking-assertion-module (assert-name)
  "Genera un módulo para chequear el assert cuyo nombre se recibe como parámetro." 
  (format nil 
	  "open ~a~%check ~a for ~a expect 0~%" 
					; El módulo de la especificación del usuario
	  *current-als-file-name*
					; El assert.
	  assert-name
					; El 'scope'.
	  *global-scope*
					; Los 'facts' que representan las fórmulas que deben negarse.
	  ))

(defun build-checking-assertion-file (file-name assertion-name)
  "Genera un módulo para chequear el assert cuyo nombre se recibe como parámetro."
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
		  (format stream "~a" (build-checking-assertion-module assertion-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build Candidate Postulation Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-candidate-postulation-file (file-name existencial-assertion positive-facts negative-facts)
  "Genera un módulo para postular un candidato para el assert existencial-assertion."
  (excl:run-shell-command (format nil "cp ~a.als ~a " *current-als-file-name* file-name))
  (with-open-file (stream file-name :direction :output :if-exists :append)
		  (format stream "~%~a" (build-candidate-postulation-module existencial-assertion positive-facts negative-facts))))

(defvar *postulation-assert-name* "existencialAssertion")

(defun build-candidate-postulation-module (existencial-assertion positive-facts negative-facts)
  "Sintetiza las fórmulas pasadas como parámetros en un módulo Alloy4, sobre el cual se puede ejecutar el postulador de candidatos existenciales. Devuelve un String con el módulo Alloy resultante."
  (setq *postulation-assert-name* (format nil "existencialAssertion~a" (random 1024)))
  (format nil 
	  "~{~a~^~%~}~%~{~a~^~%~}~%~%~{~a~^~%~}~%assert ~a {~a}~%" 
	  ;; 				; El módulo de la especificación del usuario
	  ;; *current-als-file-name*
					; Los 'facts' que representan las fórmulas que conservan su polaridad.
	  (loop for pform in positive-facts
		collect (format nil "fact { ~a }" pform))
					; Los 'facts' que representan las fórmulas que deben negarse.
	  (loop for nform in negative-facts
		collect (format nil "fact { !~a }" nform))
					; Las declaraciones que resultan de sintetizar las constantes de Skolem.
	  (loop for sc in (get-ordered-skolem-constants) collect (synthesize-skolem-constant-as-signature sc))
	  *postulation-assert-name* existencial-assertion))

(defvar *postulation-filename* "toPost.als")

(defun synthesize-candidate-postulation (fnum)
  ""
  (let*((ex-ass(formula(first(select-seq (s-forms *goal*) fnum))))
	(ex-ass(if (> 0 fnum) (argument ex-ass) ex-ass))
	(pos-facts(loop for sform in (select-seq (s-forms *goal*) '-)
			collect (argument(formula sform))))
	(pos-facts(set-difference pos-facts (list ex-ass)))
	(neg-facts(loop for sform in (select-seq (s-forms *goal*) '+)
			collect (formula sform)))
	(neg-facts(set-difference neg-facts (list ex-ass))))
  (build-candidate-postulation-file *postulation-filename* ex-ass pos-facts neg-facts)))
