(defun type-restriction-from-formula (form)
  "Dada una fórmula que expresa las restricciones de dominio, tipo y multiplicidad, devuelve la fórmula que expresa la restricción de tipo."
  (let* ((application form)(operator (operator application)))
    (loop while (not (eq (id operator) '|Leq|)) do
      (progn (setq application (args1 application))
      (setq operator (operator application))))
    application))

(defun restrictors (restriction-formula)
  "Dada una fórmula que expresa las restricciones de dominio, tipo y muliplicidad, las devuelve como una lista de tres fórmulas (los 'restictores')."
  (de-conj restriction-formula))

(defun type-restriction-from-axiom (field-axiom)
  "Dada la definición de un axioma de campo, devuelve la fórmula que expresa su restricción de tipo."
  (let* ((formula (definition field-axiom))
	 (application (if (forall-expr? formula) (expression formula)
			formula)))
    (type-restriction-from-formula application)))

; Esta función encapsula la política de nombres que sigue la traducción.
(defun universal-constant-name (sig-name)
  "Dada un nombre de signatura, devuelve el nombre de la constante universal correspondiente."
  (format nil "univ_~a" sig-name))

(defun sig-name-from-iden (iden-name)
  "Dado un nombre de identidad parcial, devuelve el nombre de la signatura a la que corrsponde."
  (let ((iden (if (symbolp iden-name) (symbol-name iden-name) iden-name))) 
    (subseq iden 5))) ; 5 es la longitud de "iden_"

(defun univs-list-from-subtype (subtype)
  "Dada una instancia de 'subtype' devuelve una lista de (expresiones que denotan) universales (parciales o totales) que representan al producto cartesiano indicado por el 'subtype'."
  (let ((pred (predicate subtype)))
    (cond ((lambda-expr? pred) 
					; Si es una lambda-expr, debe tener la forma
					; LAMBDA (x: Carrier): RESTRICTIONS(x)
					; donde "x" representa al elemento cuyo "subtype" fue pasado
					; como parámetro y RESTRICTIONS al predicado
					; que expresa las restricciones de dominio, tipo
					; y multiplicidad que se aplican sobre el elemento "x".
					; Sólo me voy a quedar con el lado derecho de la
					; restricción de tipo en RESTRICTIONS.
	   (univs-list-from-expr (args2 (type-restriction-from-formula (expression pred)))))
	  ((name-expr? pred)
					; Si es una name-expr, tiene que ser el predicado característico 
					; de una signatura; cuyo nombre coincide con la signatura.
	   (list (pc-typecheck (pc-parse (universal-constant-name (id pred)) 'expr))))
	  (t 				; Esto no debería ocurrir.
	   (error "(univs-list-from-subtype) I don't know how to construct an univs-list from ~a" subtype)))))

(defun univs-list-from-expr (expr)
  "Devuelve la restricción de tipo de la expresión como una lista de expresiones, donde cada una de ellas es la universal de una signatura. \nEl parámetro 'expr' debe ser una expresión cerrada."
  (cond ((eq (type-of expr) 'name-expr)
					; Todas las constantes (de Skolem, campos, universales parciales, 
					; identidades parciales) son representadas como "name-expr".
	 (let* ((name-expr (pc-parse (id expr) 'name))
					; Pongo en "resolutions" las fórmulas y definiciones cuyo nombre 
					; coincide con la expresión que me dieron.
		(resolutions (formula-or-definition-resolutions name-expr))
					; Las separo según sean fórmulas o definiciones.
		(formula-resolutions 
		 (remove-if-not #'(lambda (res) (typep (declaration res) 'formula-decl)) resolutions))
					; La (sub) lista de definiciones puede tener a lo sumo un elemento:
					; todos los nombres definidos por el usuario en la especificación 
					; tienen un prefijo que contiene el símbolo "?" que no puede ser
					; usado para nombrar constantes de Skolem, y dentro del mismo módulo
					; los prefijos desambiguan los nombres de elementos que podrían repetirlos
					; (nombres de campos de signaturas distintas).
		(definition-resolution
		  (first (remove-if #'(lambda (res) (typep (declaration res) 'formula-decl)) resolutions))))
					; Si la definición es de una constante de Skolem, es una constante de Skolem :)
	   (if (eq (type-of (declaration definition-resolution)) 'skolem-const-decl)
	       (univs-list-from-subtype (type (declaration definition-resolution)))
					; Si hay fórmulas con el nombre de la expresión, debe ser el axioma
					; que impone las restricciones a los campos, por lo tanto es un campo.
					; (Acá estoy usando que los nombres de los campos, así como de sus
					; axiomas correspondientes son generados por el traductor con nombres
					; que no pueden ser escritos por el usuario -ya que contienen el
					; símbolo "?"-.)
	     (if (not (null formula-resolutions))
		 (univs-list-from-expr (args2 (type-restriction-from-axiom (declaration (first formula-resolutions)))))
					; Si no ocurre ninguna de las condiciones anteriores, sólo puede
					; tratarse de universales o identidades, parciales o totales, o del zero.
	       (let ((id-as-string (symbol-name (id expr)))) 
		 (if (eq (search "univ" id-as-string) 0) 
					; Si el nombre del símbolo empieza con "univ", es una universal.
		     (list expr)
		   (if (eq (search "iden" id-as-string) 0)
					; Si el nombre del símbolo empieza con "iden", es una identidad.
		       (if (string= id-as-string "iden")
			   (list (pc-typecheck (pc-parse "univ" 'expr)) (pc-typecheck (pc-parse "univ" 'expr))) 
			 (let ((partial-univ 
				(pc-typecheck (pc-parse 
					       (universal-constant-name 
						(sig-name-from-iden (id expr))) 'expr)))) 
			   (list partial-univ partial-univ))) 
					; Si no es una universal ni una identidad, es el zero (cuyo tipo es univ).
		     (list (pc-typecheck (pc-parse "univ" 'expr))))))))))
	((application? expr)
	 (let ((operator (id (operator expr))))
	   (case operator 
		 ((|RTC| |TC| |converse|)
					; Como son operaciones unarias (sobre relaciones binarias),
					; tienen el mismo tipo que sus argumentos.
		  (univs-list-from-expr (args1 expr)))
		 ((|sum| |Minus| |minus| |product|)
					; Como expr ya está "typechecked" tomo cualquiera 
					; de los dos argumentos (deben tener el mismo tipo).
		  (univs-list-from-expr (args1 expr)))
		 (|Navigation| 
					; Es la composición de sus argumentos.
		  (append (butlast (univs-list-from-expr (args1 expr))) (cdr (univs-list-from-expr (args2 expr)))))
		 ((|CartesianProduct| 

		   |CartesianProduct_Any_Lone|
		   |CartesianProduct_Any_One|
		   |CartesianProduct_Any_Some|

		   |CartesianProduct_Lone_Lone|
		   |CartesianProduct_Lone_One|
		   |CartesianProduct_Lone_Some|
		   |CartesianProduct_Lone_Any|

		   |CartesianProduct_Some_Any|
		   |CartesianProduct_Some_Lone|
		   |CartesianProduct_Some_One|
		   |CartesianProduct_Some_Some|

		   |CartesianProduct_One_Any|
		   |CartesianProduct_One_Lone|
		   |CartesianProduct_One_One|
		   |CartesianProduct_One_Some|)
					; Es el producto cartesiano de sus argumentos.
		  (append (univs-list-from-expr (args1 expr)) (univs-list-from-expr (args2 expr))))
		 (otherwise 
					; Esto no debería ocurrir.	  
		  (error "(univs-list-from-expr) I don't know how to combine the types of the arguments of ~a." expr)))))
	(t 
					; Esto no debería ocurrir.
	 (error "(univs-list-from-expr) I don't know how to build an univs-list from ~a." expr))))

(defun type-as-string (expr)
  (let ((*dps-pp-activated* nil) (list (univs-list-from-expr expr)))
    (format nil 
	    "~[~;~:;CartesianProduct(~]~{~A~#[~:;, ~:;, CartesianProduct(~]~}~{~A~}" 
	    (length list) list (loop for str in (cdr list) collect ")"))))

(defun add-type-match (in-alist &optional out-alist)
  (format t "~a~%" in-alist)
  (if (null in-alist)
      (nreverse out-alist)
    (let* ((var (caar in-alist))
	   (term (cdar in-alist))
	   (expected (when (slot-exists-p var 'type) (substit (type var) out-alist)))
	   (new-term (typecheck term :expected expected))
	   (new-out-alist (acons var new-term out-alist)))
      (if (some #'(lambda (fv) (not (memq (declaration fv) *bound-variables*)))
		(freevars expected))
	  (let* ((decl (pc-typecheck (pc-parse (type-as-string new-term) 'expr)))
		 (rests (restrictors (expression (predicate expected))))
					; Solo me quedo con los 'restrictors' que tienen variables libres.
		 (rests-with-fv (remove-if-not #'(lambda (form) (free-variables form)) rests))
		 (all-new-matches 
		  (loop for restrictor in rests-with-fv 
			collect (let* 
				    ((expr (args2 restrictor))
				     (new-matches (if expr (match expr decl nil nil) 'fail)))
				  (when (not (eq new-matches 'fail)) new-matches))))
		 (all-new-matches (remove-if #'null all-new-matches))
					; Aplano la lista de resultados.
		 (all-new-matches (reduce #'append all-new-matches)))
	    (if (not (consp all-new-matches))
		(add-type-match (cdr in-alist) new-out-alist)
	      (let* ((in-symbols (loop for (x . y) in in-alist collect (id x)))
		     (filtered-new-matches 
		      (remove-if #'(lambda (match) (memq (id (car match)) in-symbols)) all-new-matches)))
		(add-type-match (cdr in-alist)
				(append filtered-new-matches new-out-alist)))))
	(add-type-match (cdr in-alist) new-out-alist)))))

; Overloaded function.
(defun match (expr instance bind-alist subst)
  (let* ((*match-cache* (or *match-cache* ;;for initialization not shadowing
			    (make-hash-table :test #'eq)))
	 (hashed-table-expr
	  (unless *no-bound-variables-in-match*	;;NSH(10.19.95)
	    (gethash expr *match-cache*))) ;;should not cache or lookup
	 (hashed-value (when hashed-table-expr
			 (gethash instance hashed-table-expr)))
	 (hashed-result (when hashed-value (car hashed-value)))
	 (hashed-modsubst (when hashed-value (cdr hashed-value)))
	 (*dont-cache-match?* nil)
	 (*remaining-actuals-matches* nil))
    (cond ((and hashed-value
		(eq hashed-result 'fail))
	   'fail)
	  (hashed-value (setq *modsubst* hashed-modsubst)
			(if subst
			    (merge-subst subst hashed-result)
			    hashed-result))
	  (t (let* ((frees (freevars expr)) ;just to set no-freevars?
		    (*strict-matches* nil) ; SO 2003/7/15 - to control tc-match
		    (res (match* expr instance bind-alist subst))
					; En el código original (PVS4.2), si quedaban variables libres
					; se asginaba 'fail a "result"; en este caso, nosotros trataremos
					; de hallar nuevos "matches" para esas variables libres, utilizando,
					; por ejemplo, los axiomas que definen las restricciones de tipo
					; de los campos.
		    (extended-res 
		     (if (and (not (eq res 'fail))
			      (not (subsetp frees res 
		     			    :test #'(lambda (x y) (same-declaration x (car y))))))
			     (add-type-match res) res))
					; Si después de nuestra búsqueda aún quedan variables libres,
					; señalaremos el error.
		    (result
		     (if (or (eq extended-res 'fail)
			     (not (subsetp frees extended-res 
					   :test #'(lambda (x y) (same-declaration x (car y))))))
			 'fail
		       (match-remaining-actuals
			*remaining-actuals-matches* extended-res))))
	       (when (and (null subst)
			  (null *dont-cache-match?*)
			  (null *no-bound-variables-in-match*))
		 (if hashed-table-expr
		     (setf (gethash instance hashed-table-expr)
			   (cons result *modsubst*))
		     (setf (gethash expr *match-cache*)
			   (let ((hash (make-hash-table :test #'eq)))
			     (setf (gethash instance hash)
				   (cons result *modsubst*))
			     hash))))
	       result)))))
