(defvar *show-arrow-multiplicity-annotations*
    nil 
  "Si es true, se deben mostrar las anotaciones de las flechas al 'pretty-printear' las expresiones.")

(defmethod infer-dps-representation* :around ((number number-expr))
  (write number))

(defmethod infer-dps-representation* :around ((app application))
  "Infiere una expresión DPS desde una expresión PDOCFA."
  (let ((operator (get-pp-operator* (operator app))) )
    (format nil "(~A)" 
      (cond 
       ;; No debería ser necesaria la traducción de una "lambda expression"
       ;; ya que en DPS no hay manera (aún, al menos) de escribirlas.
       ((lambda-expr? operator) (error "infer-dps-representation* doesn't know how to pretty-print a lambda expression."))
       ;; Composición
       ((eq (id operator) '|composition|)
;;tuve que poner esto porque sino, no puedo hacer "grind", pues dicho comando imprime expresiones en las que aparece la composición
	(format nil "~A ; ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app)))
	)
       ;; Conjunción
       ((conjunction? app)
        ;; Si la conjunción tiene la forma:
        ;; Leq( <pdocfa-var> , <pdocfa-expr> ) and Atom(<pdocfa-var>)
        ;; debe ser inferida como
        ;; <pdocfa-var> : <pdocfa-expr>.
        (let ((left-operand (args1 app)) (right-operand (args2 app)))
          (cond
           ((and 
             ;; Si el operando izquierdo es un "Leq".
             (application? left-operand)
             (eq (id (get-pp-operator* (operator left-operand))) '|Leq|)
             ;; Si el operando derecho es la aplicación de un "Atom".
             (application? right-operand)
             (eq (id (get-pp-operator* (operator right-operand))) '|Atom|)
             ;; Si los dos operandos se refieren a la misma variable.
             (name-expr? (args1 left-operand))
             (name-expr? (args1 right-operand))
             (eq (id (args1 left-operand)) (id (args1 right-operand))))
            (format nil "~A : ~A" (infer-dps-representation* (args1 left-operand)) (infer-dps-representation* (args2 left-operand))))
           ;; o al revés
           ((and 
             ;; Si el operando derecho es un "Leq".
             (application? right-operand)
             (eq (id (get-pp-operator* (operator right-operand))) '|Leq|)
             ;; Si el operando izquierdo es la aplicación de un "Atom".
             (application? left-operand)
             (eq (id (get-pp-operator* (operator left-operand))) '|Atom|)
             ;; Si los dos operandos se refieren a la misma variable.
             (name-expr? (args1 left-operand))
             (name-expr? (args1 right-operand))
             (eq (id (args1 left-operand)) (id (args1 right-operand))))
            (format nil "~A : ~A" (infer-dps-representation* (args1 right-operand)) (infer-dps-representation* (args2 right-operand))))	       
           (t (format nil "~A && ~A" (infer-dps-representation* left-operand) (infer-dps-representation* right-operand))))))
       ;; Inclusión.
       ((eq (id operator) '|Leq|) 
        (format nil "~A in ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|NotLeq|) 
        (format nil "~A !in ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|NotEquals|) 
        (format nil "~A != ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Conjunción.
       ((eq (id operator) '|product|) 
        (format nil "~A & ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Producto cartesiano.
       ;; Si show-arrow-multiplicity-annotations es nil, muestro todos los productos cartesianos como "->".
       ;; Si no, los muestro con su respectiva multiplicidad.
       ((eq (id operator) '|CartesianProduct|) 
        (format nil "~A -> ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Any_Lone|) 
        (format nil "~A ~:[->~;->lone~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Any_One|) 
        (format nil "~A ~:[->~;->one~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Any_Some|) 
        (format nil "~A ~:[->~;->some~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Lone_Lone|) 
        (format nil "~A ~:[->~;lone->lone~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Lone_One|) 
        (format nil "~A ~:[->~;lone->one~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Lone_Some|) 
        (format nil "~A ~:[->~;lone->some~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Lone_Any|) 
        (format nil "~A ~:[->~;lone->any~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Some_Any|) 
        (format nil "~A ~:[->~;some->any~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Some_Lone|) 
        (format nil "~A ~:[->~;some->lone~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Some_One|) 
        (format nil "~A ~:[->~;some->one~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_Some_Some|) 
        (format nil "~A ~:[->~;some->some~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_One_Any|) 
        (format nil "~A ~:[->~;one->~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_One_Lone|) 
        (format nil "~A ~:[->~;one->lone~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_One_One|) 
        (format nil "~A ~:[->~;one->one~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|CartesianProduct_One_Some|) 
        (format nil "~A ~:[->~;one->some~] ~A"
          (infer-dps-representation* (args1 app))
          *show-arrow-multiplicity-annotations*
          (infer-dps-representation* (args2 app))))
       ;; Navegación.
       ((eq (id operator) '|Navigation|) 
        (format nil "~A . ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Suma.
       ((eq (id operator) '|sum|) 
        (format nil "~A + ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Resta.
       ((eq (id operator) '|Minus|) 
        (format nil "~A - ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|minus|) 
        (format nil "~A - ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|ranRest|) 
        (format nil "~A :> ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ((eq (id operator) '|domRest|) 
        (format nil "~A <: ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Implicación.
       ((implication? app) 
        (format nil "~A => ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Disyunción.
       ((disjunction? app) 
        (format nil "~A || ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Equivalencia lógica.
       ((eq (id operator) '|IFF|) 
        (format nil "~A <=> ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Igualdad.
       ((equality? app) 
        (format nil "~A = ~A" 
          (infer-dps-representation* (args1 app)) 
          (infer-dps-representation* (args2 app))))
       ;; Clausura reflexiva y transitiva.
       ((eq (id operator) '|RTC|) 
        (format nil "*~A" 
          (infer-dps-representation* (args1 app))))
       ;; Clausura transitiva.
       ((eq (id operator) '|TC|) 
        (format nil "^~A" 
          (infer-dps-representation* (args1 app))))
       ;; Conversa.
       ((eq (id operator) '|converse|) 
        (format nil "~~~A" 
          (infer-dps-representation* (args1 app))))
       ;; Predicado One.
       ((eq (id operator) '|One|) 
        (format nil "one ~A" 
          (infer-dps-representation* (args1 app))))
       ;; Predicado Lone.
       ((eq (id operator) '|Lone|) 
        (format nil "lone ~A" 
          (infer-dps-representation* (args1 app))))
       ;; Predicado Some.
       ((eq (id operator) '|Some|) 
        (format nil "some ~A" 
          (infer-dps-representation* (args1 app))))
       ;; Predicado None.
       ((eq (id operator) '|None|) 
        (format nil "no ~A" 
          (infer-dps-representation* (args1 app))))
       ;; Predicado "atom". Si estoy aplicando un predicado Atom,
       ;; estoy diciendo que la expresión a la que lo aplico
       ;; es un átomo de la universal
       ((eq (id operator) '|atom|) 
        (format nil "~A : univ" 
          (infer-dps-representation* (args1 app))))
       ;; Negación.
       ((negation? app) 
        (format nil "!~A" 
          (infer-dps-representation* (args1 app))))
       ;; Si no es ningún símbolo del álgebra, debe ser un símbolo definido por el usuario.
       (t (let ((operator-name (string (id (operator app))))
                (args (cond ((arg-tuple-expr? (argument app)) (exprs (argument app)))
                            (t (list (argument app)))))) 
            (format nil "~A[~{~A~^, ~}]" 
              (if (excl:match-regexp "\\w?\\w" operator-name) (pretty-print-name operator-name)
                (format nil "~A" operator-name))
              (loop for arg in args
                  if (not (and (name-expr? arg) (excl:match-regexp "\\w?\\w" (string (id arg)))))
                  collect (infer-dps-representation* arg)))))))))

(defmethod pp* :around ((app application))
  (if (not *dps-pp-activated*) (call-next-method)
    (write (infer-dps-representation* app))))


;; ----------------------------------------------------------- ;;
;; bind-decl
;; ----------------------------------------------------------- ;;

;; "prettyprinting" para las "bind-decl"
(defmethod pp* :around ((ex bind-decl))
  (if (not *dps-pp-activated*) (call-next-method)
    (write (infer-dps-representation* ex))))

(defmethod infer-dps-representation* :around ((pdocfa-decl bind-decl))
  (let*((dps-var (pretty-print-name (symbol-name (id pdocfa-decl))))
        (pdocfa-restriction (declared-type pdocfa-decl))
	(pped-type 
	 (cond 
	  ;; Si es una "expr-as-type", infiero su correspondiente en DPS.
	  ((expr-as-type? pdocfa-restriction) (infer-dps-representation* pdocfa-restriction))
	  ;; Si es una "type-name", infiero su correspondiente en DPS.
	  ((type-name? pdocfa-restriction) (infer-dps-representation* pdocfa-restriction))
	  ;; Si es un "setsubtype", debo estudiar el tipo de fórmula que define
	  ;; el rango de la variable.
	  ((setsubtype? pdocfa-restriction) (pp-multiplicity-restriction (formula pdocfa-restriction)))
	  (t (error (format nil "Unexpected type of declaration: \"~A\" of class ~a " pdocfa-restriction (class-of pdocfa-restriction)))))))
    (if (string= pped-type "Carrier")
	(format nil "~A" dps-var)
      (format nil "~A : ~A" dps-var pped-type))))

(defmethod pp* :around ((stype subtype))
  (if (not *dps-pp-activated*) (call-next-method)
    (write (infer-dps-representation* stype))))

(defun pp-multiplicity-restriction (formula)
  "Infiere la versión maquillada de la restricción de multiplicidad que denota la fórmula pasada como parámetro."
  ;; Sólo espero recibir cosas del estilo
  ;; { a : Carrier | 
  ;;     ( ( <restricción de tipo sobre a> 
  ;;     [ && <restricción de dominio sobre a> ] )
  ;;     [ && <restricciones de multiplicidad sobre a> ] ) ) }
  ;; Podría ocurrir que las restricciones de multiplicidad no estuvieran (cuando son triviales).
  ;; Pero si están y "a" no fue generada por un producto cartesiano
  ;; (en cuyo caso leo la multiplicidad de los productos cartesianos anotados del álgebra)
  ;; uso el predicado que denota la restricción de multiplicidad para
  ;; construir la versión DPS maquillada de la restricción.
  (let ((rest-part1 (if (conjunction? formula) 
			(if (conjunction? (args1 formula)) 
			    (args1 (args1 formula))
			  (args1 formula)) formula))
	(rest-part2 (when (conjunction? formula)
			(if (conjunction? (args1 formula))
			    (args2 (args1 formula))
			  (args2 formula))))
	(rest-part3 (when (conjunction? formula)
		      (when (conjunction? (args1 formula))
			(args2 formula))))) 
    (let ((old-showing-annotations *show-arrow-multiplicity-annotations*)
	(type-rest rest-part1)
	(domain-rest (if rest-part3 rest-part2 
		       (when (and rest-part2 (args2 rest-part2)) rest-part2)))
	(mult-rest (if rest-part3 rest-part3
		     (unless (and rest-part2 (args2 rest-part2)) rest-part2))))
    (setf *show-arrow-multiplicity-annotations* t)
    (let ((decl-expr (infer-dps-representation* 
		      (if (application? (if domain-rest domain-rest type-rest)) 
			  (args2 (if domain-rest domain-rest type-rest))
			(if domain-rest domain-rest type-rest)))))
      (setf *show-arrow-multiplicity-annotations* old-showing-annotations)
      (format nil (if (application? mult-rest)
		      (let ((operator (get-pp-operator* (operator mult-rest))))
			(cond ((eq (id operator) '|Lone|) (values "lone ~a"))
			      ((eq (id operator) '|One|) (values "one ~a")) 
			      ((eq (id operator) '|Some|) (values "some ~a"))
             ;; Si sólo es la aplicación es un "Atom", debo decir que es un átomo de la universal.
			      ((eq (id operator) '|atom|) (values "univ"))
			      (t (values "set ~a"))))
		    (values "set ~a")) decl-expr)))))

(defmethod infer-dps-representation* :around ((stype subtype))
  (let ((predicate (predicate stype)))
    (cond ((lambda-expr? predicate)
	   (let ((formula (expression predicate)))
	     (pp-multiplicity-restriction formula)))
	  (t (infer-dps-representation* predicate)))))


;; "prettyprinting" para las "binding-expr"
(defmethod pp* :around ((be binding-expr))
  (if (not *dps-pp-activated*) (call-next-method)
    (format t "~A" (infer-dps-representation* be))))

(defmethod infer-dps-representation* :around ((ex binding-expr))
  ;; Sólo espero recibir cuantificaciones que sean forall-expr y exists-expr.
  (format nil 
      (format nil 
          "(~A ~{~A~^, ~}| ~~A)" 
        (cond ((forall-expr? ex)(format nil "all"))
              ((exists-expr? ex)(format nil "some"))
              (t (error "infer-dps-representation* on binding-expr doesn't know how to infer a binding-expr of other type than forall-expr or exists-expr.")))
        (loop for bind in (bindings ex)
            collect (infer-dps-representation* bind)))
    (infer-dps-representation* (expression ex)))) 

;; Acá es donde se imprimen los tipos de las cuantificaciones de átomos.
(defmethod pp* :around ((ex expr-as-type))
  (if (eq *dps-pp-activated* t)
     (write (pretty-print-name (string (id (expr ex)))))
   (call-next-method)))

(defmethod pp* :around ((ne name-expr))
  (if (eq *dps-pp-activated* t)
      (let ((name (string (id ne))))
        (write (pretty-print-name name)))
    (call-next-method)))

(defun pretty-print-name (var-name)
  "Dado un nombre de variable relacional PDOCFA/PVS (pdocfa implementado en pvs) lo transforma en un nombre DPS.
[20080201] Un nombre de variable relacional PDOCFA/PVS cumple con los siguientes patrones:
   - si no contiene símbolos de interrogación, deben dejarse como están;
   - si los tiene al final, hay que traducirlos de la siguiente manera:
        dos signos representan un tilde ('), 
        tres signos representan comillas ;
   - si luego de realizar las modificaciones mencionadas anteriormente aparecen signos de interrogación en el  medio del nombre, el resultado debe ser el sufijo más grande que no contenga un signo de interrogación, a menos que sea algún 'iden', en cuyo caso hay que poner 'iden'. "
  (let ((pped-var-name (excl:replace-regexp (excl:replace-regexp (excl:replace-regexp var-name "????_" "$") "???_" "\"") "??_" "'")))
    (let ((separator-position (position #\? pped-var-name :from-end t)))
      (if (not separator-position) (values pped-var-name)
	(if (search "iden_" pped-var-name)
            (format nil "iden")
          (subseq pped-var-name (+ separator-position 1)))))))

(defun un-pretty-print-name (var-name)
  "Desmaquilla un nombre DPS en su correspondiente PDOCFAonPVS."
  (let ((pped-var-name (excl:replace-regexp (excl:replace-regexp (excl:replace-regexp var-name "\\$" "????_") "\\\"" "???_") "'" "??_"))) pped-var-name))

(defmethod infer-dps-representation* :around ((name name-expr))
  "Infiere una expresión DPS desde una expresión PDOCFA."
  ;; Sólo tengo que maquillar su nombre.
  (pretty-print-name (string (id name))))

(defmethod infer-dps-representation* :around ((expr expr-as-type))
  "Infiere una expresión DPS desde una expresión PDOCFA."
  (let ((expr-id (id (expr expr))))
    (cond 
     ;; Si sólo es un "atom", debo decir que es un átomo de la universal.
     ((eq expr-id '|atom|)
      (format nil "univ"))
     ;; En otro caso, sólo tengo que maquillar su nombre.
     (t (pretty-print-name (string expr-id))))))

(defmethod infer-dps-representation* :around ((name type-name))
  "Infiere una expresión DPS desde una expresión PDOCFA."
  ;; Sólo tengo que maquillar su nombre.
  (pretty-print-name (string (id name))))



;; Se reemplaza el impresor de proofstate, para que se maquille el "label".
(defmethod print-object ((ps proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*)))
	 (*pp-print-parens* *show-parens-in-proof*))
    (if *debugging-print-object*
	(call-next-method)
	(if (comment ps)
	    (format stream "~%~a : ~%~a~%~a"
	      (if *dps-pp-activated* (pretty-print-name (label ps)) (label ps))
	      (comment ps)
	      (current-goal ps))
	    (format stream "~%~a :  ~%~a"  (if *dps-pp-activated* (pretty-print-name (label ps)) (label ps))
		    (current-goal ps))))))

(defmethod print-object ((ps tcc-proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*)))
	 (*pp-print-parens* *show-parens-in-proof*))
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~%~a (TCC):   ~%~a"  (if *dps-pp-activated* (pretty-print-name (label ps)) (label ps))
	      (current-goal ps)))))

;; PP para declaraciones
(defmethod pp* :around ((decl auto-rewrite-decl))
  (if (not *dps-pp-activated*) (call-next-method)
    (with-slots (rewrite-names semi) decl
		(when (or (not *pretty-printing-decl-list*)
			  (not *pretty-printed-prefix*))
		  (when *pretty-printing-decl-list*
		    (setq *pretty-printed-prefix* t))
		  (write "//")
		  (write-char #\space)
		  (pp-rewrite-names rewrite-names)
		  (typecase decl
			    (auto-rewrite-plus-decl (write " is used as rewriting rule."))
			    (auto-rewrite-minus-decl (write " is marked for never be used as rewriting rule.")))
		  (write-char #\space))
		(when semi
		  (write-char #\;)))))

;; reemplaza el método pp* (module) en pp.lisp
(defmethod pp* ((mod module))
  (with-slots (id formals exporting assuming theory) mod
    (if *dps-pp-activated*
      (pprint-logical-block (nil nil)
        (write "// ####### Declarations from ")
	(write id)
	(write " ####### ")
	(pprint-newline :mandatory)
	(pp-theory (if *unparse-expanded*
		       theory
		     (remove-if #'generated-by theory))))
      (pprint-logical-block
       (nil nil)
       (write id)
       (pp-theory-formals formals)
       (write ": ")
       (pprint-indent :block 2)
       (write 'THEORY)
       (pprint-indent :block 1)
       (pprint-newline :mandatory)
       (pp* exporting)
       (write 'BEGIN)
       (pprint-indent :block 2)
       (pprint-newline :mandatory)
       (pp-assuming (if *unparse-expanded*
			assuming
		      (remove-if #'generated-by assuming)))
       (pp-theory (if *unparse-expanded*
		      theory
		    (remove-if #'generated-by theory)))
       (pprint-indent :block 1)
       (pprint-newline :mandatory)
       (write 'END)
       (write-char #\space)
       (write id)))))

(defun pp-theory (theory)
  (when theory
    (let ((*pretty-printing-decl-list* t)
	  (last-one (car (last theory))))
      (pprint-logical-block (nil (check-chained-syntax theory))
	(pprint-indent :block 0)
	(loop (let ((*pretty-printed-prefix* nil)
		    (decl (pprint-pop)))
		(if (typep decl '(or importing theory-abbreviation-decl))
		    (let ((imps (list decl)))
		      (loop while (chain? (car imps))
			    do (setq decl (pprint-pop))
			    do (push decl imps))
		      (pprint-logical-block (nil (nreverse imps))
			(if *dps-pp-activated*
			    (write "// Using declarations from ")
			  (write "IMPORTING"))
			(write #\space)
			(pprint-indent :current 0)
			(loop (pp* (pprint-pop))
			      (pprint-exit-if-list-exhausted)
			      (write-char #\,)
			      (write-char #\space)
			      (pprint-newline :fill))))
		    (pp* decl))
		(unless (or (chain? decl)
			    (eq decl last-one))
		  (unless *pp-compact*
		    (pprint-newline :mandatory))))
	      (pprint-exit-if-list-exhausted))))))

(defmethod pp* ((decl formula-decl))
  (if (not *dps-pp-activated*) 
      (with-slots (spelling definition) decl
		  (write spelling)
		  (pprint-indent :block 2)
		  (write-char #\space)
		  (pprint-newline :fill)
		  (pp* definition)
		  (pprint-indent :block 0))
    (with-slots (spelling definition id) decl
		(cond ((equal spelling 'AXIOM) (write "fact "))
		      ((equal spelling 'THEOREM) (write "assert "))
		      ((equal spelling 'OBLIGATION) (write "assert "))
		      (t (error "unknown symbol ~a" spelling)))
		(write id)
		(write " { ")
		(pprint-indent :block 2)
		(write-char #\space)
		(pprint-newline :fill)
		(pp* definition)
		(write " } ")
		(pprint-indent :block 0))))

(defmethod pp* :around ((decl declaration))
  (with-slots (id module formals chain? semi) decl
    (when (or *unparse-expanded*
	      *adt*
	      (not (generated-by decl)))
      (cond ((theory-abbreviation-decl? decl)
	     (call-next-method))

	    ((and chain?
		  *pretty-printing-decl-list*)
	     (write id)
	     (unless (typep decl '(or formal-decl adtdecl))
	       (write-char #\,)
	       (write-char #\space)
	       (pprint-newline :fill)))

	    (t (when (newline-comment decl)
		 (write (car (newline-comment decl)))
		 (write-char #\space)
		 (pprint-newline :mandatory))
	       (when (and *comment-on-proof-status*
			  (tcc? decl))
		 (format t "  % ~a~%" (proof-status-string decl)))
	       (cond 
		(*dps-pp-activated*
		 (pp-decl-formals formals))
		(t
		 (write id)
		 (pprint-indent :block 6)
		 (pp-decl-formals formals)
		 (write-char #\:)
		 (write-char #\space)))
	       (call-next-method)
	       (when semi (write-char #\;))
	       (pprint-indent :block 0))))))

;; Para imprimir los TCCs de una teoría hay que apagar siempre el maquillador.
(defun show-tccs (theoryref &optional arg)
  (let* ((theory (get-typechecked-theory theoryref))
	 (unproved-only? (and arg (not (minusp arg))))
	 (include-trivial? (and arg (minusp arg)))
	 (*no-comments* nil)
	 (*dps-pp-activated* nil))
    (when theory
      (let* ((*comment-on-proof-status* t)
	     (*no-comments* t)
	     (*unparse-expanded* t)
	     (*pp-new-projection-forms* t)
	     (unparsed-a-tcc? nil)
	     (str (string-trim
		   '(#\Space #\Tab #\Newline)
		   (with-output-to-string (out)
		     (dolist (decl (all-decls theory))
		       (dolist (cmt (cdr (assq decl (tcc-comments theory))))
			 (when (or include-trivial?
				   (not (eq (fourth cmt) 'in-context)))
			   (write (apply #'print-tcc-comment decl cmt)
				  :stream out :escape nil)
			   (terpri out) (terpri out)))
		       (when (and (tcc? decl)
				  (or (not unproved-only?)
				      (unproved? decl)))
			 (unparse decl :stream out)
			 (terpri out) (terpri out)
			 (setq unparsed-a-tcc? t))))))
	     (buffer (format nil "~a.tccs" (id theory))))
	(cond ((not (string= str ""))
	       (let ((*valid-id-check* nil))
		 (setf (tcc-form theory)
		       (if unparsed-a-tcc?
			   (parse :string str :nt 'theory-part)
			   str)))
	       (pvs-buffer buffer str t t))
	      (t (pvs-message "Theory ~a has no TCCs" theoryref)))))))

