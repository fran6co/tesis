;; Funciones auxiliares

(defvar *dps-one-auxiliar-file* t
  "Si es t, se usa el mismo archivo auxiliar para las búsquedas de instancias (vía AA). Si no, se usa uno nuevo para cada búsqueda.")

; Usado para debug
(defmacro other-filename (filename)
  `(format nil "~a.~a.als" ,filename (random 1024)))

(defmacro run-on-forms (positive-forms negative-forms spurious)
  `(let((goal-filename(if *dps-one-auxiliar-file* 
			  +synthesized-goal-file-name+ 
			(other-filename +synthesized-goal-file-name+))))
     (synthesize-seq-forms-to-file goal-filename ,positive-forms ,negative-forms)
     (let((analysis-result(first (analyze-file goal-filename))))
       (setq ,spurious (equal analysis-result :translation-ok))
       (when (equal analysis-result :counterexample-found)
	 (format t "New antecedent is not contradictory. An instance satisfying it was found.~%"))
       (when (equal analysis-result :translation-ok)
	 (format t "WARNING: No instance found in that scope. The new hypothesis could contradict the previous antecedent formulas.~%")))))


(defun ask-for-candidates (validation-scope generation-scope max-rank terms elems ban)
  ""
  (multiple-value-bind
      (out-stream error-stream)
      (run-postulator validation-scope generation-scope max-rank terms elems ban)
    (with-open-stream 
     (err error-stream)
     (do ((char (read-char err nil) (read-char err nil)))
	 ((null char))
       (format t "~a" char)))
    (process-postulator-result out-stream)))

(defstep dps-seminst (fnum &optional vs gs mr terms elems &rest ban)
  (let((vs(if vs vs *global-scope*))
       (gs(if gs gs 0))
       (mr(if mr mr 2))
       (form(select-seq (s-forms *goal*) fnum))
       (form(when form (formula(first form)))))
  (if (null form)
      (let((msg(format nil "~a is not a valid sequent formula number." fnum)))(skip-msg msg))
    (if (or (and (> 0 fnum) (not (forall-expr? form))) (and(< 0 fnum) (not (exists-expr? form))))
	(let((msg(format nil "The formula ~a is not a quantification with existencial strenght." form)))(skip-msg msg))
      (if (or (not (numberp mr)) (< mr 1))
	  (skip-msg "mr parameter has to be a number greater than zero.")
	(let((msg(when form (format t "Searching candidates for ~a with parameters~% validation scope: ~a~% generation scope: ~a~% maximum rank of terms: ~a~%~@[ maximum count of terms: ~a~%~]~@[ maximum count of semantic elements: ~a~%~]" form vs gs mr terms elems)))
	     (dummy(synthesize-candidate-postulation fnum)))
	  (if(ask-for-candidates vs gs mr terms elems ban)
	      (if *last-candidate-is-one*
		  (let((msg(format t "It is suggested to instantiate the quantifier with the expression ~a." *last-candidate-expr*))
		       (expr *last-candidate-expr*))
		    (inst fnum expr))
		(let((msg(format t "It is suggested to instantitate the quantifier with any atom included in ~a." *last-candidate-expr*))
		     (original-form(format nil "~a" (formula (first(select-seq (s-forms *goal*) fnum)))))
		     (some(search "some" original-form))(colon(search ":" original-form))(pipe(search "|" original-form))
		     (type(subseq original-form(+ colon 1)pipe))
		     (var(subseq original-form(+ some 4)colon))
		     (form(format nil "some ~a : ~a | ~a in ~a " var type var *last-candidate-expr*))
		     (old-sks (collect-skolem-constants)))
		  (then (case form)
			(skolem! -1)
			(let((new-sks(set-difference (collect-skolem-constants) old-sks))
			     (new-sk(when new-sks(id(first new-sks))))
			     (fnum (if (> 0 fnum) (+ 1 fnum) fnum)))
			  (if new-sk (inst fnum new-sk) (skip))))))
	    (skip-msg "No candidates found.")))))))
  "Sugerencias semánticas para instanciación de existenciales."
  "Applying suggestion")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Separación en casos validada con el Alloy Analyzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addrule 'dps-case nil (&rest formulas)
  (dps-case-rule-fun formulas)
  "Splits the proof in cases and uses the Alloy Analyzer to find models of each case.
 (dps-case a b c) on a sequent A |- B generates subgoals:
   a, b, c, A  |- B (search for models of a && b && c && A);
   a, b, A |- c, B  (search for models of a && b && A && !c);
   a, A |- b, B     (search for models of a && A && !b);
   A |- a, B        (search for models of A && !a).
 See also dps-case*")

(defun dps-case-rule-fun (fmlas)
  #'(lambda (ps)
      (if *dps-pp-activated*
	  (let((fmlas (if (listp fmlas) fmlas (list fmlas))))
	    (if (null fmlas)
		(progn (error-format-if "~%No formulas given.") (values 'X nil nil))
	      (let* ((tfmlas (translate-skolemized-formulas fmlas))
		     (tc-fmlas (loop for fml in tfmlas
				     collect (internal-pc-typecheck (pc-parse fml 'expr) :expected *boolean* :tccs 'all)))
		     (freevars (freevars tc-fmlas)))
		(cond ((null tc-fmlas)
		       ;; Hubo algún error en la traducción de Alloy a PDOCFA
		       (values 'X nil nil))
		      ((not (null freevars))
		       (error-format-if "~%Irrelevant free variables ~{~a, ~} occur in formulas." freevars)
		       (values 'X nil nil))
		      (t 
		       (format t "~%Case splitting on ~{~a~^, ~}" fmlas)
		       (multiple-value-bind 
			(subgoals dependent-decls) 
			(make-cases (current-goal ps) tc-fmlas nil)
			(format t "~%Checking spuriousness in the case separation with models of size at most ~a...~%" *global-scope*)
			(loop for current-subgoal in subgoals do (run-goal-as-case current-subgoal *goal*)
			      finally (return (values '? subgoals (list 'dependent-decls dependent-decls))))))))))
	(progn (error-format-if "~%Command available in alloy-like mode only.")
	       (values 'X nil nil)))))

(defun run-goal-as-case (current-goal old-goal)
  "Invoca el analizador sobre la conjunción entre las fórmulas del antecedente del subgoal y la negación de las fórmulas más nuevas en el consecuente. Se usa para chequear la separación en casos."
  (let*((positive-forms
	 (loop for sform in (select-seq (s-forms current-goal) '-)
	       collect (argument(formula sform))))
	(new-forms-nums(new-formula-nums current-goal old-goal))
	(negative-forms
	 (gather-seq(s-forms current-goal)
					; sólo quiero las que aparecen en el consecuente
					; las del precedente ya las agarré.
		    (loop for new-fn in new-forms-nums
			  when (> new-fn 0)
			  collect new-fn)
		    nil)))
    (format t "~%Analyzing case:~%~{ ~a~%~}~{ !~a~%~}" positive-forms negative-forms)
    (let((dummy nil))(run-on-forms positive-forms negative-forms dummy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-scope
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Asigna momentáneamente un scope dado para las búsquedas de instancias Alloy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addrule 'with-scope (scope step) nil
	 (apply-step
	  `(let ((old-scope *global-scope*)
		 (dummy(setq *global-scope* ,scope)))
	     (then (apply ,step) (let((dummy(setq *global-scope* old-scope)))(query*))))
	  :save? t)
  "Asigna momentáneamente un scope dado para las búsquedas de instancias Alloy."
  (format nil "Please note than the default scope (~a) was overrided for the previous analysis." *global-scope*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-case*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Versión repetitiva del comando dps-case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-case* (&rest formulas)
  (let ((old-goal *goal*)
	(msg(format t "~%Checking spuriousness in the case separation with models of size at most ~a...~%" *global-scope*)))
    (then (case* :formulas formulas) 
	  (let ((dummy (run-goal-as-case *goal* old-goal)))(skip))))
  "Complete version of DPS-CASE command where all the formulas are case split
along every branch."
  "Analyzed case-splitting fully on ~@{~% ~a, ~}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-hyp*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Versión repetitiva del comando dps-hyp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-hyp* (&rest formulas)
  (if (consp formulas)
      (let ((first (car formulas))
	    (rest (cdr formulas)))
	(then (dps-hyp first)
	      (dps-hyp*$ :formulas rest)))
      (skip))
  "This command perform a sucesive introduction of the formulas given as new hypothesis."
  "Introducing successively ~@{~%   ~a, ~} as new hypothesis.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-hyp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Introducción de hipótesis validada con el Alloy Analyzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addrule 'dps-hyp nil (&rest formulas)
  (dps-hyp-rule-fun formulas)
  "Splits according to the truth or falsity of the formulas in FORMULAS.
 (DPS-CASE a b c) on a sequent A |- B generates subgoals:
 a, b, c, A  |- B;
   a, b, A |- c, B;
   a, A |- b, B;
   A |- a, B.
 See also DPS-CASE*")

(defun dps-hyp-rule-fun (fmlas)
  #'(lambda (ps)
      (if *dps-pp-activated*
	  (let((fmlas (if (listp fmlas) fmlas (list fmlas))))
	    (if (null fmlas)
		(progn (error-format-if "~%No formulas given.") (values 'X nil nil))
	      (let* ((tfmlas(translate-skolemized-formulas fmlas))
		     (tc-fmlas (loop for fml in tfmlas
				     collect (internal-pc-typecheck (pc-parse fml 'expr) :expected *boolean* :tccs 'all)))
		     (freevars (freevars tc-fmlas)))
		(format t "~%Analyzing the introduction of hypothesis ~{~a~^, ~} on the sequent ~a with models of size at most ~a...~%" 
			fmlas (current-goal ps) *global-scope*)
		(cond 
		 ((null tc-fmlas)
		  ;; Hubo algún error en la traducción de Alloy a PDOCFA
		  (values 'X nil nil))
		 ((not (null freevars))
		  (error-format-if "~%Irrelevant free variables ~{~a, ~} occur in formulas." freevars)
		  (values 'X nil nil))
		 (t
		  (multiple-value-bind 
		   (subgoals dependent-decls) 
		   (make-cases (current-goal ps) tc-fmlas nil)
		   (let((subg-count(length subgoals))
			;; prev-spurious indica si el subgoal anterior representa 
			;; una introducción de casos espúrea
			;; (si lo es, hay que validar si el subgoal actual también 
			;; lo representa, a menos que sea el último).
			(prev-spurious nil))
		     (let((hypothesis(loop for sform in (select-seq (s-forms (first subgoals)) '-)
					   collect (argument(formula sform)))))
		       (format t "~%Analyzing new antecedent in main branch:~%~{ ~a~%~}" hypothesis)
		       (run-on-forms hypothesis nil prev-spurious))
		     ;(format t "~%Searching for counterexamples of the proof obligations...~%")
		     (loop for current-subgoal in (rest subgoals) 
			   for index from 2
			   for positive-forms = (loop for sform in (select-seq (s-forms current-subgoal) '-)
						      collect (argument(formula sform)))
			   for new-forms-nums = (new-formula-nums current-subgoal *goal*)
			   for negative-forms = (gather-seq (s-forms current-subgoal)
					; sólo quiero las que aparecen en el consecuente
					; las del precedente ya las agarré.
							    (loop for new-fn in new-forms-nums
								  when (> new-fn 0) collect new-fn) nil)

			   do (format t "~%Analyzing:~%~{ ~a~%~} =>~%~{  ~a~%~}" positive-forms negative-forms)
			   ;; Si el anterior fue espúreo, tengo que ver si también lo es este subgoal
			   ;; a menos que sea el último
			   if (< index subg-count)
 			   if prev-spurious do (run-on-forms positive-forms nil prev-spurious)
			   else do (format t "The antecedent is trivially not contradictory. The previous instance satisfies it.~%")
			   ;; Para todos los demás, busco contraejemplos de las
			   ;; implicaciones correspondientes a las demostraciones
			   ;; de la validez de las hipótesis nuevas.
			   do (progn 
				(synthesize-seq-forms-to-file +synthesized-goal-file-name+ positive-forms negative-forms)
				(let((analysis-result(first (analyze-file +synthesized-goal-file-name+))))
				  (when(equal :counterexample-found analysis-result)
				    (format t "Counterexample found.~%" )
				    (show-last-counterexample)
				    (return (values 'X nil nil)))
				  (when(equal :translation-ok analysis-result)
				    (format t "No counterexample found.~%"))))
			   finally (return (values '? subgoals (list 'dependent-decls dependent-decls)))))))))))
	(progn (error-format-if "~%Command available in alloy-like mode only.")
	       (values 'X nil nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CASE (con agregados para que funcione con fórmulas maquilladas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun case-rule-fun (fmlas)
  #'(lambda (ps)
      ;; Si se están mostrando los secuentes maquillados, tengo que "desmaquillar" las fórmulas que me pasaron
      (let((fmlas (if (listp fmlas) fmlas (list fmlas))))
	(if (null fmlas)
	    (progn (error-format-if "~%No formulas given.")
		   (values 'X nil nil))
	  (let*((fmlas (when *dps-pp-activated*
			  (translate-skolemized-formulas fmlas)))
		(tc-fmlas 
		 (loop for fml in fmlas
		       collect (internal-pc-typecheck (pc-parse fml 'expr)
						      :expected *boolean*
						      :tccs 'all)))
		(freevars (freevars tc-fmlas)))
	    (cond((null tc-fmlas)
		  ;; Hubo algún error en la traducción de Alloy a PDOCFA
		  (values 'X nil nil))
		 ((not (null freevars))
		  (error-format-if
		   "~%Irrelevant free variables ~{~a, ~} occur in formulas."
		   freevars)
		  (values 'X nil nil))
		 (t 
		  (multiple-value-bind
		   (subgoals dependent-decls)
		   (make-cases (current-goal ps) tc-fmlas nil)
		   (values '? subgoals (list 'dependent-decls dependent-decls))))))))))

(addrule 'case nil (&rest formulas)
  (case-rule-fun formulas)
  "Splits according to the truth or falsity of the formulas in FORMULAS.
 (CASE a b c) on a sequent A |- B generates subgoals:
 a, b, c, A  |- B;
   a, b, A |- c, B;
   a, A |- b, B;
   A |- a, B.
 See also CASE-REPLACE, CASE*"
  "~%Case splitting on ~@{~%   ~a, ~}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lemma
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modificado para que traduzca expresiones (en substituciones)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lemma-rule-fun (name &optional substs)
  #'(lambda (ps)
      (let((substs(if(and substs *dps-pp-activated*)
		      (let*((terms(loop for sub in substs for i from 0 
					if (oddp i) collect sub))
			    (translated-terms(translate-terms terms)))
			(loop for sub in substs for i from 0
			      if (oddp i) collect (nth (-(/(+ i 1)2)1) translated-terms)
			      else collect sub))
		    substs)))
	(lemma-step name substs ps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INST (con agregados para que funcione con expresiones maquilladas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro translate-terms (terms)
  `(let*((trans-result 
	  (translate-skolemized-expressions ,terms))
	 (translated-expressions (rest trans-result))
	 (result-tag (first trans-result))
	 (result-value (second trans-result)))
     (if(equal result-tag :translation-error)
	 (progn(error-format-if
		"There were problems in the translation of the formula.~%~a~%(See dt.log for details.)"
		result-value) nil)
       translated-expressions)))

;; función principal de instantiate (regla en la que se basan todos los comandos de instanciación).
;; Está modificada para traducir los términos con los que se realiza la instanciación.
(defun quant-rule-fun (sformnum  terms &optional copy?)
  #'(lambda (ps)
      (let* 
	  ((terms (if (listp terms) terms (list terms)))
	   (terms (if (null *dps-pp-activated*) terms
		    (translate-terms terms)))
	   (sformnum (find-quant sformnum terms ps)))
	(if (not terms)
	    (values 'X nil nil)
	  (quant-step sformnum ps terms copy?)))))

;; Esta función se modifica para "typecheckear" el resultado de la instanciación.
(defun quant-step (sformnum ps &optional terms copy?)
  (cond
   ((or (null sformnum)(null terms))
    (error-format-if "~%No suitable (+ve EXISTS/-ve FORALL) quantified formula found.")
    (values 'X nil nil))
   (t(let((*tccforms* nil)
	  (*dependent-decls* nil))
       (multiple-value-bind 
	(signal subgoal)
	(sequent-reduce
	 (current-goal ps)
	 #'(lambda (sform)
	     (quant-step-sform ps sform terms copy?))  
	 (list sformnum))
	(let((qsforms 
	      (select-seq 
	       (s-forms (current-goal ps))
	       (list sformnum)))
	     (other-sforms 
	      (delete-seq 
	       (s-forms (current-goal ps))
	       (list sformnum))))
	  (when (and (eq signal '?) (not copy?))
	    (loop for sf in qsforms
		  do (pushnew sf (hidden-s-forms subgoal)
			      :test #'tc-eq)))
	  (let* ((*tccforms* (remove-duplicates *tccforms*
						:test #'tc-eq
						:key #'tccinfo-formula))
		 (tccforms (assert-tccforms *tccforms* ps))
		 (instantiated-form(gather-seq (s-forms subgoal) (new-formula-nums subgoal *goal*) nil)))
	    (cond 
	     ((some #'(lambda (tccf)
			(tc-eq (tccinfo-formula tccf) *false*))
		    tccforms)
	      (values 'X nil))
	     ((and *dps-pp-activated*
		   (equal(first(translate-skolemized-formula instantiated-form :without-validating)) :translation-error))
	      (progn
		(error-format-if 
		 "~%~%The resulting formula ~a failed to be typechecked." instantiated-form)
		(values 'X nil)))
	     (t(let*((tcc-subgoals
		      (loop for tccinfo in tccforms
			    collect
			     (let((newgoal
				   (change-class
				   (copy subgoal 's-forms
					 (cons 
					  (make-instance 
					   's-formula :formula
					   (tccinfo-formula tccinfo))
					  other-sforms))
				   'tcc-sequent))
				 (references nil))
			      (setf (tcc newgoal)(tccinfo-formula tccinfo)
				    (reason newgoal)(tccinfo-reason tccinfo)
				    (expr newgoal)(tccinfo-expr tccinfo)
				    (kind newgoal)(tccinfo-kind tccinfo)
				    (type newgoal)(tccinfo-type tccinfo))
			      (list newgoal
				    'dependent-decls
				    (push-references-list
				     (tccinfo-formula tccinfo)
				     references))))))
		(values signal (cons (list subgoal
					   'dependent-decls
					   *dependent-decls*)
				     tcc-subgoals))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPAND (con agregados para que funcione con expresiones maquilladas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expand (name &optional sformnum occurrence if-simplifies
		    assert?)
  #'(lambda (ps)
      (expand-step 
       (cond ((string= name "!in") "NotLeq")
	     ((string= name "in") "Leq")
	     ((string= name "!=") "NotEquals")
	     (*dps-pp-activated* (format nil "this?~a" name))
	     (t name))
       ps sformnum occurrence if-simplifies
		   assert?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; typepred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Voy a renombrar la regla original (typepred) como pvs-typepred.
;;; Ahora typepred actúa de la siguiente manera: si se están mos-
;;; trando las fórmulas maquilladas, hace el typepred y luego assert;
;;; si no están maquilladas, sólo hace el typepred.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun typepred-fun (exprs all? &optional implicit?)
  #'(lambda (ps)
      (let*((exprs(if (listp exprs) exprs (list exprs)))
	    (exprs(if *dps-pp-activated*
		      (loop for skct in exprs collect (un-pretty-print-name skct))
		    exprs)))
	  (typepred-step exprs all? implicit? ps))))

(addrule 'pvs-typepred () (&rest exprs)
  (typepred-fun exprs nil)
  "Extract subtype constraints for EXPRS and add as antecedents.
 Note that subtype constraints are also automatically recorded by
the decision procedures. E.g.,
 (typepred \"abs(c)\"): Adds abs(c) > 0 to the antecedent."
  "~%Adding type constraints for ~@{ ~a,~}")

(addrule 'typepred () (&rest exprs)
 (apply-step
  (if *dps-pp-activated*
      `(then (pvs-typepred :exprs ,exprs) (assert))
    `(pvs-typepred :exprs ,exprs))
  :save? t)
  "Extract subtype constraints for EXPRS and add as antecedents.
 Note that subtype constraints are also automatically recorded by
the decision procedures. E.g.,
 (typepred \"abs(c)\"): Adds abs(c) > 0 to the antecedent."
  "~%Adding type constraints for ~@{ ~a,~}")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skolem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SKOLEM (con agregados para que funcione con expresiones maquilladas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addrule 'skolem  (fnum constants) (skolem-typepreds? dont-simplify?)
  (skolem-rule-fun fnum (if *dps-pp-activated* (loop for skct in constants collect (un-pretty-print-name skct)) constants) skolem-typepreds? dont-simplify?)
  "Replaces the universally quantified variables in FNUM with new
skolem constants in CONSTANTS.  If SKOLEM-TYPEPREDS? is T, then typepreds
will be generated for the introduced constants.  If DONT-SIMPLIFY? is T,
then the simplifications that occur automatically during substitution are
suppressed.  
Example: (skolem 1 (\"A\" \"B\"))
See also SKOLEM!, SKOSIMP, SKOSIMP*."
  "~%For the top quantifier in ~a, we introduce Skolem constants: ~2I~:_~a,")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flatten
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; También expande los operadores negados ("!in" y "!=")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep flatten (&rest fnums) 
  (if *dps-pp-activated* 
      (then (expand "!in")(expand "!=")(flatten-disjunct fnums nil))
    (flatten-disjunct fnums nil))
 "Disjunctively simplifies chosen formulas.  It simplifies 
top-level antecedent conjunctions, equivalences, and negations, and
succedent disjunctions, implications, and negations from the sequent."
 "Applying disjunctive simplification to flatten sequent")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-validate-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Valida la fórmula pasada como parámetro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-validate-form (possibly-skolemized-formula)
  (let((user-msg (format t "Trying to validate the formula with models of size ~A." *global-scope*))
       (translation-result (translate-skolemized-formula possibly-skolemized-formula :validating t))
       (result-tag (first translation-result)))
    (if (equal result-tag :counterexample-found) 
	(let ((dummy (show-last-counterexample)))(skip-msg "Counterexample found. The formula is invalid."))
      (if (equal result-tag :translation-error) (skip-msg "There were problems in the translation of the formula. (See dt.log for details.)")
	(skip-msg "No counter-example found in that scope. The formula may be valid."))))
  "Valida la fórmula pasada como parámetro con el analizador de Alloy."
  "Validating...")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-hide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trata de simplificar el goal actual usando el Alloy Analyzer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-hide (&optional (mode uc))
  (if (eq mode 'it)
      (let((user-msg (format t "Evaluating suggestions using models of size at most ~A.~%" *global-scope*))
	   (timer (get-internal-real-time)))
	(let((sugs (iteratively-prune-current-goal *global-scope*)))
	  (if sugs
	      (let((fnums-to-hide(first sugs))) 
		(try (hide-all-but fnums-to-hide)
		     (let ((user-msg (format t "Keeping formulas ~{~a ~}.~%Analysis statistics: ~a calls in ~f secs." 
				     fnums-to-hide (second sugs) (/ (- (get-internal-real-time) timer) internal-time-units-per-second))))
		       (skip))
		     (skip-msg "All formulas are suggested to be used in the proof.")))
	    (let ((user-msg (format t "There were problems in the translation of the goal. (See dt.log for details.)")))
	      (skip)))))
    (if (eq mode 'uc)
	(let ((user-msg (format t "Trying to prune the goal using UnSAT-cores of models of size at most ~A.~%" *global-scope*))
	      (dummy (synthesize-goal-to-file +synthesized-goal-file-name+))
	      (translation-result (analyze-synthesized-goal +synthesized-goal-file-name+))
	      (result-tag (first translation-result)))
	  (if (equal result-tag :counterexample-found) 
	      (let ((dummy (show-last-counterexample)))
		(skip-msg "Counterexample found. The goal is invalid."))
	    (if (equal result-tag :translation-error) 
		(skip-msg "There were problems in the translation of the goal. (See dt.log for details.)")
	      (let((user-msg (format t "No counter-example found in that scope. The goal may be valid.~%"))
		   (suggested-fnums (loop for fnum in (read-core-suggestions +sugs-file-name+) when (select-seq (s-forms *goal*) fnum) collect fnum))
		   (dummy (when (probe-file +core-file-name+)  (pvs-emacs-eval "(paint-suggestions)"))))
		(if suggested-fnums
		    (try (hide-all-but suggested-fnums)
			 (let((user-msg (format t "~%Hidding suggested formulas. (Hiding all but ~a.)~%" suggested-fnums)))(skip))
			 (skip-msg "All formulas are suggested to be used in the proof."))
		  (skip-msg "There are no suggestions."))))))
      (skip-msg "Error: MODE must be of the form it or uc.")))
  "Trata de simplificar el goal actual."
  "WARNING: evidence for usefulness could be found in larger domains")

(defun iteratively-prune-current-goal ()
  "Poda el secuente actual invocando iterativamente al AlloyAnalyzer. Devuelve una lista que contiene:
1) una lista con los números de fórmula que pueden ser ocultados, y
2) la cantidad de llamadas al analizador realizadas.
Si el resultado es nil, ocurrió un error en el proceso iterativo."
  (let((analyzer-calls-count 0) (keeped-fnums '()) (keep-ant-list '()) (keep-con-list '())
       (antecedents (select-seq (s-forms *goal*) '-))
       (consequents (select-seq (s-forms *goal*) '+)))
  ;; Primero trato de quitar antecedentes
    (when 
	(loop
	 for x on antecedents for i from -1 downto (- 0 (list-length antecedents))
	 finally (return 1) ; algo que no sea nil
	 do (let*((current-module
		   (synthesize-seq-forms-to-module (append keep-ant-list (rest x)) consequents))
		  (write-module
		   (with-open-file 
		    (stream +synthesized-goal-file-name+ :direction :output :if-exists :supersede)
		    (format stream "~a" current-module)))
		  (translation-result (analyze-file +synthesized-goal-file-name+))
		  (result-tag (first translation-result)))
	      (setf analyzer-calls-count (+ analyzer-calls-count 1))
	      (cond 
	       ((equal result-tag :translation-error)
		;; Si hubo un error en la validación, cancelo la operación.
		(return nil))
	       ((equal result-tag :counterexample-found)
		;; Si encontré un contraejemplo, tengo que dejar la fórmula que había sacado
		(setf keep-ant-list (append keep-ant-list (list (first x))))
		(setf keeped-fnums (append keeped-fnums (list i))))))) 
      ;; Ahora trato de quitar consecuentes
      (when 
	  (loop 
	   for x on consequents for i from 1 to (list-length consequents)
	   finally (return 1) ; algo que no sea nil
	   do(let*((current-module
		    (synthesize-seq-forms-to-module keep-ant-list (append keep-con-list (rest x))))
		   (write-module
		    (with-open-file 
		     (stream +synthesized-goal-file-name+ :direction :output :if-exists :supersede)
		     (format stream "~a" current-module)))
		   (translation-result (analyze-file +synthesized-goal-file-name+))
		   (result-tag (first translation-result)))
	       (setf analyzer-calls-count (+ analyzer-calls-count 1))
	       (cond 
		((equal result-tag :translation-error)
		 ;; Si hubo un error en la validación, cancelo la operación.
		 (return nil))
		((equal result-tag :counterexample-found)
		 ;; Si encontré un contraejemplo, tengo que dejar la fórmula que había sacado
		 (setf keep-con-list (append keep-con-list (list (first x))))
		 (setf keeped-fnums (append keeped-fnums (list i)))))))
	(list keeped-fnums analyzer-calls-count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-validate-goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Busca contraejemplos para el goal actual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-validate-goal ()
  (let ((user-msg (format t "Trying to validate the goal with models of size ~A." *global-scope*))
	(dummy (synthesize-goal-to-file +synthesized-goal-file-name+))
	(translation-result (analyze-file +synthesized-goal-file-name+))
	(result-tag (first translation-result)))
    (if (equal result-tag :counterexample-found) 
	(let ((dummy (show-last-counterexample)))
	  (skip-msg "Counterexample found. The goal is invalid."))
      (if (equal result-tag :translation-error) 
	  (skip-msg "There were problems in the translation of the goal. (See dt.log for details.)")
	(if (probe-file +core-file-name+)
	    (let((msg (format nil "No counter-example found in that scope. The goal may be valid. ~%(There are available suggestions. Use M-x show-suggestions to see them.)"))
		 (dummy (pvs-emacs-eval "(paint-suggestions)")))
		(skip-msg msg))
	  (skip-msg "No counter-example found in that scope. The goal may be valid.")))))
  "Busca contraejemplos para el goal actual"
  "Validating current goal...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-hide-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Esconde una fórmula y busca contraejemplos del secuente resultante.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-hide-form (fnum)
  (try (let ((user-msg (format t "Trying to prune the goal using models of size ~A.~%" *global-scope*))) (hide fnum))
       (let ((goal-as-formula (goal-to-form))
	     (translation-result (translate-skolemized-formula goal-as-formula :validating t))
	     (result-tag (first translation-result)))
	 (if (equal result-tag :counterexample-found) 
	     (let ((dummy (show-last-counterexample))
		   (usr-msg (format t "Keeping the formula (it should be used in the proof).")))
	       (fail))
	   (if (equal result-tag :translation-error) 
	       (let ((user-msg (format t "There were problems in the translation of the goal. (See dt.log for details.)")))
		 (fail))
	     (let ((user-msg (format t "Hidding the formula (it may not be useful).")))
	       (skip)))))
       (skip))
  "Esconde una fórmula y busca contraejemplos del secuente resultante."
  "WARNING: evidence for usefulness could be found in larger domains")

(defun goal-to-form ()
  "devuelve el goal actual en forma de fórmula"
  (let ((antecedents (loop for seq in (select-seq (s-forms *goal*) '-) collect (formula seq)))
	(consequents (select-seq (s-forms *goal*) '+)))
    (cond ((null antecedents)(format nil "~{~A ~^|~^| ~}" consequents))
	  ((null consequents)(format nil "~{~A ~^|~^| ~}" antecedents))
          (t (format nil "~{~A ~^|~^| ~} || ~{~A ~^|~^| ~}" antecedents consequents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intrans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transitividad del "in".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep intrans (alpha beta gamma)
  (try
  (apply (then
   (label "alpha" alpha)(label "beta" beta)(label "gamma" gamma)
   (hide-all-but ("alpha" "beta" "gamma"))
   (disable-alloylike)
   (expand "Leq" "alpha")(expand "Leq" "beta")(expand "Leq" "gamma")
   (enable-alloylike)
   (replace "beta" "gamma" :dir rl :hide? t)
   (use "BA_5")
   (replace -1 "gamma" :hide? t)
   (replace "alpha" "gamma" :hide? t)))(fail)(skip))
  "Aplica la transitividad de la inclusión."
  "Applying transitivity of inclusion operator")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enable-alloylike
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addrule 'enable-alloylike () ()
	 (apply-step
	  '(let ((dummy (setq *dps-pp-activated* t))) (query*))
	  :save? t)
  "Muestra las demostraciones en formato 'alloy'"
  "Enabling alloy like pretty printing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; disable-alloylike
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addrule 'disable-alloylike () ()
	 (apply-step
	  '(let ((dummy (setq *dps-pp-activated* nil))) (query*))
	  :save? t)
  "Muestra las demostraciones en formato estándar"
  "Disabling alloy like pretty printing")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-lemma
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lemma que busca contraejemplos del assertion a incluir como lema y traduce las expresiones usadas en las substituciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-lemma (name &optional subs)
  (let((msg(format t "Searching counterexamples for the assertion ~a with scope ~a.~%" name *global-scope*))
       (dummy(build-checking-assertion-file *assertion-file* name))
       (result(first(analyze-file *assertion-file*))))
    (if(equal result :counterexample-found)
	(let((dummy(show-last-counterexample)))
	  (skip-msg "Counterexample found. The assertion is invalid. You must not use it as a lemma."))
      (if(equal :translation-error result)
	  (skip-msg "ERROR in validation process. See dt.log for details.~%")
	(lemma name subs))))
  "Sólo introduce el assertion indicado como lema si no encuentra contraejemplos."
  "Introducing lemma")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dps-use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "use" que busca contraejemplos del assertion a incluir como lema y traduce las expresiones usadas en las substituciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstep dps-use (lemma &optional subst (if-match best) (instantiator inst?)
		    polarity? let-reduce?)
  (try-branch (dps-lemma lemma subst)
	      ((let ((fnum (car *new-fmla-nums*))
		     (command (generate-instantiator-command
			       if-match polarity? instantiator fnum)))
		 (then 
		  (beta fnum :let-reduce? let-reduce?)
		  (repeat command)))
	       (skip))
	      (skip))
  "Introduces lemma LEMMA, then does BETA and INST? (repeatedly) on
 the lemma.  The INSTANTIATOR argument may be used to specify an alternative
 to INST?."
  "Using lemma ~a")

