
;;; Modificación a código PVS que permite que proof-status indique cuáles de los elementos de los que depende cada demostración se usan directamente en ella.
(defmethod pc-analyze ((decl formula-decl))
  (let* ((*dependings* nil)
	 (*proved-dependings* nil)
	 (*unproved-dependings* nil)
	 (*defn-dependings* nil)
	 (*axiom-dependings* nil)
	 (*assumption-dependings* nil)
	 (*depending-chain* nil)
	 (*depending-cycles* nil)
	 (*current-context* (context decl))
	 (*possible-judgements* (possible-judgements decl))
	 (fdecls (union (union (refers-to decl)
			       (proof-refers-to decl))
			(assuming-tccs decl)))
	 (decls (union fdecls *possible-judgements*)))
    (pc-analyze* decls)
    (mapc #'(lambda (y)
	      (cond ((typep y 'formula-decl)
		     (if (axiom? y)
			 (push y *axiom-dependings*)
			 (if (assumption? y)
			     (push y *assumption-dependings*)
			     (if (proved? y)
				 (push y *proved-dependings*)
				 (push y *unproved-dependings*)))))
		    ((and (or (typep y 'const-decl)
			      (typep y 'def-decl))
			  (definition y))
		     (push y *defn-dependings*))
		    (t)))
	  *dependings*)
    (if (axiom? decl)
	(format t "~%~a.~a is an axiom." (id (module decl)) (id decl))
	(if (proved? decl)
	    (format t "~%~a.~a has been PROVED." (id (module decl)) (id decl))
	    (format t "~%~a.~a is UNPROVED." (id (module decl)) (id decl))))
    (when *depending-cycles*
      (format t "~%~%***Warning***: The proof chain for ~a is CIRCULAR in the following:"
	(id decl))
      (loop for x in *depending-cycles*
	    do (format t "~%   ~a.~a" (id (module x))(id x))))
    (cond
      ((and (null *unproved-dependings*) (proved? decl))
       (format t "~%~%  The proof chain for ~a is COMPLETE." (id decl)))
      ((proved? decl)
       (format t "~%~%  The proof chain for ~a is INCOMPLETE.~
                  ~%  It depends on the following unproved conjectures:"
	 (id decl))
       (loop for x in (pc-sort *unproved-dependings*)
	     do
	     (format t "~%    ~a.~a ~a" (id (module x)) (id x) 
		     (if(member x decls) "(top-level use)" "")))))
    (when *proved-dependings*
      (format t "~%~%  ~a depends on the following proved theorems:"
	(id decl))
      (loop for x in (pc-sort *proved-dependings*)
	     do
	     (format t "~%    ~a.~a ~a" (id (module x)) (id x)
		     (if(member x decls) "(top-level use)" ""))))
    (when *axiom-dependings*
      (format t "~%~%  ~a depends on the following axioms:"
	(id decl))
      (loop for x in (pc-sort *axiom-dependings*)
	     do
	     (format t "~%    ~a.~a ~a" (id (module x)) (id x)
		     (if(member x decls) "(top-level use)" ""))))
    (when *defn-dependings*
      (format t "~%~%  ~a depends on the following definitions:"
	(id decl))
      (loop for x in (pc-sort *defn-dependings*)
	     do
	     (format t "~%    ~a.~a ~a" (id (module x)) (id x)
		     (if(member x decls) "(top-level use)" ""))))
    (when *assumption-dependings*
      (format t "~%~%  ~a depends on the following assumptions:"
	(id decl))
      (loop for x in (pc-sort *assumption-dependings*)
	     do
	     (format t "~%    ~a.~a ~a" (id (module x)) (id x)
		     (if(member x decls) "(top-level use)" ""))))
    (when *possible-judgements*
      (format t "~%~%  ~a may depend on the following judgements:"
	(id decl))
      (loop for x in (pc-sort *possible-judgements*)
	     do
	     (format t "~%    ~a.~a ~a" (id (module x)) (id x)
		     (if(member x decls) "(top-level use)" ""))))))
