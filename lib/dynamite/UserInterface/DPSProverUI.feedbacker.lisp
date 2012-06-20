(defun show-dps-lemmas (theory-names-list) ;;una lista de theoryname
  (let ((result-string
	 (loop for theoryname in theory-names-list
	       append (progn ;let ((theoryname (first theory-data))(filename (second theory-data)))
			(let ((file	;(or filename
			       (pvs-file-of theoryname))) ;)
			  (when file
			    (parse-file file nil t)))
			(let* ((theory (get-parsed?-theory theoryname))
			       (*current-context* (when theory (saved-context theory)))
			       (*no-comments* nil)
			       (*show-conversions* nil)
			       (*ppmacros* t))
			  (when theory
			    (list
			     (string-right-trim
			      '(#\space #\tab #\newline)
			      (unparse theory
				       :string t
				       :char-width *default-char-width*)))))))))
    (pvs-buffer "Assertions and facts" (format nil "~{~a~%~%~}" result-string) t t)))
