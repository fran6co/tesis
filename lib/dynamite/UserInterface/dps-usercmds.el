(global-set-key (kbd "s-<right>") 'goto-next-sug)
(global-set-key (kbd "s-<left>") 'goto-prev-sug)

(add-hook 'before-change-functions 'clear-for-hook)

(defun clear-for-hook (beg end)
   (clear-suggestions))

(defun goto-next-sug ()
  (interactive)
  (goto-char (next-overlay-change (point))))

(defun goto-prev-sug ()
  (interactive)
  (goto-char (previous-overlay-change (point))))

(defun clear-suggestions ()
  (interactive)
  (remove-overlays))

(defun paint-region (start end)
  (let ((face 'highlight))
    (hi-lock-mode 1)
    (save-excursion 
      (let ((overlay (make-overlay start end)))
	(overlay-put overlay 'hi-lock-overlay t)
	(overlay-put overlay 'face face)))))

(defun paint-suggestions ()
  "Pinta las sugerencias de uso de partes de especificación, si existen."
  (interactive)
  (let ((core-file-name-and-path (format  "%s%s" (pvs-current-directory) +core-file-name+))) 
  (if (not (file-readable-p core-file-name-and-path))
      (message "There are no suggestions.")
    (mapc (apply-partially 'paint-suggestion)
	  (parse-suggestion-data-file core-file-name-and-path)))))

(defun paint-suggestion (suggestion-data)
  "Pinta la región ocupada por la 'suggestion-data' (una lista que contiene exactamente los cinco elementos que forman la sugerencia: el nombre del archivo, la línea y la columna donde comienza la sugerencia y la línea y la columna donde la sugerencia termina)."
  (let ((file-name (nth 0 suggestion-data))
	(line1 (nth 1 suggestion-data))
	(col1 (nth 2 suggestion-data))
	(line2 (nth 3 suggestion-data))
	(col2 (nth 4 suggestion-data)))
    (unless (string-match (format "%s$" +synthesized-goal-file-name+) file-name)
      (let ((data-buffer (find-file-noselect file-name)))
	(when (null data-buffer) (error "Couldn't read file '%s'." file-name))
	(with-current-buffer data-buffer
	  (paint-region (progn (save-excursion (goto-line line1) (forward-char (- col1 1)) (point)))
			(progn (save-excursion (goto-line line2) (forward-char col2) (point)))))))))


(defun show-suggestions ()
  "Muestra las sugerencias de uso de partes de especificación, si existen."
  (interactive)
  (when (get-buffer +suggestions-buffer-name+) (kill-buffer +suggestions-buffer-name+))
  (let ((core-file-name-and-path (format  "%s%s" (pvs-current-directory) +core-file-name+))) 
  (if (not (file-readable-p core-file-name-and-path))
      (message "There are no suggestions to show.")
    (mapc (apply-partially 'append-suggestion (get-buffer-create +suggestions-buffer-name+))
	  (parse-suggestion-data-file core-file-name-and-path))
    (switch-to-buffer (get-buffer-create +suggestions-buffer-name+))
    (not-modified)
    (setq buffer-read-only t)
    (message "Showing suggestions in buffer %s. " +suggestions-buffer-name+))))

(defun append-suggestion (suggestion-buffer suggestion-data)
  "Escribe la sugerencia en el buffer indicado. La 'suggestion-data' es una lista que contiene exactamente los cinco elementos que forman la sugerencia: el nombre del archivo, la línea y la columna donde comienza la sugerencia y la línea y la columna donde la sugerencia termina."
  (let ((file-name (nth 0 suggestion-data))
	(line1 (nth 1 suggestion-data))
	(col1 (nth 2 suggestion-data))
	(line2 (nth 3 suggestion-data))
	(col2 (nth 4 suggestion-data)))
    (let ((data-buffer (find-file-noselect file-name)))
      (when (null data-buffer) (error "Couldn't read file '%s'." file-name))
    (with-current-buffer (get-buffer-create suggestion-buffer)
      (insert (format "\n-- from %s\n-- line %d column %d to line %d column %d\n" file-name line1 col1 line2 col2)))
    (with-current-buffer data-buffer
      (append-to-buffer suggestion-buffer
		    (progn (save-excursion (goto-line line1) 
					   (point)))
		    (progn (save-excursion (goto-line (+ line2 1))
					   (point))))))))

(defun parse-suggestion-data-buffer (&optional buffer)
  "Parsea sugerencias (string seguido de cuatro números, separados por un espacio) del buffer indicado. Si no se indica ninguno, lo hace sobre el buffer actual."
  (interactive)
  (with-current-buffer (if (null buffer) (current-buffer) buffer)
  (goto-char 0)
  (let (result '())
  (while (re-search-forward "[/~][-a-z/0-9_.]+[-a-z/0-9_.\ ]*[a-z0-9]+" nil t)
    (let (str n1 n2 n3 n4) 
      (setq str (buffer-substring (match-beginning 0) (match-end 0)))
      (re-search-forward "[0-9]+")
      (setq n1 (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))
      (re-search-forward "[0-9]+")
      (setq n2 (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))
      (re-search-forward "[0-9]+")
      (setq n3 (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))
      (re-search-forward "[0-9]+")
      (setq n4 (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))
      (setq result (append (list (list str n1 n2 n3 n4)) result)))) result)))

(defun parse-suggestion-data-file (filename)
  "Parsea un archivo de sugerencias (string y cuatro números enteros separados por espacios). Devuelve una lista de listas, cada una de las listas internas representa una sugenrencia (es una lista de exactamente cinco elementos: un string y cuatro números enteros)."
  (with-temp-buffer
  (insert-file filename)
  (parse-suggestion-data-buffer)))

(defun keep-auxiliar-als (answer)
  ""
  (interactive (list (completing-read "Keep auxiliar als?(yes/no) " (list "yes" "no") nil '1 nil)))
  (ilisp-send (format "(setq *dps-one-auxiliar-file* %s)" (string= answer "no"))))
  
(defun open-alloy-file (filename)
  "Carga la especificación Alloy indicada"
  (interactive "fLoad Alloy file named: ")
  (change-context (file-name-directory filename))
  (switch-to-buffer (find-file-noselect filename))
  (setq *dps-actual-alloy-file* (file-name-nondirectory filename))
  ;; Agrego un "hook" para que cada vez que se grabe el modelo Alloy
  ;; se desahabilite la posibilidad de comenzar una demostración
  ;; hasta que se generen nuevamente las teorías subyacentes.
  (add-hook 'after-save-hook
	    '(lambda () (setq *dps-ready-to-prove* nil)) nil t)
  ;; cargo en el lisp interno el nombre del archivo als cargado actualmente.
  (ilisp-send 
   (format "(setq *current-als-file-name* \"%s\")" 
	   (file-name-sans-extension 
	    (file-name-nondirectory filename))))
  (message "Alloy model correctly loaded."))
 
(defun generate-pdocfa-theories ()
  "Genera la estructura PDOCFA subyacente para la especificación Alloy actualmente cargada."
  (interactive)
  (if (not *dps-actual-alloy-file*)
      (error "There's no Alloy specification loaded.")
    (when (get-buffer TRANSLATOROUTPUT-BUFFERNAME)
      (kill-buffer TRANSLATOROUTPUT-BUFFERNAME))
    (dps-translate-spec (get-buffer-create TRANSLATOROUTPUT-BUFFERNAME))
    (save-excursion 
      (set-buffer (get-buffer TRANSLATOROUTPUT-BUFFERNAME))
      (when (> (buffer-size) 0)
	  (error "There were some problems trying to generate the underlying theories. \nSee \"DynamiteTranslator ouput\" buffer for details."))
      (setq *dps-ready-to-prove* t)
      (message "Underlying theories correctly generated."))))

(defun show-dps-lemmas ()
  "Muestra los lemas disponibles para ser usados en las demostraciones de asserts Alloy."
  (interactive)
  (unless (interactive-p) (pvs-collect-theories))
  (pvs-bury-output)
  (save-some-pvs-buffers t)
  (pvs-typecheck-file (spec-theorems-theory-name (current-alloy-file-name)) nil nil 'typecheck)
  (pvs-send-and-wait
   (format "(show-dps-lemmas (list \"%s\" \"%s\" \"%s\" \"PDOCFATheorems\"))"
	   (spec-theorems-theory-name (current-alloy-file-name))
	   (spec-lemmas-theory-name (current-alloy-file-name))
	   (spec-axioms-theory-name (current-alloy-file-name)))
   "Prettyprinting"
   (pvs-get-abbreviation 'prettyprint-theory)
   'dont-care)
  (message
   (format "Showing available lemmas for %s. " (current-alloy-file-name))))

(defun exit-dps ()
  "Sale de DPS."
  (interactive)
  (cond ((and ilisp-buffer
	      (get-buffer ilisp-buffer)
	      (ilisp-process)
	      (eq (process-status (ilisp-process)) 'run)
	      pvs-initialized)
	 (confirm-not-in-checker)
	 (when (or noninteractive
		   (y-or-n-p "Do you want to exit DPS? "))
	   (exit-pvs-process)
	   (save-buffers-kill-emacs nil)))
	(t (save-buffers-kill-emacs nil))))

;; reescribo la asignación de teclas rápidas
(global-set-key "\C-x\C-c" 'exit-dps)