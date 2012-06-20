(defvar alloy-mode-hook nil)

(defvar alloy-mode-map
  (let ((alloy-mode-map (make-keymap)))
    (define-key alloy-mode-map "\C-j" 'newline-and-indent)
    alloy-mode-map)
  "Keymap for Alloy major mode")

(defconst alloy-default-tab-width 2)
(add-to-list 'auto-mode-alist '("\\.als\\'" . alloy-mode))

;; basado en http://alloy.mit.edu/alloy4/alloy4syntax.txt
(defvar alloy-font-lock-keywords 
  (list
   (cons
    (mapconcat 
     'identity
     '("\\bmodule" "open" "exactly" "private" "as"
       "fact" "assert"
       "fun" "pred"
       "run" "check"
       "for" "but" "expect"
       "int" "seq"
       "sig" "abstract" 
       "enum" 
       "extends"
       "let" "not" "implies" "else"
       "none" "iden" "univ" "Int" "seq/Int" 
       "disj"
       "no" "some" "lone" "one" "set" "seq"
       "iff" "and" "or" "implies" "not"
       "all" "sum" "in" "set"
       "this\\b")
     "\\b\\|\\b")
    font-lock-keyword-face)
   (cons
    (mapconcat 
     'identity
     '("*" "~" "\\^" 
       "\\." "\\[" "\\]"
       ":" "<" ">" 
       "-" "&" "+" "#"
       "=" "!" "|" ","
       "{" "}" "("")" "@")
     "\\|")
    font-lock-keyword-face))
  "Default highlighting expressions for Alloy mode")

(defvar alloy-mode-syntax-table
  (let ((alloy-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" alloy-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" alloy-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" alloy-mode-syntax-table)
    (modify-syntax-entry ?\-  ". 124b" alloy-mode-syntax-table)
   alloy-mode-syntax-table)
  "Syntax table for cl-mode")

(defun alloy-mode ()
  "Major mode for editing Alloy specifications"
  (interactive)
  (kill-all-local-variables)
  (use-local-map alloy-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(alloy-font-lock-keywords))
  (setq major-mode 'alloy-mode)
  (setq mode-name "Alloy")
  (run-hooks 'alloy-mode-hook)
  (set-syntax-table alloy-mode-syntax-table)
)
 
(provide 'alloy-mode)