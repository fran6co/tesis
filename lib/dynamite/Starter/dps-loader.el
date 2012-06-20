;; dps-loader.el
;;
;; carga las extensiones de emacs para DPS.

;; Cargo los "paths" de los módulos de DPS.
(defconst USERINTERFACEPATH (format "%s/UserInterface" DPSPATH))
(defconst INTERMEDIARIESPATH (format "%s/Intermediaries" DPSPATH))

;; Invoco a los cargadores de cada módulo.
(load (format "%s/dps-loader" USERINTERFACEPATH) nil noninteractive)
(load (format "%s/dps-loader" INTERMEDIARIESPATH) nil noninteractive)
