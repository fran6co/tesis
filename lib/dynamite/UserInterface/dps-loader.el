;; Declaro los nombres de los buffers que necesita el módulo.
(defconst TRANSLATOROUTPUT-BUFFERNAME "DynamiteTranslator output")

;; Cargo los archivos del módulo
(load (format "%s/dps-usercmds" USERINTERFACEPATH) nil noninteractive)
(load (format "%s/dps-prover" USERINTERFACEPATH) nil noninteractive)
(load (format "%s/dps-menu" USERINTERFACEPATH) nil noninteractive)
(load (format "%s/alloy-mode" USERINTERFACEPATH) nil noninteractive)
 
