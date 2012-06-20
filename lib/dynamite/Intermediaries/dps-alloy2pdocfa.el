(defun dps-translate-spec (output)
  "Provoca que el traductor genere la estructura subyacente de teorías PDOCFA."
  (save-excursion
    (call-process JAVA nil output nil "-cp" 
    	(concat (concat DYNAMITETRANSLATOR ":") ALLOY) 
    	"ar.uba.dc.dynamite.api.SpecificationTranslatorRunner" 
    	*dps-actual-alloy-file* ))) 