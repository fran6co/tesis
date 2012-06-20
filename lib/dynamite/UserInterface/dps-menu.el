(require 'easymenu)

(defconst dps-menus
  '("Dynamite"
    ["Open file..." open-alloy-file t]
    ["Prove assertion..." prove-alloy-assert 
      (string= (buffer-name) *dps-actual-alloy-file*)]
    ["Exit..." exit-dps t]))

(when (string-match "GNU Emacs" (emacs-version))
  (let ((easy-menu-fast-menus t))
    (easy-menu-define DPS global-map "DPS menus" dps-menus)))
