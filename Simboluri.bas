;; Modifica factorul de scalare a blocurilor din desenul curent ;;

(defun c:MSB ( / enx idx scl sel )
  (initget 6)
  (if (and (setq scl (getdist "\nSpecifica noul factor de scalare : "))
           (setq sel (ssget "_:L" '((0 . "INSERT"))))
      )
      (repeat 
           (setq idx (sslength sel))
           (setq enx (entget (ssname sel (setq idx (1- idx)))))
           (foreach dxf '(41 42 43)
               (setq enx (subst (cons dxf scl) (assoc dxf enx) enx))
           )
           (entmod enx)
      )
  )
  (princ)
)
