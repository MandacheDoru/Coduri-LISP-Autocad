;; Modifica factorul de scalare a blocurilor din desenul curent ;;
;; --------------------------------------BEGIN---------------------------------------------- ;;

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
      
;; --------------------------------------END------------------------------------------------ ;;
      
;; Roteste la  0 grade un bloc selectat ;;
;; --------------------------------------BEGIN---------------------------------------------- ;;
      
(defun C:ROTB0_LP (/ bdata sel)
  ;(setq sel (ssget "_x" '((0 . "INSERT")(2 . "NUME_BLOC"))))
  (setq bdata (entget (car (entsel "\nSelecteaza blocul pentru a fi rotit la 0: "))))
    (entmod (subst '(50 . 0) (assoc 50 bdata) bdata))
)    
              
(vl-load-com)
(defun C:ROTB0_VL ( / ss cnt)
  (setq ss (ssget "_x" '((0 . "INSERT")(2 . "NUME_BLOC"))))    ; NUME_BLOC = numele blocului
  (repeat (setq cnt (sslength ss))
    (vlax-put-property (vlax-ename->vla-object (ssname ss (setq cnt (1- cnt)))) 'rotation 0)
  )
  (princ)
)  
                      
;; --------------------------------------END------------------------------------------------ ;;
