; Acest program funcționează în același mod ca și comanda TCircle din Express Tools: 
; permițând utilizatorului să creeze un cadru dreptunghiular cu polilinie 2D în jurul obiectelor Text & MText selectate, 
; cu un offset definit de utilizator. De asemenea, programul va funcționa cu succes cu Text sau MText 
; definit în orice plan de construcție și în toate setările UCS și vizualizare.

(defun c:IT ( / *error* def enx idx lst off sel )
 
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
 
    (if (or (not (setq def (getenv "LMac\\boxtext-off")))
            (not (setq def (distof def 2)))
        )
        (setenv "LMac\\boxtext-off" (rtos (setq def 0.35) 2 2))
    )
    (initget 4)
    (if (setq off (getreal (strcat "\nDistanţa dintre text <" (rtos def 2 2) "> mm: ")))
        (setenv "LMac\\boxtext-off" (rtos off 2 2))
        (setq off def)
    )
    
    (LM:startundo (LM:acdoc))
    (if (setq sel (LM:ssget "\nSelecteaza textul <exit>: " '(((0 . "TEXT,MTEXT")))))
        (repeat (setq idx (sslength sel))
            (setq enx (entget (ssname sel (setq idx (1- idx))))
                  lst (text-box-off enx (* off (cdr (assoc 40 enx))))
            )
            (entmake
                (append
                   '(
                        (000 . "LWPOLYLINE")
                        (100 . "AcDbEntity")
                        (100 . "AcDbPolyline")
                        (090 . 4)
                        (070 . 1)
                    )
                    (LM:defaultprops enx)
                    (list (cons  038 (caddar lst)))
                    (mapcar '(lambda ( x ) (cons 10 x)) lst)
                    (list (assoc 210 enx))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)
 
;; ssget 
 
(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)
 
;; Default Properties  
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)
 
;; Text Box  
 
(defun text-box-off ( enx off / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (= "TEXT" (cdr (assoc 00 enx)))
            (setq bpt (cdr (assoc 10 enx))
                  rot (cdr (assoc 50 enx))
                  lst (textbox enx)
                  lst
                (list
                    (list (- (caar  lst) off) (- (cadar  lst) off)) (list (+ (caadr lst) off) (- (cadar  lst) off))
                    (list (+ (caadr lst) off) (+ (cadadr lst) off)) (list (- (caar  lst) off) (+ (cadadr lst) off))
                )
            )
        )
        (   (= "MTEXT" (cdr (assoc 00 enx)))
            (setq ocs  (cdr (assoc 210 enx))
                  bpt  (trans (cdr (assoc 10 enx)) 0 ocs)
                  rot  (angle '(0.0 0.0) (trans (cdr (assoc 11 enx)) 0 ocs))
                  wid  (cdr (assoc 42 enx))
                  hgt  (cdr (assoc 43 enx))
                  jus  (cdr (assoc 71 enx))
                  org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                             (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                       )
                  lst
                (list
                    (list (- (car org) off)     (- (cadr org) off))     (list (+ (car org) wid off) (- (cadr org) off))
                    (list (+ (car org) wid off) (+ (cadr org) hgt off)) (list (- (car org) off)     (+ (cadr org) hgt off))
                )
            )
        )
    )
    (if lst
        (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
            (list
                (list (cos rot) (sin (- rot)) 0.0)
                (list (sin rot) (cos rot)     0.0)
               '(0.0 0.0 1.0)
            )
        )
    )
)
 
;; Matrix x Vector 
 
(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)
 
;; Start Undo  
 
(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)
 
;; End Undo
 
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)
 
;; Active Document 
 
(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)
 
;;----------------------------------------------------------------------;;
 
(vl-load-com)

(princ)

;; --------------------------------------------------------------------;;

(defun C:MTXT (/ count sstext textent tb ll ur ul lr tf)
  (setvar "osmode" 0)
  (setq CURRENTLAYER (getvar "CLAYER"))
  (command "_.undo" "Begin")
  (command "_.layer" "m" "marcaj_text" "c" "8" "" "")
  (setq SSTEXT (ssget '((0 . "TEXT"))))
  (setq COUNT 0)
  (if SSTEXT
    (while (< COUNT (sslength SSTEXT))
      (setq textent (cdr (car (entget (ssname SSTEXT COUNT)))))
      (command "_.ucs" "Object" textent)
      (setq tb (textbox (list (cons -1 textent)))
      ll (car tb)
      ur (cadr tb)
      tf (/ (- (cadr ur) (cadr ll)) 6)
      ul (list (- (car ll) tf) (+ (cadr ur) tf))
      lr (list (+ (car ur) tf) (- (cadr ll) tf))
      ll (list (- (car ll) tf) (- (cadr ll) tf))
      ur (list (+ (car ur) tf) (+ (cadr ur) tf))
      )
      (command "_.solid" ll lr ul ur "")
      (command "_.copy" textent "" "0,0" "0,0")
      (command "_.erase" "p" "")
      (command "_.ucs" "p")
      (setq COUNT (1+ COUNT))
    )
  )
  (command "_.undo" "End")
  (setvar "CLAYER" CURRENTLAYER)
  (princ)
)
