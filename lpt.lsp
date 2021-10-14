;--------------------------------------
(defun zpctl ()
(setq ind_pct 0)
(setq sir (getstring  "Punct <Toate> :"))
(setq sspct (ssget "X" (list (cons 0 "TEXT"))))
(while (and sset
            (/=
               (setq text (ssname sspct ind_pct))
            nil))
(setq ed (entget text))
(setq text_string (cdr (assoc 1 (entget text))))
(setq texp (cdr (assoc 10 ed)))
(setq c1 (-  (car texp) 5))
(setq c2 (+  (cadr texp) 5))
(setq c3 (+  (car texp) 5))
(setq c4 (-  (cadr texp) 5))
(setq c5 (list c1 c2))
(setq c6 (list c3 c4))
(if (= text_string sir)
(command "ZOOM" "W" c5 c6)
)
(if (= text_string sir)
(setq sel text)
)
(setq ind_pct (1+ ind_pct))
)
(command "SELECT" sel "")
)
;---------------------------------
(defun start ()
(setq flag 0)
(setq index 1)
(setq indice 0)
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Anuleaza/<Primul punct trasare> :")))
;-----------------------------
(if ( = punct "C")
(progn
(setq col (acad_colordlg 4))
(command "COLOR" col)
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Anuleaza/<Primul punct trasare> :")))
)
)
;-----------------------------
(if ( = punct "Z")
(progn
(ZPCTL)
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Anuleaza/<Primul punct trasare> :")))
)
)
;----------------------
(if ( = punct "E")
(progn
(command "ERASE" last "")
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Primul punct trasare> :")))
)
)
;----------------------
(if ( = punct "T")
(progn
(setq tip_pl (getstring "\nTip polilinie :")) 
(command "LINETYPE" "S" tip_pl "")
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Primul punct trasare> :")))
)
)
;----------------------
(if ( = punct "G")
(progn
(setq g_pl (getreal "\nGrosime polilinie :")) 
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Primul punct trasare> :")))
)
)
;----------------------
(if ( = punct "L")
(progn
(setq la_num (getstring "\nNume layer curent :"))
(command "LAYER" "S" la_num "") 
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Primul punct trasare> :")))
)
)
(setq sset (ssget "X" (list (cons 0 "INSERT")(cons 2 "Punct"))))
)
;----------------------------
(defun c:lpt ()
(setvar "LTSCALE" 0.1)
(setq pauza "")
(setq num2 1) 
(setvar "cmdecho" 0)
(setq g_pl 0.0)
(start)
;-----------------------------
(if ( = punct "C")
(progn
(setq col (acad_colordlg 4))
(command "COLOR" col)
(start)
)
)
;-----------------------------
(if ( = punct "Z")
(progn
(ZPCTL)
(start)
)
)
;----------------------
(if ( = punct "E")
(progn
(command "ERASE" last "")
(start)
)
)
;----------------------
(if ( = punct "T")
(progn
(setq tip_pl (getstring "\nTip polilinie :")) 
(command "LINETYPE" "S" tip_pl "")
(start)
)
)
;----------------------
(if ( = punct "G")
(progn
(setq g_pl (getreal "\nGrosime polilinie :")) 
(start)
)
)
;----------------------
(if ( = punct "L")
(progn
(setq la_num (getstring "\nNume layer curent :"))
(command "LAYER" "S" la_num "") 
(start)
)
)
;----------------------
(while (and sset
            (/=
               (setq att (ssname sset indice))
            nil))
;Citeste atribute
;*****************
(setq ent (entget att))
(setq sw (cdr (assoc -1 ent)))
(while (/= "SEQEND" (cdr (assoc 0 (entget sw))))
(setq w (entget (entnext sw)))
(if (/= "SEQEND" (cdr (assoc 0 w)))
(progn
(if (= index 1)
(progn
(setq cit1 (cdr (assoc 1 w)))
(setq c1 (car (cdr (assoc 10 ent))))
(setq c2 (cadr (cdr (assoc 10 ent))))
(if (=  cit1 punct)
(setq pt1 (list c1 c2))
)
)
)
(if (= index 2)
(setq cit2 (cdr (assoc 1 w)))
)
(if (= index 3)
(setq cit3 (cdr (assoc 1 w)))
)
)
)
(setq sw (entnext sw))
(setq index (1+ index))
)
(setq index 1)
(setq indice (1+ indice))
)
;------------------
(setq index 1)
(setq indice 0)

(while (/=
 (setq input (cadr (grread T 5)))
  nil)
(setq indice 0)
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
;----------------------
(if ( = punct "P")
(progn
(start)
(setq pauza "P")
)
)
;(if ( = punct "AR") (setq flag 4))
;---------------------
(if ( = punct "A")
(progn
(setq num2 0)
(setq flag 1)
(setq num "A")
)
)
;----------------------
(if ( = punct "Z")
(progn
(ZPCTL)
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
;(prompt text_string)
)
)
;----------------------
(if ( = punct "E")
(progn
(PROMPT "OK")
(command "ERASE" last "")
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
)
)
;----------------------
(if ( = punct "T")
(progn
(setq tip_pl (getstring "\nTip polilinie :")) 
(command "LINETYPE" "S" tip_pl "")
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
)
)
;----------------------
(if ( = punct "G")
(progn
(setq g_pl (getreal "\nGrosime polilinie :")) 
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
)
)
;--------------------
(if ( = punct "C")
(progn
(setq col (acad_colordlg 4))
(command "COLOR" col)
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
)
)
(if ( = punct "L")
(progn
(setq la_num (getstring "\nNume layer curent :"))
(command "LAYER" "S" la_num "") 
(setq punct (strcase (getstring "\nArc/TipLinie/Grosime/Culoare/Layer/ZoomPct/Serie/Pauza/Sterge/<Punctul urmator> :")))
)
)
(while (and sset
            (/=
               (setq att (ssname sset indice))
            nil))
;Citeste atribute
;*****************
(setq ent (entget att))
(setq sw (cdr (assoc -1 ent)))
(while (/= "SEQEND" (cdr (assoc 0 (entget sw))))
(setq w (entget (entnext sw)))
(if (/= "SEQEND" (cdr (assoc 0 w)))
(progn
(if (= index 1)
(progn
(setq cit1 (cdr (assoc 1 w)))
(setq c1 (car (cdr (assoc 10 ent))))
(setq c2 (cadr (cdr (assoc 10 ent))))
(if (=  cit1 punct)
(progn
(setq pt2 (list c1 c2))
(if (= num "A")
(progn
(if (= flag 1)
(setq a pt1)
)
(if (= flag 2)
(setq b pt1)
)
(if (= flag 3)
(progn
(setq c pt1)
(setq num2 1)
)
)
(if (= flag 4)
(progn
(command "ARC" a b c)
(setq num "")
)
)
)
)
(if (/= pauza "P")
(if (= num2 1)

(command "PLINE" pt1 "W" g_pl g_pl pt2 "")
)
)
(setq flag (1+ flag))
(setq pauza "")
(setq pt1 pt2)
)
)
)
)
(if (= index 2)
(setq cit2 (cdr (assoc 1 w)))
)
(if (= index 3)
(setq cit3 (cdr (assoc 1 w)))
)
)
)
(setq sw (entnext sw))
(setq index (1+ index))
)
(setq index 1)
(setq indice (1+ indice))
)
)
)
