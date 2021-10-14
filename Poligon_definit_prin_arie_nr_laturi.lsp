; Acest cod LISP creaza un poligon definit prin aria lui si numarul de laturi
; Autor : Mandache Doru
; Data actualizare : 14.10.2021

(defun c:pba (/ a n apt ptloc)
(setq n (getint "Numarul de laturi ale poligonului: "))
(setq a (getreal "Aria dorita pentru poligon: "))
(setq ang (/ pi n))
(setq apt (sqrt (/ (/ a (/ (sin ang) (cos ang))) n))) ;calculeaza apotema pentru poligonul circumscris
(setq ptloc (getpoint "Selectati coordonatele: "))
(command "_POLYGON" n ptloc "C" apt)
)
