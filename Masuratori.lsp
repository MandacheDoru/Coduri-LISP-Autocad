(defun c:CLL ( / s e )                                                ; Citeste lungimea unei linii selectate
  (princ "\n Selecteaza o linie pentru citirea lungimii :")
    (if (setq s (ssget "_+.:S:E" '((0 . "LINE"))))
      (princ (strcat "\n Lungimea totala este : " (rtos (distance (cdr (assoc 10 (setq e (entget (ssname s 0))))) (cdr (assoc 11 e))) 2)))
    )
  (princ)
)

(defun c:CLLT ( / obj st end mid and dist)                            ; Citeste lungimea unei linii si pune in text
    (setq obj (car (entsel "Pick a Line")))
    (setq txtstr (rtos (vla-get-length (vlax-ename->vla-object obj)) 2 3))
    (setq st (cdr (assoc 10 (entget obj))))
    (setq end (cdr (assoc 11 (entget obj))))
    (setq ang (angle st end))
    (setq dist (distance st end))
    (setq mid (polar st ang (/ dist 2.0)))
    (command "text" mid 2.5 0 txtstr)
    (princ)
)

(defun c:CALE (/ _getlength i n ss)                                  ; Citeste si aduna lungimele mai multor entitati selectate
  (defun _getlength (ename / ep)
    (if (vl-catch-all-error-p (setq ep (vl-catch-all-apply 'vlax-curve-getendparam (list ename))))
      0.0
      (vlax-curve-getdistatparam ename ep)
    )
  )
  (if (setq ss (ssget '((0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE"))))
    (progn (setq n 0)
           (repeat (setq i (sslength ss)) (setq n (+ n (_getlength (ssname ss (setq i (1- i)))))))
           (alert (vl-princ-to-string n))
    )
  )
  (princ)
)

(defun GetLengthOfLine (lineEntity / lineEntityInfo)
	 (setq lineEntityInfo (entget lineEntity))                          ; get line entity info list
	 (distance
     (cdr (assoc 10 lineEntityInfo))
       (cdr (assoc 11 lineEntityInfo)))                               ; access the info at keys 10 & 11 (start and end points),
)                                                                     ; chop off the key so its just the points,
	                                                                    ; measure the distance between the two points,
            
(defun GetLengthOfEachMultilineSegment ( lineEntity / lineEntityInfo segmentList)
	 (setq lineEntityInfo (entget lineEntity))                          ; get line entity info list
	 (setq segmentList nil)
	 (foreach infoItem lineEntityInfo
     (cond
			(  (= (car infoItem) 11)
				(setq segmentList (append segmentList (list (cdr infoItem))))
				)))
	 (setq firstPoint (car segmentList))
	 (setq segmentList (cdr segmentList))
	 (setq lengthsList nil)
	 (foreach secondPoint segmentList
     (setq lengthsList 
			(append lengthsList (list (distance firstPoint secondPoint))))
		 (setq firstPoint secondPoint))
	 lengthsList
)
                                                                      
(defun GetLengthOfPolyline ( lineEntity / )
	 (vl-load-com)
     (vlax-curve-getDistAtParam lineEntity
		   (vlax-curve-getEndParam lineEntity))
)
