(ai_tiledvp 1 nil)  ;;; 单个视口
(ai_tiledvp 2 "_V")	;;;	两个视口
;;;	_-vports	合并视口

;;; 自定义->界面->....鼠标双击->块->宏 bedit->refedit

;;;	(setvar "orthomode" (abs (1- (getvar "orthomode"))))



(defun C:1 ( / gp)
	(setvar "cmdecho" 0)
	(princ "\n★改变对象颜色为红色★")
	(setq gp (ssget))
	(if (/= gp nil) (command ".change" gp "" "p" "c" "1" ""))
	(princ "\nOK")
	(princ)
);end defun C:1 




;;; 将文字旋转至所选角度
;;;	50-角度，51-倾斜角度
;;;	TODO：选择对象
(defun c:aaaalign-textangle (/ A B) 
	(setq A (ssget))
	(setq B (getangle "指定第一点:<E:east>\n"))
	(defun changebyanlge (/ i)
		(defun change-angle (new-rad ent-data)
			(entmod (subst
						(cons 50 new-rad) 
						(assoc 50 ent-data)
						ent-data)))
		(setq i 0)
		(repeat (sslength A)
			(progn 
				(change-angle B (entget (ssname A i)))
				(setq i (1+ i)))))
	(changebyanlge))
		
(defun c:ddf (/ A B)
	(princ "指定要修改对象:")
	(setq A (ssget))
	(princ "指定目标对象:")
	(setq B (ssget))
	(changebyobject)
	(defun changebyobject (/ i)
		(setq i 0)
		(repeat (sslength A)
			(progn 
				(entmod (subst
							(assoc 50 (entget (ssname B 0)))
							(assoc 50 (entget (ssname A i)))
							(entget (ssname A i))))
				(setq i (1+ i)))))
)




	
(defun align-text-angle (/ A B)
	;;;	Judge witch mode
	(nil)
	(defun changebyanlge (/ i)	;;;	Change by anlge
		(defun change-angle (new-rad ent-data)
			(entmod (subst
						(cons 50 new-rad) 
						(assoc 50 ent-data)
						ent-data)))
		(setq i 0)
		(repeat (sslength A)
			(progn 
				(change-angle B (entget (ssname A i)))
				(setq i (1+ i)))))
	(defun changebyobject (/ i)	;;;	Change by object
		(setq i 0)
		(repeat (sslength A)
			(progn 
				(entmod (subst
							(assoc 50 (entget (ssname B 0)))
							(assoc 50 (entget (ssname A i)))
							(entget (ssname A i))))
				(setq i (1+ i))))))
				
				
(defun calc-text (/ A)
	(setq A (ssget))
	(cal (cdr (assoc 1 (entget (ssname A 0))))))
	
