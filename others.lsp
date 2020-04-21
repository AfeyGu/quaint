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




	
;;;	选择集 selectset -> ssget
;;;	图元（对象） -> 
;;;	对象名 -> ssname
;;;	对象的定义数据ent -> entget
;;;	assoc  cdr




;;; 计算表达式值
;;;	maybe can use foreach funcation
;;;	add setting of accuracy 
;;;	subset x by * (more in cutstr)
(defun calc-text (/ A i text)
	(setq A (ssget))
	(defun cutstr (str)
		(substr str 1 (VL-String-Search "=" str)))
	(setq i 0)
	(repeat (sslength A)
		(progn
			(setq text (cutstr (get-obj-att (ssname A i) 1)))
			(set-obj-att
				(ssname A i)
				1
				(strcat text "=" (rtos (cal text) 2 3)))
			(setq i (1+ i)))))
;;;	get attribute of object
(defun get-obj-att (Obj num)
	(cdr (assoc num (entget Obj))))
;;;	set attribute of object
(defun set-obj-att (Obj num att)
	(entmod (subst
			(cons num att)
			(assoc num (entget Obj))
			(entget Obj))))
;;;	查询对应属性
(defun )
