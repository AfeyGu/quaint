(ai_tiledvp 1 nil);;; 单个视口
(ai_tiledvp 2 "_V");;;	两个视口
;;;	_-vports	合并视口

;;; 自定�?->界面->....鼠标双击->�?->�? bedit->refedit

;;;	(setvar "orthomode" (abs (1- (getvar "orthomode"))))



(defun C:1 (/ gp)
	(setvar "cmdecho" 0)
	(princ "\n★改变对象颜色为红色�?")
	(setq gp (ssget))
	(if (/= gp nil) (command ".change" gp "" "p" "c" "1" ""))
	(princ "\nOK")
	(princ)
);end defun C:1 




;;; 将文字旋转至所选角�?
;;;	50-角度�?51-倾斜角度
;;;	TODO：选择对象
(defun c:aaaalign-textangle (/ A B)
	(setq A (ssget))
	(setq B (getangle "指定第一�?:<E:east>\n"))
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
	(princ "指定要修改对�?:")
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
	(defun changebyanlge (/ i);;;	Change by anlge
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
	(defun changebyobject (/ i);;;	Change by object
		(setq i 0)
		(repeat (sslength A)
			(progn
				(entmod (subst
						(assoc 50 (entget (ssname B 0)))
						(assoc 50 (entget (ssname A i)))
						(entget (ssname A i))))
				(setq i (1+ i))))))



;;;	ѡ�� selectset -> ssget
;;;	ͼԪ������ -> 
;;;	������ -> ssname
;;;	����Ķ�������ent -> entget
;;;	assoc  cdr


(defun calc-text* (/ A i)
	(setq A (ssget))
	(setq i 0)
	(defun cutstr (str)
		(substr str 1 (VL-String-Search "=" str)))
	(defun calc (obj)
		(cond ( (wcmatch (get-obj-att obj 1) "=*")
				(set-obj-att
					obj
					1
					(strcat
						(cutstr (get-obj-att obj 1))
						"="
						(rtos (cal (cutstr (get-obj-att obj 1)))))))
			( (not (wcmatch (get-obj-att obj 1) "=*"))
				(set-obj-att obj 1 (rtos (cal (get-obj-att obj 1)))))
			(t pause)))
	(repeat (sslength A)
		(progn
			(set-obj-att
				(ssname A i)
				1
				(calc (ssname A i)))
			(setq i (1+ i))
)))


(defun c:asb () (calc-text*))
(defun c:ass () (calc-text))
;;;	maybe can use foreach funcation
(defun calc-text (/ A i text)
	(setq A (ssget))
	(setq i 0)
	(repeat (sslength A)
		(progn
			(setq text (get-obj-att (ssname A i) 1))
			(set-obj-att
				(ssname A i)
				1
				(strcat text "=" (rtos (cal text))))
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

