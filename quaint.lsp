;;	别名	
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
(defun c:a1 () (align-textangle))
(defun c:satt () (search-att))
(defun c:v1 () (command "-vports" "j"));;;	v1 v2 v3 改变视口
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
(defun c:ji () (calc-text))
(defun c:gatt () (get-att))
(defun c:wzad () (text-join*))


;;	交选
;;	待加入空集判断
;;	解决选择完不显示的问题
(defun c:xselect (/ A B)
	(setq A (ssget) B (ssget))
	(defun intersection (/ C i)
		(setq C (ssadd))
		(setq i 0)
		(repeat (sslength B)
			(if (/= nil (ssmemb (ssname B i) A)) (ssadd (ssname B i) C))
			(setq i (1+ i)))
		(setq A nil B nil)
		(progn (sssetfirst nil C)))
	(intersection))


;;; 将文字旋转至所选角度
;;;	50-角度，51-倾斜角度
;;;	TODO：选择对象
(defun align-textangle (/ A B)
	(defun change-angle (new-rad ent-data)
		(entmod (subst
				(cons 50 new-rad)
				(assoc 50 ent-data)
				ent-data)))
	(setq A (ssget))
	(setq B (getangle "指定第一点:"))
	(setq i 0)
	(repeat (sslength A)
		(progn
			(change-angle B (entget (ssname A i)))
			(setq i (1+ i)))))


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
				(strcat text "=" (rtos (cal text) 2 2)))
			(setq i (1+ i)))))


;;; 文字合并
(defun text-join (obj1 obj2)
	(set-obj-att
		obj1
		1
		(strcat (get-obj-att obj1 1) (get-obj-att obj2 1)))
	(command "erase" obj2 ""))
(defun text-join* (/ obj1 obj2)
	(text-join  (car (entsel)) (car (entsel))))


;;; 查询所选对象属性
(defun search-att ()
	(princ (entget (ssname (ssget) 0))))
;;; 查询所选对象对应属性	
(defun get-att (/ num)
	(setq num (getint "\nDXF:"))
	(princ (get-obj-att (ssname (ssget) 0) num)))
	
	
	
	
	
	
;;;	get attribute of object
(defun get-obj-att (Obj num)
	(cdr (assoc num (entget Obj))))
;;;	set attribute of object
(defun set-obj-att (Obj num att)
	(entmod (subst
			(cons num att)
			(assoc num (entget Obj))
			(entget Obj))))