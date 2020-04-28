;;;	别名
;;;	选择
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
;;;	视口
(defun c:v0 () (ai_tiledvp 1 nil))	;;;	v0 v1 v2 v3 改变视口数量
(defun c:v1 () (command "-vports" "j"))
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
(defun c:ji () (calc-text))	;;;	计算text内容
;;;	属性
(defun c:satt () (search-att))
(defun c:gatt () (get-att))
;;;	文本操作
(defun c:a1 () (align-textangle))	;;;	文字旋转指定角度
(defun c:wzad () (text-join*))	;;;	文字合并
(defun c:wzap () (text-add-app*))
(defun c:wzc () (text-copy*))
;;;	块操作
(defun c:sbil () (search-block-inlayer*))	;;;	选择图层上所有块
(defun c:gb () (copy-to-block))	;;;	复制为块
(defun c:br () (radom-named-block))	;;;	定义为随机命名的块


;;	交选
;;	待加入空集判断
;;	解决选择完不显示的问题
(defun c:xselect (/ A B intersection)
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


;;;	选择图层上所有块  
;;; 需要用list, 使用`不行
(defun search-block-inlayer (layer)
	(sssetfirst nil (ssget "X" (list (cons 0 "INSERT") (cons 8 layer)))))
(defun search-block-inlayer* ()
	(princ "\n Select layer:")
	(search-block-inlayer (get-obj-att (car (entsel)) 8)))
;;;	复制为块
(defun copy-to-block (/ A)
	(setq A (ssget))
	(command "copybase" (getpoint "指定基点：") A "")
	(command "pasteblock")
	(princ))
;;;	定义为随机命名的块
(defun radom-named-block (/ A p)
	(setq A (ssget))
	(setq p (getpoint "指定基点："))
	(command "copybase" p A "")
	(command "pasteblock" p)
	(command "erase" A "")
	(princ))




;;; 将文字旋转至所选角度
;;;	50-角度，51-倾斜角度
;;;	TODO：选择对象
(defun align-textangle (/ A B i change-angle)
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
(defun calc-text (/ A i text cutstr)
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
	(cond
		((eq obj1 obj2) nil)
		(t (command "erase" obj2 ""))))
(defun text-join* (/ obj1 obj2)
	(text-join (car (entsel)) (car (entsel))))

;;; 文字加括号
(defun text-add-app(obj)
	(set-obj-att obj 1 (strcat "(" (get-obj-att obj 1) ")")))
(defun text-add-app* ()
	(text-add-app (car (entsel)))
	(text-add-app*))

;;;	 文字复制
(defun text-copy (t1 t2)
	(set-obj-att t2 1 (get-obj-att t1 1)))
(defun text-copy* ()
	(text-copy (car (entsel)) (car (entsel)))
	(princ))

;;; 查询所选对象属性
(defun search-att ()
	(princ (entget (ssname (ssget) 0)))
	(princ))
;;; 查询所选对象对应属性	
(defun get-att (/ num)
	(setq num (getint "\nDXF:"))
	(princ (get-obj-att (ssname (ssget) 0) num))
	(princ))




;;; nil?
(defun nil? (/ a) (= nil a))
(defun != (/ a b) (not (eq a b))) ;	not eq
;;;	get attribute of object
(defun get-obj-att (Obj num)
	(cdr (assoc num (entget Obj))))
;;;	set attribute of object
(defun set-obj-att (Obj num att)
	(entmod (subst
			(cons num att)
			(assoc num (entget Obj))
			(entget Obj)))) 