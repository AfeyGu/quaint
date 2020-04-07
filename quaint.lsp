;;	别名	
(defun c:zzselect() (c:xselect))
(defun c:a2() (align-textangle))
(defun c:satt() (search-att))

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
			(setq i (1+ i)) )
		(setq A nil B nil)
		(progn (sssetfirst nil C)))
	(intersection))


;;; 将文字旋转至所选角度
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


;;; 查询所选对象属性
(defun search-att ()
	(princ (entget (ssname (ssget) 0))))