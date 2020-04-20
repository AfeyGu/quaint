(ai_tiledvp 1 nil);;; 鍗曚釜瑙嗗彛
(ai_tiledvp 2 "_V");;;	涓や釜瑙嗗彛
;;;	_-vports	鍚堝苟瑙嗗彛

;;; 鑷畾涔?->鐣岄潰->....榧犳爣鍙屽嚮->鍧?->瀹? bedit->refedit

;;;	(setvar "orthomode" (abs (1- (getvar "orthomode"))))



(defun C:1 (/ gp)
	(setvar "cmdecho" 0)
	(princ "\n鈽呮敼鍙樺璞￠鑹蹭负绾㈣壊鈽?")
	(setq gp (ssget))
	(if (/= gp nil) (command ".change" gp "" "p" "c" "1" ""))
	(princ "\nOK")
	(princ)
);end defun C:1 




;;; 灏嗘枃瀛楁棆杞嚦鎵�閫夎搴?
;;;	50-瑙掑害锛?51-鍊炬枩瑙掑害
;;;	TODO锛氶�夋嫨瀵硅薄
(defun c:aaaalign-textangle (/ A B)
	(setq A (ssget))
	(setq B (getangle "鎸囧畾绗竴鐐?:<E:east>\n"))
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
	(princ "鎸囧畾瑕佷慨鏀瑰璞?:")
	(setq A (ssget))
	(princ "鎸囧畾鐩爣瀵硅薄:")
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
