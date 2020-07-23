;;; 自定义->界面->....鼠标双击->块->宏 bedit->refedit

;;;	----------------------------------------------------------------------------
;;; model->fun ; or import->model (从某个模组运行函数或者加载模组后直接运行函数)
;;; 要求：多个函数可以整合成一个函数
;;; (defun fun () (fun* (ssget))) fun与fun*关系
;;;	----------------------------------------------------------------------------

;;;	-----------------------------------------------------
;;;	(setvar "orthomode" (abs (1- (getvar "orthomode"))))
;;;	选择集 selectset -> ssget
;;;	图元（对象） -> 
;;;	对象名 -> ssname ?
;;;	(car (entsel))
;;;	对象的定义数据ent -> (entget (car (entsel)))
;;;	assoc  cdr
;;;	-----------------------------------------------------


;;;	todo
;;;	标注
;;;	(command "dimaligned" '(1000 0 0) '(2000 0 0) '(0 -1000 0))
;;; 图块续编
;;; 文字编号
;;; 添加到块

;;; 改变颜色
(vla-put-color (vlax-ename->vla-object (car (entsel))) 8)
(VLAX-Dump-Object (vlax-ename->vla-object (car (entsel))) T)


;;;	视口切换
(defun pan-view (size-ctr / size ctr)
	(setq 
		size (car size-ctr)
		ctr (cdr size-ctr))
	(command "pan" ctr (getvar "viewctr"))
	(command "zoom" "s" (/ size (getvar "viewsize")))
	)
(defun set-view (Num / table setab)
	(defun table () 
		(cond 
			((= Num 0) (setab v0))
			((= Num 1) (setab v1))
			((= Num 2) (setab v2))
			((= Num 3) (setab v3))
			((= Num 4) (setab v4))
			((= Num 5) (setab v5))
			((= Num 6) (setab v6))
			((= Num 7) (setab v7))
			((= Num 8) (setab v8))
			((= Num 9) (setab v9))
			(T nil))
	(defun setab (view)
		(setq view (cons (getvar "viewsize") (getvar "viewctr"))))))
(defun d (/ a)
	(setq a (getint "input:"))
	(cond 
		((= nil a) (set-view 1))
		()))

;;;	可选参数
(defun define (fun (args) (express))
	(setq nil)
	(cond (nil) (defun )))
(define func (args) (express))


(defun wz-edit ()
	(initget 7 "Join ")
	(nil))

(defun C:8 (/ gp)
	;(setvar "cmdecho" 0)
	(setq gp (ssget))
	(if (/= gp nil) (command ".change" gp "" "p" "c" "8" ""))
	(princ)) 



;;; 计算表达式值
;;;	add setting of accuracy 
(defun calc-text (/ A i text text0 cutstr)
	(setq A (ssget))
	(defun cutstr (str)
		(substr str 1 (VL-String-Search "=" str)))
	(setq i 0)
	(repeat (sslength A)
		(progn
			(setq text0 (cutstr (get-obj-att (ssname A i) 1)))
			(setq text (vl-string-Translate "xX" "**" text0))
			(set-obj-att
				(ssname A i)
				1
				(strcat text0 "=" (rtos (cal text) 2 2)))
			(setq i (1+ i)))))


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

;;;	change-att
(defun c:catt () (change-att))
(defun change-att (/ num vl)
	(setq num (getint "\nDXF:"))
	(setq vl (getstring "\n Value:"))
	(set-obj-att (ssname (ssget) 0) num vl)
	(princ))
(defun c:catt2 () (change-att2))
(defun change-att2 (/ num vl)
	(setq num (getint "\nDXF:"))
	(setq vl (getstring "\n Value:"))
	(set-obj-att (ssname (ssget) 0) num vl)
	(princ))




;;;	--------------
;;;	函数库
;;;	--------------
(defun nil? (/ a) (= nil a))
(defun != (/ a b) (not (eq a b)))
;;;	get attribute of object
(defun get-obj-att (Obj num)
	(cdr (assoc num (entget Obj))))
;;;	set attribute of object
(defun set-obj-att (Obj num att)
	(entmod (subst
			(cons num att)
			(assoc num (entget Obj))
			(entget Obj))))
;;;	ssset->sslist
(defun ssset->sslist (setA / i ll)
	(setq i 0)
	(setq ll nil)
	(repeat (sslength setA)
		(setq ll (cons (ssname setA i) ll))
		(setq i (+ 1 i)))
	(car (cons ll nil)))
;;; sslist filter
(defun sslist-filter (sslist dxf value / ll) 
  (setq ll nil)
  (foreach each sslist 
    (if (= value (get-obj-att each dxf)) 
      (setq ll (cons each ll))
      t))
  (car (cons ll nil)))

;;; wzdj
(defun text-spacing (/ d sslist h y each) 
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT"))
  (setq d (if (setq d (getreal "输入行间距<defeat=0.4>：")) d 0.4))
  (setq sslist (vl-sort 
                 sslist
                 '(lambda (ent1 ent2) 
                    (> 
                      (car (cdr (get-obj-att ent1 10)))
                      (car (cdr (get-obj-att ent2 10)))))))
  (setq h (* (+ 1 d) (get-obj-att (car sslist) 40)))
  (setq y (car (cdr (get-obj-att (car sslist) 10))))
  (foreach each sslist 
    (set-obj-att 
      each
      10
      (cons (car (get-obj-att each 10)) 
            (cons y (cons 0 nil))))
    (setq y (- y h)))
  (princ))
  
  