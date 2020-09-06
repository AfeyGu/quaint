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
(defun c:22 () (setlayer0))	;;; 设置当前图层为0
(defun c:bk () (breakatpoint))
;;;	文本操作
(defun c:a1 () (align-textangle))	;;;	文字旋转指定角度
(defun c:wzad () (text-join*))	;;;	文字合并
(defun c:aq () (text-add-app*))
(defun c:wzc () (text-copy*))
(defun c:wzdj () (text-spacing))
(defun c:jj () (calc-steel-area))  ;;; 计算配筋面积
(defun c:wzzz() (select-matched-text)) ;;; 通过正则选择文字
;;;	块操作
(defun c:bb () (block-based-zero))	;;;	以0为基点打块
(defun c:sbil () (search-block-inlayer*))	;;;	选择图层上所有块
(defun c:gb () (copy-to-block))	;;;	复制为块
(defun c:br () (random-named-block))	;;;	定义为随机命名的块
(defun c:bg () (command "REFSET" "R"))
(defun c:bf () (command "REFSET" "A"))
(defun c:rs () (command "REFCLOSE" "S"))
;;; 图形操作
(defun c:bk () (breakatpoint)) ;;; breakatpoint


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





;;;	-----------------------------------------------------------------------
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
(defun random-named-block (/ A p)
	(setq A (ssget))
	(setq p (getpoint "指定基点："))
	(command "copybase" p A "")
	(command "pasteblock" p)
	(command "erase" A "")
	(princ))
;;;	以0为基点打块
(defun block-based-zero (/ A p)
	(setq A (ssget))
	(setq p (list 0 0 0))
	(command "copybase" p A "")
	(command "pasteblock" p)
	(command "erase" A "")
	(princ))
;;;	-----------------------------------------------------------------------





;;;	文字操作 --------------------------------------------------------------
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
;;;	add setting of accuracy 
(defun calc-text (/ A i e text text0 text1 cutstr)
	(setq A (ssget))
	(defun cutstr (str)
		(substr str 1 (VL-String-Search "=" str)))
	(setq i 0)
	(repeat (sslength A)
		(progn
			(setq text0 (get-obj-att (ssname A i) 1))
			(if (= "=" (substr text0 1 1))	;;;	try to move first "="
				(progn (setq e "=") (setq text1 (cutstr (substr text0 2)))) 
				(progn (setq e "") (setq text1 (cutstr text0))))
			(setq text (vl-string-Translate "xX" "**" text1))	;;;	subset x by * (not in cutstr, because we need use "xX" in strcar funcation)
			(set-obj-att
				(ssname A i)
				1
				(strcat e text1 "=" (rtos (cal text) 2 2)))
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
;;; 文字等行间距
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
;;;	pop littler num
(defun c:popp () (littlefilter))
(defun littlefilter ()
  (setq ll (ssset->sslist (ssget)))
  (setq num (getint "Num:"))
  (setq A (ssadd))
  (setq ll (sslist-filter ll 0 "TEXT"))
  (foreach each ll
    (if (< (atoi (get-obj-att each 1)) num) (ssadd each A) t))
  (sssetfirst nil A)
  (princ))
;;; 搜索相同内容字符
(defun c:sszf(/ A s)
	(princ "Input:\n")
	(setq s (get-obj-att (ssname (ssget) 0) 1))
	(princ "Input Area:")
	(setq A (ssset->sslist (ssget)))
	(setq A (sslist-filter A 1 s))
	(sssetfirst nil (sslist->ssset A))
	(princ))
;;; wzzz
;;; 查找符合条件内容的文字
(defun select-matched-text(/ Pattern A)
	(setq A (ssset->sslist (ssget)))
	(setq A (sslist-filter A 0 "TEXT")) ; 生成文字的选择列表
  (princ "Input Pattern:\n")
  (setq Pattern (read-line))
  (defun selecter(A)
    (cond ((nil? A) nil)
      ((test (get-obj-att (car A) 1) Pattern) (cons (car A) (selecter (cdr A))))
      (t (selecter (cdr A)))))
  (sssetfirst nil (sslist->ssset (selecter A))))
;;; wzth
;;; 替换匹配内容的文本
(defun c:wzth() (replace-matched-text))
(defun replace-matched-text(/ Pattern sslist str)
	(setq sslist (ssset->sslist (ssget)))
	(setq sslist (sslist-filter sslist 0 "TEXT")) ; 生成文字的选择列表
  (princ "Input Pattern:\n")
  (setq Pattern (read-line))
  (princ "Input Replace string:")
  (setq str (read-line))
  (foreach each sslist 
    (set-obj-att each 1 (replace (get-obj-att each 1) str Pattern)))
)
;;; 计算配筋面积
(defun calc-steel-area (/ sslist)
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT"))
  (cal 
    (replace* 
      (get-obj-att (car sslist) 1)
      (list "$1" " (/) " 
            "$1+$2" "([0-9]+%%132[0-9]+)/([0-9]+%%132[0-9]+)" 
            "" "[0-9]+/[0-9]+" 
            "$1*$2*$2*0.78539815" "([0-9]+)%%132([0-9]+)"
      )
    )
  )
)
;;;	-----------------------------------------------------------------------




;;; 图层操作 ---------------------------------------------------------------
;;; 设置当前图层为0
(defun setlayer0 ()
	(setvar "clayer" "0")
	;(command "-layer" "s" "0" "")
	(princ))
;;; 关闭其他
(Defun C:toffotherlayer (/ SS CNT LAY LAYLST VAL)
  (setvar "cmdecho" 0)
  (if (not (setq SS (ssget "i")))
    (progn
      (setq SS (ssget))))
  (if SS
    (progn
      (setq CNT 0)
      (while (setq LAY (ssname SS CNT))
        (setq LAY (cdr (assoc 8 (entget LAY))))
        (if (not (member LAY LAYLST))
          (setq LAYLST (cons LAY LAYLST)))
        (setq CNT (1+ CNT)))
      (if (member (getvar "CLAYER") LAYLST)
        (setq LAY (getvar "CLAYER"))
        (setvar "CLAYER" (setq LAY (last LAYLST))))
      (command "_.-LAYER" "_OFF" "*" "_Y")
      (foreach VAL LAYLST (command "_ON" VAL))
      (command "")))
  (setvar "cmdecho" 1)
  (princ))
;;; 图层全开
(DEFUN C:openalllayer()
	(COMMAND "-LAYER" "T" "*" "ON" "*" "")
	(princ))
;;; 关闭图层
(Defun C:tofflayer (/ SS CNT LAY LAYLST VAL CLAYER)
  (setvar "cmdecho" 0)
  (setq CLAYER (getvar "CLAYER"))
  (if (not (setq SS (ssget "i")))
    (progn
      (setq SS (ssget))))
  (if SS
    (progn
      (setq CNT 0)
      (while (setq LAY (ssname SS CNT))
        (setq LAY (cdr (assoc 8 (entget LAY))))
        (if (not (member LAY LAYLST))
          (setq LAYLST (cons LAY LAYLST)))
        (setq CNT (1+ CNT)))
      (command "_.-LAYER")
      (foreach VAL LAYLST (command "_f" VAL))
      (foreach VAL LAYLST 
	    (if (= VAL CLAYER) (command "_off" VAL "Y") (command "_off" VAL)))
      (command "")))
  (setvar "cmdecho" 1)
  (princ))
;;;	-----------------------------------------------------------------------





;;;	-----------------------------------------------------------------------
;;; 图形
;;; Break with First option at 1 point
(defun breakatpoint ()
  (command "_.break" pause "_first" pause "@"))
;;;	-----------------------------------------------------------------------



;;;	-----------------------------------------------------------------------
;;;	改变颜色
(defun C:8 (/ gp co)
	;(setvar "cmdecho" 0)
	(setq gp (ssget))
	(setq co (if (= "" (setq co (getstring "输入颜色："))) "ByLayer" co))
	(if (/= gp nil) (command ".change" gp "" "p" "c" co ""))
	(princ))
(defun C:88 (/ gp co) ;设置为灰色
	;(setvar "cmdecho" 0)
	(setq gp (ssget))
	(if (/= gp nil) (command ".change" gp "" "p" "c" 8 ""))
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
;;;	-----------------------------------------------------------------------








;;;	-----------------------------------------------------------------------
;;;	函数库
;;; nil?
(defun nil? (a) (= nil a))
(defun != (/ a b) (not (eq a b))) ;	not eq
;;; princs
(defun princs (clist)
	(foreach each clist (princ each))
	(princ))
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
;;; sslist->ssset
;;; (sssetfirst nil (sslist->ssset (ssset->sslist (ssget))))
(defun sslist->ssset(sslist / ssset)
  (setq ssset (ssadd))
  (defun ssiter (sslist)
    (cond ((nil? sslist) nil)
      (t (progn (ssadd (car sslist) ssset) (ssiter (cdr sslist))))
    ))
  (ssiter sslist)
  return ssset)
;;; sslist filter
;;; 根据某一组码过滤ssslist，返回符合条件的内容
;;; e.g. (sslist-filter sslist 0 "TEXT")  过滤不是"TEXT"类型的对象
(defun sslist-filter (sslist dxf value / ll) 
  (setq ll nil)
  (foreach each sslist 
    (if (= value (get-obj-att each dxf)) 
      (setq ll (cons each ll))
      t))
  (car (cons ll nil)))
;;; regex 正则 --------------------------------------------------------------
;;; 注：\d 要写成\\d
(setq Global 1)
(setq IgnoreCase 1)
;;; replace 方法
(defun replace (Str1 Str2 Pattern / nstr) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg "IgnoreCase" IgnoreCase)
  (vlax-put-property reg "Global" Global)
  (vlax-put-property reg "Pattern" Pattern)
  (setq nstr (vlax-invoke-method reg "Replace" Str1 Str2))
  (vlax-release-object reg)
  return
  nstr)
;;; replace* 方法
(defun replace* (str alist) 
  (cond 
    ((nil? alist) str)
    ((nil? (cdr alist)) nil)
    (t (replace* (replace str (car alist) (cadr alist)) (cddr alist)))))
;;; execute 方法
;;; 列出匹配项 位置，长度，内容
(defun execute (Str1 Pattern / nstr each l) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg "IgnoreCase" IgnoreCase)
  (vlax-put-property reg "Global" Global)
  (vlax-put-property reg "Pattern" Pattern)
  (setq nstr (vlax-invoke-method reg "Execute" Str1))
  (vlax-release-object reg)
  (vlax-for each 
            nstr
            (setq pos (vlax-get-property each "FirstIndex")
                  len (vlax-get-property each "Length")
                  str (vlax-get-property each "value"))
            (setq l (cons (list pos len str) l)))
  return l)
;;; test 方法
(defun test (Str1 Pattern / nstr) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg "IgnoreCase" IgnoreCase)
  (vlax-put-property reg "Pattern" Pattern)
  (setq nstr (eq :vlax-true (vlax-invoke-method reg "Test" Str1)))
  (vlax-release-object reg)
  return nstr)
;;;	-----------------------------------------------------------------------
;;; Load
(defun c:quaint () (load "quaint.lsp"))
(setvar "cmdecho" 0)
(command "cal" nil)
(setvar "cmdecho" 1)
(princ "\nQuaint 已加载。\n")
(princ)