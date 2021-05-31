;;; 别名
;;; 选择
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
;;; 视口
(defun c:v0 () (ai_tiledvp 1 nil))  ;;; v0 v1 v2 v3 改变视口数量
(defun c:v1 () (command "-vports" "j"))
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
;;; 属性
(defun c:satt () (search-att))
(defun c:gatt () (get-att))
(defun c:22 () (setlayer0)) ;;; 设置当前图层为0
;;; 文本操作
(defun c:a1 () (align-textangle)) ;;; 文字旋转指定角度
(defun c:wzad () (text-join*))  ;;; 文字合并
(defun c:aq () (text-add-app*))
(defun c:fe () (text-copy*))
(defun c:wzdj () (text-spacing))
(defun c:wzbh () (continuous-identifier)) ;;; 文字编号修改
(defun c:ji () (calc-text)) ;;; 计算text内容
(defun c:jjj() (withclose "osmode" 0 calc-steel-area*)) ;;; 计算配筋面积并记录
(defun c:jj () (calc-steel-area))  ;;; 计算配筋面积
(defun c:regex() (select-matched-text)) ;;; 通过正则选择文字
(defun c:wzth() (replace-matched-text))
(defun c:rws() (rewrite-steel*)) ;;; 重写原位标注钢筋写法
(defun c:pop () (littlefilter)) ;;; Pop littler num
(defun c:ppp() (littlefilter*)) ;;; Pop littler num plus
;;; 块操作
(defun c:bb () (block-based-zero))  ;;; 以0为基点打块
(defun c:sbil () (search-block-inlayer*)) ;;; 选择图层上所有块
(defun c:gb () (copy-to-block)) ;;; 复制为块
(defun c:br () (random-named-block))  ;;; 定义为随机命名的块
(defun c:bg () (command "REFSET" "R"))
(defun c:bf () (command "REFSET" "A"))
(defun c:rs () (command "REFCLOSE" "S"))
;;; 图形操作
(defun c:kk () (breakatpoint)) ;;; breakatpoint
(defun c:k () (command "_.break" pause "f"))
(defun c:qe () (command "pedit" "M"))

;;  交选
;;  待加入空集判断
;;  解决选择完不显示的问题
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





;;; -----------------------------------------------------------------------
;;; 选择图层上所有块  
;;; 需要用list, 使用`不行
(defun search-block-inlayer (layer)
  (sssetfirst nil (ssget "X" (list (cons 0 "INSERT") (cons 8 layer)))))
(defun search-block-inlayer* ()
  (princ "\n Select layer:")
  (search-block-inlayer (get-obj-att (car (entsel)) 8)))
;;; 复制为块
(defun copy-to-block (/ A)
  (setq A (ssget))
  (command "copybase" (getpoint "指定基点：") A "")
  (command "pasteblock")
  (princ))
;;; 定义为随机命名的块
(defun random-named-block (/ A p)
  (setq A (ssget))
  (setq p (getpoint "指定基点："))
  (command "copybase" p A "")
  (command "pasteblock" p)
  (command "erase" A "")
  (princ))
;;; 以0为基点打块
(defun block-based-zero (/ A p)
  (setq A (ssget))
  (setq p (list 0 0 0))
  (command "copybase" p A "")
  (command "pasteblock" p)
  (command "erase" A "")
  (princ))
;;; -----------------------------------------------------------------------





;;; 文字操作 --------------------------------------------------------------
;;; 将文字旋转至所选角度
;;; 50-角度，51-倾斜角度
;;; TODO：选择对象
(defun align-textangle (/ A B i change-angle)
  (defun change-angle (new-rad ent-data)
    (entmod (subst
        (cons 50 new-rad)
        (assoc 50 ent-data)
        ent-data)))
  (setq A (ssget))
  (setq A (ssset->sslist A))
  (setq A (sslist-filter A 0 "TEXT"))
  (setq A (sslist->ssset A))
  (setq B (getangle "指定第一点:"))
  (setq i 0)
  (repeat (sslength A)
    (progn
      (change-angle B (entget (ssname A i)))
      (setq i (1+ i)))))

;;; 计算表达式值
;;; add setting of accuracy 
;;; 整数必须介于 2147483647 和 -2147483648 之间
(defun calc-text (/ A i e text text0 text1 cutstr)
  (setq A (ssget))
  (defun cutstr (str)
    (substr str 1 (VL-String-Search "=" str)))
  (setq i 0)
  (repeat (sslength A)
    (progn
      (setq text0 (get-obj-att (ssname A i) 1))
      (if (= "=" (substr text0 1 1))  ;;; try to move first "="
        (progn (setq e "=") (setq text1 (cutstr (substr text0 2)))) 
        (progn (setq e "") (setq text1 (cutstr text0))))
      (setq text (vl-string-Translate "xX" "**" text1)) ;;; subset x by * (not in cutstr, because we need use "xX" in strcar funcation)
      (set-obj-att
        (ssname A i)
        1
        (strcat e text1 "=" (rtos (cal (strcat "0.0+" text)) 2 2)))
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
;;;  文字复制
(defun text-copy (t1 t2)
  (set-obj-att t2 1 (get-obj-att t1 1)))
(defun text-copy* (/ tester es)
  (defun tester () 
    (setq es (car (entsel)))
    (if (= "TEXT" (get-obj-att es 0))
      es
      (progn 
        (princ "重新选取：")
        (tester)
        )))
  (text-copy (tester) (tester))
  (princ))
;;; 文字等行间距 
;;; 组码11为0 则改10 ，否则改11
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
;;; 编号连续
;;; 修改"1.xxxx"格式的文字的序号
(defun continuous-identifier (/ ss str id)
  (setq ss (ssset->sslist (ssget (list '(0 . "TEXT"))))) ; 选择文本
  (setq ss (sslist-filter* ss 1 
            (lambda (str) (test str "^\\d+\\."))
           )
  ) ; 过滤掉没编号的
  (setq ss (vl-sort
            ss
            '(lambda (ent1 ent2)
              (>
               (car (cdr (get-obj-att ent1 10)))
               (car (cdr (get-obj-att ent2 10)))
              )))) ; 按Y坐标排序
  (setq id 1)
  (foreach each ss
    (setq str (get-obj-att each 1))
    (setq str (replace str (strcat (rtos id) ".") "^\\d+\\."))
    (set-obj-att each 1 str)
    (setq id (+ 1 id)))
  (princ)
)
;;; Pop littler num
;;; todo: 加入范围选数的功能
(defun littlefilter (/ A ll num)
  (setq ll (ssset->sslist (ssget)))
  (setq num (getreal "\nNum:"))
  (setq A (ssadd))
  (setq ll (sslist-filter ll 0 "TEXT"))
  (setq ll (sslist-filter* ll 1 (lambda (x) (test x "^(-?\\d+)(\\.\\d+)?$")))) ; 过滤掉非数字
  (foreach each ll
    (if (<= (atof (get-obj-att each 1)) num) (ssadd each A) t))
  (sssetfirst nil A)
  (princ))
;;; Pop littler num plus
;;; todo 增加选择文字作为过滤对象的选项
;;; TODO 更合理的颜色
(setq pop-color-mode 0)
(defun littlefilter*(/ getlist int A ll c color)
  (setq ll (ssset->sslist (ssget)))
  (setq ll (sslist-filter ll 0 "TEXT"))
  (setq ll (sslist-filter* ll 1 (lambda (x) (test x "^(-?\\d+)(\\.\\d+)?$")))) ; 过滤掉非数字
  (defun color (c / cc c2) ; 改颜色
    (cond 
      ((= pop-color-mode 0) ; 使用真彩色
        (setq cc (hsl->rgb (* 250.0 (/ c 1.0 (length int))) 1 0.5))
        (setq c2 (strcat (rtos (car cc)) "," (rtos (cadr cc)) "," (rtos (caddr cc))))
        (command ".chprop" A "" "c" "t" c2 ""))
      (t (command ".CHPROP" A "" "c" (+ c 1) "")) ; 0-9
      ))
  (defun getlist()
    (setq int (getreal "\nNum:")) ; 实数也行
    (cond ((nil? int) nil)
      (t (cons int (getlist)))))
  ;(setq int (reverse (getlist)))
  (setq int (vl-sort (getlist) '>)) ; 从大到小排序
  (setq A (ssadd)) ; 大于最大的红色
  (foreach each ll
      (if (> (atof (get-obj-att each 1)) (car int)) (ssadd each A) t))
  (color 0)
  (setq c 1) ; 剩下的依次改颜色
  (foreach eachint int
    (setq A (ssadd))
    (foreach each ll
     (if (<= (atof (get-obj-att each 1)) eachint) (ssadd each A) t))
    (if (not (= 0 (sslength A)))
      (color c))
    (setq c (+ 1 c)))
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
;;; regex
;;; 查找符合条件内容的文字
;;; todo 添加内置匹配模式(新的匹配函数)
(setq select-matched-text-Pattern "") ; 默认值
(defun select-matched-text(/ Pattern A)
  (setq A (ssset->sslist (ssget)))
  (setq A (sslist-filter A 0 "TEXT")) ; 生成文字的选择列表
  (princ "Input Pattern <")
  (princ select-matched-text-Pattern)
  (princ ">:\n")
  (setq Pattern (read-line))
  (cond ((= "" Pattern) (setq Pattern select-matched-text-Pattern))
    (t (setq select-matched-text-Pattern Pattern))) ; 设置默认值
  (defun selecter(A)
    (cond ((nil? A) nil)
      ((test (get-obj-att (car A) 1) Pattern) (cons (car A) (selecter (cdr A))))
      (t (selecter (cdr A)))))
  (setq A (selecter A))
  (sssetfirst nil (sslist->ssset A))
  (princ "找到 ")
  (length A))
;;; wzth
;;; 替换匹配内容的文本
(defun replace-matched-text(/ Pattern sslist str)
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT")) ; 生成文字的选择列表
  (princ "Input Pattern:\n")
  (setq Pattern (read-line))
  (princ "Input Replace string:")
  (setq str (read-line))
  (foreach each sslist 
    (set-obj-att each 1 (replace (get-obj-att each 1) str Pattern)))
  (princ "已操作 ")
  (princ (length sslist))
  (princ)
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
            "$1*$2*$2*0.78539815" "([0-9]+)%%132([0-9]+)"))))
;;; 两个配筋面积之比 （0.3）
(defun c:lli (/ tester es calcer)
  (defun tester () 
    (setq es (car (entsel)))
    (if (= "TEXT" (get-obj-att es 0))
      es
      (progn 
        (princ "重新选取：")
        (tester)
        )))
  (defun calcer (txt)
    (cal 
      (replace* 
        (get-obj-att txt 1)
        (list "$1" " (/) " 
            "$1+$2" "([0-9]+%%132[0-9]+)/([0-9]+%%132[0-9]+)" 
            "" "[0-9]+/[0-9]+" 
            "$1*$2*$2*0.78539815" "([0-9]+)%%132([0-9]+)"))))
  (/ (calcer (tester)) (calcer (tester))))
;;; 转化为某一直径
;;; todo: 转为几种直径的最优解
(defun c:zh (/ phi tester es calcer)
  (defun tester () 
    (setq es (car (entsel)))
    (if (= "TEXT" (get-obj-att es 0))
      es
      (progn 
        (princ "重新选取：")
        (tester))))
  (defun calcer (txt)
    (cal 
      (replace* 
        (get-obj-att txt 1)
        (list "$1" " (/) " 
            "$1+$2" "([0-9]+%%132[0-9]+)/([0-9]+%%132[0-9]+)" 
            "" "[0-9]+/[0-9]+" 
            "$1*$2*$2*0.78539815" "([0-9]+)%%132([0-9]+)"))))
  (setq phi (getint "Num:"))
  (/ (calcer (tester)) (* 3.1415926 phi phi 0.25)))
;;; 计算配筋面积并记录
(defun calc-steel-area* (/ sslist area calcarea calc) 
  (setvar "cmdecho" 0)
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT"))
  (defun filter (text) (test text "^[0-9\(]+%%132[0-9]+"))
  (defun calc(str)
    (cal 
      (replace* 
        str
        (list "$1" " (/) " 
              "$1+$2" "([0-9]+%%132[0-9]+)/([0-9]+%%132[0-9]+)" 
              "" "[0-9]+/[0-9]+"
              "$1*$2*$2*0.78539815" "([0-9]+)%%132([0-9]+)"
        )
      )
    ))
  (defun calcarea (str / areaa)  ;计算面积
    (setq areaa "")
    (cond 
      ((not (test str "^[\(0-9]+%%132[0-9 %;/+\(\)]+$")) nil) ; G2%%13216 之类的
      ((and 
          (test str "([\(0-9]+%%132[0-9\)]+[/ +]*)+")
          (test str ";")) ; 带;的表达式
        (foreach each (execute- str "([\(0-9]+%%132[0-9\)]+[/ +]*)+")
          (setq areaa (strcat areaa (rtos (calc each)) "  "))))
      ((test str "([0-9]+%%132[0-9]+[/ +]*)+") ; 单个配筋的表达式
        (rtos (calc str)))
      (t nil)))
  (foreach each sslist 
    (setq area
      (calcarea (get-obj-att each 1)))
    (if (and (not (= nil area)) (test (get-obj-att each 1) "%%132")) 
      (command "text" 
               "s"
               "Standard"
               "tl"
               (get-obj-att each 10)
               200
               (* 57.29577951472 (get-obj-att each 50))
               area)
      t))
  (setvar "cmdecho" 0)
  (princ))
;;; 调整原位标注写法
(defun rewrite-steel*(/ sslist)
  (defun rewrite (str)
    (if (test str "^([ ]?[0-9]{1,2}%%132[0-9]{1,2}[ ]?[+/])+[ ]?[0-9]{1,2}%%132[0-9]{1,2}$") 
      (rewrite-steel str)
      str))
  (princ "选择对象：")
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT"))
  (foreach each sslist
    (set-obj-att each 1 (rewrite (get-obj-att each 1)))
    (princ (rewrite (get-obj-att each 1)))))
;;; rewrite steel
;;; (rewrite-steel "2%%13216+2%%13220 / 2%%13220") -> "4%%13220+2%%13216 4/2"
(defun rewrite-steel(strr / prefix prefixl suffix link linkadd steellist addeach)
  (setq str strr) ; 是否必要
  (defun linkadd (alist str)
    (cond ((= nil (cdr alist)) (itoa (cal (car alist))))
      (t (strcat (itoa (cal (car alist))) str (linkadd (cdr alist) str)))))
  (defun link (alist str)
    (cond ((= nil (cdr alist)) (car alist))
      (t (strcat (car alist) str (link (cdr alist) str)))))
  (setq suffix (linkadd (execute- (replace str "" "%%132[123][245680]")  "[0-9+?]+") "/"))
  ;(setq steellist (list "32" "28" "25" "22" "20" "18" "16" "14" "12"))
  (setq steellist (list "12" "14" "16" "18" "20" "22" "25" "28" "32"))
  (defun addeach (alist) 
    (cond 
      ((nil? alist) 0)
      (t (+ (atoi (car alist)) (addeach (cdr alist))))))
  (foreach each steellist 
    (setq str strr) ; 是否必要
    (if (test str (strcat "%%132" each))
      (setq prefixl (cons 
      (strcat 
        (itoa (addeach (execute- str (strcat "[0-9]{1,2}(?=%%132" each ")"))))
        "%%132" each)
      prefixl))
      (pass)
    ))
  (setq prefix (link prefixl "+"))
  (strcat prefix " " suffix))
;;; -----------------------------------------------------------------------




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
;;; (exist-layer? LayerName) 判断图层是否存在
(defun exist-layer? (LayerName)
	(if (tblsearch "LAYER" LayerName) T nil))
;;; 改图层名
;;; (rename-layer oldname newname)
(defun rename-layer (oldname newname)
	(if (exist-layer? oldname) (command "_.rename" "LAYER" oldname newname) nil)
	(command))
;;; 图层合并
;;; (Layer-Merge A B)
(defun layer-merge (A B)
	(command "laymrg" "N" A "" "N" B "Y"))
;;; change-layer-name 改图层名
;;; (change-layer-name test-pattern replace-pattern newname)
;;; (setq newer (replace layername newname pattern))
(defun change-layer-name (test-pattern replace-pattern newname / layerlist layername newer)
	(setvar "clayer" "0")
	(setq layerlist (table "Layer" 2))
	(foreach layername layerlist
		(if (and (test layername test-pattern) (null (= layername "0"))) ; 如果图名匹配
			(progn
				(setq newer (replace layername newname replace-pattern)) ; 新图名
				(if (tblsearch "LAYER" newer) 
					(layer-merge layername newer) ; 新图名图层存在，合并图层
					(rename-layer layername newer))) ; 新图名图层不存在，重命名
			nil)
	)
)
;;; 添加 -LEON
; (replace "asd" "^(?!(.*)-LEON$)" "$1+2")
(defun C:LEON+ ()
	(change-layer-name "^(?!(.*)-LEON$)" "^(.*)$" "$1-LEON")
	(print))
;;; 删除 -LEON
(defun C:LEON- ()
	(change-layer-name "(.*)-leon$" "(.*)-leon$" "$1")
	(print))
;;; -----------------------------------------------------------------------





;;; -----------------------------------------------------------------------
;;; 图形
;;; Break with First option at 1 point
(defun breakatpoint ()
  (command "_.break" pause "_first" pause "@"))
;;; -----------------------------------------------------------------------



;;; -----------------------------------------------------------------------
;;; 改变颜色
(defun C:7 (/ gp co) ;设置为红色
  ;(setvar "cmdecho" 0)
  (setq gp (ssget))
  (if (/= gp nil) (command ".change" gp "" "p" "c" 1 ""))
  (princ))
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
(defun C:9 (/ gp co) ;设置为30
  ;(setvar "cmdecho" 0)
  (setq gp (ssget))
  (if (/= gp nil) (command ".change" gp "" "p" "c" 30 ""))
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
;;; -----------------------------------------------------------------------








;;; -----------------------------------------------------------------------
;;; 函数库
;;; nil?
(defun nil? (a) (= nil a))
(defun != (/ a b) (not (eq a b))) ; not eq
(defun self (a) return a)
(defun pass () return nil)
(defun round (num n) (if (<= n 0) (atoi (rtos num 2 n)) (atof (rtos num 2 n))))
;;; princs
(defun princs (clist)
  (foreach each clist (princ each))
  (princ))
;;; HSL->RGB
;;; (hsl->rgb h s l) -> a list (r g b)
(defun hsl->rgb (h s l / tc colorc q p hk rgb)
  (setq hk (/ h 360.0))
  (setq q (if (< l 0.5) (* l (+ 1 s)) (+ l s (- (* l s)))))
  (setq p (- (* 2 l) q))
  (defun tc (x)
    (cond ((< x 0) (+ 1.0 x))
      ((> x 1) (- x 1.0))
      (t x)))
  (defun colorc (x)
    (cond ((<= x (/ 1.0 6)) (+ p (* 6 x (- q p)))) ; x<1/6
      ((< (/ 1.0 6) x 0.5) q) ; 1/6<x<0.5
      ((<= 0.5 x (/ 2.0 3)) (+ p (* 6 (- (/ 2.0 3) x) (- q p))))  ; 0.5<=x<=1/3
      (t p)))
  (cond ((= 0 s) (list (round (* 255 l) 0) (round (* 255 l) 0) (round (* 255 l) 0)))
    (t ; 这里不能用else，会返回nil，上同
      (list 
        (round (* 255 (colorc (tc (+ hk (/ 1.0 3))))) 0)
        (round (* 255 (colorc (tc hk))) 0)
        (round (* 255 (colorc (tc (- hk (/ 1.0 3))))) 0)
        )))
  )
;;; color-value
;;; (color-value 255 255 255) -> 16777215
(defun color-value (r g b)
  (+ r (* 256 g) (* 65536 b)))
;;; get attribute of object
(defun get-obj-att (Obj num)
  (cdr (assoc num (entget Obj))))
;;; set attribute of object
(defun set-obj-att (Obj num att)
  (entmod (subst
      (cons num att)
      (assoc num (entget Obj))
      (entget Obj))))
;;; ssset->sslist
(defun ssset->sslist (setA / i ll)
  (setq i 0)
  (setq ll nil)
  (repeat (sslength setA)
    (setq ll (cons (ssname setA i) ll))
    (setq i (+ 1 i)))
  (car (cons ll nil)))
;;; sslist->ssset
;;; (sssetfirst nil (sslist->ssset (ssset->sslist (ssget))))
(defun sslist->ssset (sslist / ssset)
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
;;; sslist filter plus
(defun sslist-filter* (sslist dxf filter / ll)
  (setq ll nil)
  (foreach each sslist ; 应该可以改用mapcar
    (if (filter (get-obj-att each dxf))
      (setq ll (cons each ll))
      t))
  return ll)
;;; (table  table-name DXF)   返回符号表某属性的列表
;;; table-name 可取"LAYER"、"LTYPE"、"VIEW"、"STYLE"、"BLOCK"、"UCS"、"APPID"、"DIMSTYLE" 和 "VPORT"
;;; 例 (table "LAYER" 2) 返回所有图名的列表
;;; (null D) 当最后一个条目后，返回T，否则返回nil
(defun table (S dxf / D R) 
	(while (setq D (tblnext S (null D)))
		(setq R (cons (cdr (assoc dxf D)) R))))
;;; withclose 
(defun withclose (mode value fun / orgvalue)
  (setq orgvalue (getvar mode))
  (setvar mode value)
  (fun)
  (setvar mode orgvalue)
  (princ))
;;; regex 正则 --------------------------------------------------------------
;;; 注：\d 要写成\\d (写函数时必要，cad中使用似乎不用)
(setq Global 1)
(setq IgnoreCase 1)
;;; replace 方法
;;; 列出匹配项 位置，长度，内容
;;; (replace "1.2.1." "a." "^1.") -> "a.2.1."
(defun replace (Str1 Str2 Pattern / nstr reg) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg "IgnoreCase" IgnoreCase)
  (vlax-put-property reg "Global" Global)
  (vlax-put-property reg "Pattern" Pattern)
  (setq nstr (vlax-invoke-method reg "Replace" Str1 Str2))
  (vlax-release-object reg)
  return nstr)
;;; replace* 方法
(defun replace* (str alist) 
  (cond 
    ((nil? alist) str)
    ((nil? (cdr alist)) nil)
    (t (replace* (replace str (car alist) (cadr alist)) (cddr alist)))))
;;; Execute 方法
(defun execute (Str1 Pattern / nstr each l pos len str reg) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg 'IgnoreCase 1)
  (vlax-put-property reg 'Global 1)
  (vlax-put-property reg 'Pattern Pattern)
  ;(vlax-put-property reg "IgnoreCase" IgnoreCase) 这个写法旧版CAD无法使用
  ;(vlax-put-property reg "Global" Global)
  ;(vlax-put-property reg "Pattern" Pattern)
  (setq nstr (vlax-invoke-method reg "Execute" Str1))
  (vlax-release-object reg)
  (vlax-for each 
            nstr
            (setq pos (vlax-get-property each "FirstIndex")
                  len (vlax-get-property each "Length")
                  str (vlax-get-property each "value"))
            (setq l (cons (list pos len str) l)))
  (reverse l))
;;; execute- 方法
;;; 生成只有匹配结果的列表
(defun execute- (Str1 Pattern / nstr each l reg) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg 'IgnoreCase 1)
  (vlax-put-property reg 'Global 1)
  (vlax-put-property reg 'Pattern Pattern)
  (setq nstr (vlax-invoke-method reg "Execute" Str1))
  (vlax-release-object reg)
  (vlax-for each 
            nstr
            (setq str (vlax-get-property each "value"))
            (setq l (cons str l)))
  (reverse l))
;;; test 方法
;;; (test string pattern)
(defun test (Str1 Pattern / nstr reg) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg 'IgnoreCase 1)
  (vlax-put-property reg 'Pattern Pattern)
  (setq nstr (eq :vlax-true (vlax-invoke-method reg "Test" Str1)))
  (vlax-release-object reg)
  return nstr)

;;; -----------------------------------------------------------------------
;;; Load
(defun c:quaint () (load "quaint.lsp"))
(setvar "cmdecho" 0)
(command "cal" nil)
(setvar "cmdecho" 1)
(princ "\nQuaint 已加载。\n")
(princ)
