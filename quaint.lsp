;;;	����
;;;	ѡ��
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
;;;	�ӿ�
(defun c:v0 () (ai_tiledvp 1 nil))	;;;	v0 v1 v2 v3 �ı��ӿ�����
(defun c:v1 () (command "-vports" "j"))
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
;;;	����
(defun c:satt () (search-att))
(defun c:gatt () (get-att))
(defun c:22 () (setlayer0))	;;; ���õ�ǰͼ��Ϊ0
;;;	�ı�����
(defun c:a1 () (align-textangle))	;;;	������תָ���Ƕ�
(defun c:wzad () (text-join*))	;;;	���ֺϲ�
(defun c:aq () (text-add-app*))
(defun c:wzc () (text-copy*))
(defun c:wzdj () (text-spacing))
(defun c:ji () (calc-text))	;;;	����text����
(defun c:jjj() (withclose "osmode" 0 calc-steel-area*)) ;;; ��������������¼
(defun c:jj () (calc-steel-area))  ;;; ����������
(defun c:wzzz() (select-matched-text)) ;;; ͨ������ѡ������
(defun c:wzth() (replace-matched-text))
(defun c:rws() (rewrite-steel*)) ;;; ��дԭλ��ע�ֽ�д��
(defun c:pop () (littlefilter)) ;;; Pop littler num
(defun c:ppp() (littlefilter*)) ;;; Pop littler num plus
;;;	�����
(defun c:bb () (block-based-zero))	;;;	��0Ϊ������
(defun c:sbil () (search-block-inlayer*))	;;;	ѡ��ͼ�������п�
(defun c:gb () (copy-to-block))	;;;	����Ϊ��
(defun c:br () (random-named-block))	;;;	����Ϊ��������Ŀ�
(defun c:bg () (command "REFSET" "R"))
(defun c:bf () (command "REFSET" "A"))
(defun c:rs () (command "REFCLOSE" "S"))
;;; ͼ�β���
(defun c:kk () (breakatpoint)) ;;; breakatpoint
(defun c:k () (command "_.break" pause "f"))

;;	��ѡ
;;	������ռ��ж�
;;	���ѡ���겻��ʾ������
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
;;;	ѡ��ͼ�������п�  
;;; ��Ҫ��list, ʹ��`����
(defun search-block-inlayer (layer)
	(sssetfirst nil (ssget "X" (list (cons 0 "INSERT") (cons 8 layer)))))
(defun search-block-inlayer* ()
	(princ "\n Select layer:")
	(search-block-inlayer (get-obj-att (car (entsel)) 8)))
;;;	����Ϊ��
(defun copy-to-block (/ A)
	(setq A (ssget))
	(command "copybase" (getpoint "ָ�����㣺") A "")
	(command "pasteblock")
	(princ))
;;;	����Ϊ��������Ŀ�
(defun random-named-block (/ A p)
	(setq A (ssget))
	(setq p (getpoint "ָ�����㣺"))
	(command "copybase" p A "")
	(command "pasteblock" p)
	(command "erase" A "")
	(princ))
;;;	��0Ϊ������
(defun block-based-zero (/ A p)
	(setq A (ssget))
	(setq p (list 0 0 0))
	(command "copybase" p A "")
	(command "pasteblock" p)
	(command "erase" A "")
	(princ))
;;;	-----------------------------------------------------------------------





;;;	���ֲ��� --------------------------------------------------------------
;;; ��������ת����ѡ�Ƕ�
;;;	50-�Ƕȣ�51-��б�Ƕ�
;;;	TODO��ѡ�����
(defun align-textangle (/ A B i change-angle)
	(defun change-angle (new-rad ent-data)
		(entmod (subst
				(cons 50 new-rad)
				(assoc 50 ent-data)
				ent-data)))
	(setq A (ssget))
	(setq B (getangle "ָ����һ��:"))
	(setq i 0)
	(repeat (sslength A)
		(progn
			(change-angle B (entget (ssname A i)))
			(setq i (1+ i)))))

;;; ������ʽֵ
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

;;; ���ֺϲ�
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
;;; ���ּ�����
(defun text-add-app(obj)
	(set-obj-att obj 1 (strcat "(" (get-obj-att obj 1) ")")))
(defun text-add-app* ()
	(text-add-app (car (entsel)))
	(text-add-app*))
;;;	 ���ָ���
(defun text-copy (t1 t2)
	(set-obj-att t2 1 (get-obj-att t1 1)))
(defun text-copy* (/ tester es)
  (defun tester () 
    (setq es (car (entsel)))
    (if (= "TEXT" (get-obj-att es 0))
      es
      (progn 
        (princ "����ѡȡ��")
        (tester)
        )))
	(text-copy (tester) (tester))
	(princ))
;;; ���ֵ��м�� 
;;; ����11Ϊ0 ���10 �������11
(defun text-spacing (/ d sslist h y each) 
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT"))
  (setq d (if (setq d (getreal "�����м��<defeat=0.4>��")) d 0.4))
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
;;;	Pop littler num
(defun littlefilter (/ A ll num)
  (setq ll (ssset->sslist (ssget)))
  (setq num (getint "Num:"))
  (setq A (ssadd))
  (setq ll (sslist-filter ll 0 "TEXT"))
  (foreach each ll
    (if (<= (atoi (get-obj-att each 1)) num) (ssadd each A) t))
  (sssetfirst nil A)
  (princ))
;;; Pop littler num plus
(defun littlefilter*(/ getlist int A ll c)
  (setq ll (ssset->sslist (ssget)))
  (setq ll (sslist-filter ll 0 "TEXT"))
  (defun getlist()
    (setq int (getint "Int:"))
    (cond ((nil? int) nil)
      (t (cons int (getlist)))))
  (setq int (reverse (getlist)))
  (setq A (ssadd)) ; �������ĺ�ɫ
  (foreach each ll
      (if (> (atoi (get-obj-att each 1)) (car int)) (ssadd each A) t))
  (command ".CHPROP" A "" "c" 1 "")
  (setq c 2) ; ʣ�µ����θ���ɫ
  (foreach eachint int
    (setq A (ssadd))
    (foreach each ll
     (if (<= (atoi (get-obj-att each 1)) eachint) (ssadd each A) t))
    (princ (sslength A))
    (command ".CHPROP" A "" "c" c "")
    (setq c (+ 1 c))))
;;; ������ͬ�����ַ�
(defun c:sszf(/ A s)
	(princ "Input:\n")
	(setq s (get-obj-att (ssname (ssget) 0) 1))
	(princ "Input Area:")
	(setq A (ssset->sslist (ssget)))
	(setq A (sslist-filter A 1 s))
	(sssetfirst nil (sslist->ssset A))
	(princ))
;;; wzzz
;;; ���ҷ����������ݵ�����
(defun select-matched-text(/ Pattern A)
	(setq A (ssset->sslist (ssget)))
	(setq A (sslist-filter A 0 "TEXT")) ; �������ֵ�ѡ���б�
  (princ "Input Pattern:\n")
  (setq Pattern (read-line))
  (defun selecter(A)
    (cond ((nil? A) nil)
      ((test (get-obj-att (car A) 1) Pattern) (cons (car A) (selecter (cdr A))))
      (t (selecter (cdr A)))))
  (setq A (selecter A))
  (sssetfirst nil (sslist->ssset A))
  (princ "�ҵ� ")
  (length A))
;;; wzth
;;; �滻ƥ�����ݵ��ı�
(defun replace-matched-text(/ Pattern sslist str)
	(setq sslist (ssset->sslist (ssget)))
	(setq sslist (sslist-filter sslist 0 "TEXT")) ; �������ֵ�ѡ���б�
  (princ "Input Pattern:\n")
  (setq Pattern (read-line))
  (princ "Input Replace string:")
  (setq str (read-line))
  (foreach each sslist 
    (set-obj-att each 1 (replace (get-obj-att each 1) str Pattern)))
  (princ "�Ѳ��� ")
  (princ (length sslist))
  (princ)
)
;;; ����������
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
;;; ��������������¼
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
  (defun calcarea (str / areaa)  ;�������
    (setq areaa "")
    (cond 
      ((not (test str "^[\(0-9]+%%132[0-9 %;/+\(\)]+$")) nil) ; G2%%13216 ֮���
      ((and 
          (test str "([\(0-9]+%%132[0-9\)]+[/ +]*)+")
          (test str ";")) ; ��;�ı��ʽ
        (foreach each (execute- str "([\(0-9]+%%132[0-9\)]+[/ +]*)+")
          (setq areaa (strcat areaa (rtos (calc each)) "  "))))
      ((test str "([0-9]+%%132[0-9]+[/ +]*)+") ; �������ı��ʽ
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
;;; ����ԭλ��עд��
(defun rewrite-steel*(/ sslist)
  (defun rewrite (str)
    (if (test str "^([ ]?[0-9]{1,2}%%132[0-9]{1,2}[ ]?[+/])+[ ]?[0-9]{1,2}%%132[0-9]{1,2}$") 
      (rewrite-steel str)
      str))
  (princ "ѡ�����")
  (setq sslist (ssset->sslist (ssget)))
  (setq sslist (sslist-filter sslist 0 "TEXT"))
  (foreach each sslist
    (set-obj-att each 1 (rewrite (get-obj-att each 1)))
    (princ (rewrite (get-obj-att each 1)))))
;;; rewrite steel
;;; (rewrite-steel "2%%13216+2%%13220 / 2%%13220") -> "4%%13220+2%%13216 4/2"
(defun rewrite-steel(strr / prefix prefixl suffix link linkadd steellist addeach)
  (setq str strr) ; �Ƿ��Ҫ
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
    (setq str strr) ; �Ƿ��Ҫ
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
;;;	-----------------------------------------------------------------------




;;; ͼ����� ---------------------------------------------------------------
;;; ���õ�ǰͼ��Ϊ0
(defun setlayer0 ()
	(setvar "clayer" "0")
	;(command "-layer" "s" "0" "")
	(princ))
;;; �ر�����
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
;;; ͼ��ȫ��
(DEFUN C:openalllayer()
	(COMMAND "-LAYER" "T" "*" "ON" "*" "")
	(princ))
;;; �ر�ͼ��
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
;;; ͼ��
;;; Break with First option at 1 point
(defun breakatpoint ()
  (command "_.break" pause "_first" pause "@"))
;;;	-----------------------------------------------------------------------



;;;	-----------------------------------------------------------------------
;;;	�ı���ɫ
(defun C:7 (/ gp co) ;����Ϊ��ɫ
  ;(setvar "cmdecho" 0)
  (setq gp (ssget))
  (if (/= gp nil) (command ".change" gp "" "p" "c" 1 ""))
  (princ))
(defun C:8 (/ gp co)
	;(setvar "cmdecho" 0)
	(setq gp (ssget))
	(setq co (if (= "" (setq co (getstring "������ɫ��"))) "ByLayer" co))
	(if (/= gp nil) (command ".change" gp "" "p" "c" co ""))
	(princ))
(defun C:88 (/ gp co) ;����Ϊ��ɫ
	;(setvar "cmdecho" 0)
	(setq gp (ssget))
	(if (/= gp nil) (command ".change" gp "" "p" "c" 8 ""))
	(princ))
(defun C:9 (/ gp co) ;����Ϊ30
  ;(setvar "cmdecho" 0)
  (setq gp (ssget))
  (if (/= gp nil) (command ".change" gp "" "p" "c" 30 ""))
  (princ))
;;; ��ѯ��ѡ��������
(defun search-att ()
	(princ (entget (ssname (ssget) 0)))
	(princ))
;;; ��ѯ��ѡ�����Ӧ����	
(defun get-att (/ num)
	(setq num (getint "\nDXF:"))
	(princ (get-obj-att (ssname (ssget) 0) num))
	(princ))
;;;	-----------------------------------------------------------------------








;;;	-----------------------------------------------------------------------
;;;	������
;;; nil?
(defun nil? (a) (= nil a))
(defun != (/ a b) (not (eq a b))) ;	not eq
(defun self (a) return a)
(defun pass () return nil)
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
;;; ����ĳһ�������ssslist�����ط�������������
;;; e.g. (sslist-filter sslist 0 "TEXT")  ���˲���"TEXT"���͵Ķ���
(defun sslist-filter (sslist dxf value / ll) 
  (setq ll nil)
  (foreach each sslist 
    (if (= value (get-obj-att each dxf)) 
      (setq ll (cons each ll))
      t))
  (car (cons ll nil)))
;;; sslist filter plus
(defun sslist-filter* (sslist dxf value filter / ll)
  (setq ll nil)
  (foreach each sslist 
    (if (filter (get-obj-att each dxf))
      (setq ll (cons each ll))
      t))
  return ll)
;;; withclose 
(defun withclose (mode value fun / orgvalue)
  (setq orgvalue (getvar mode))
  (setvar mode value)
  (fun)
  (setvar mode orgvalue)
  (princ))
;;; regex ���� --------------------------------------------------------------
;;; ע��\d Ҫд��\\d
(setq Global 1)
(setq IgnoreCase 1)
;;; replace ����
;;; �г�ƥ���� λ�ã����ȣ�����
(defun replace (Str1 Str2 Pattern / nstr reg) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg "IgnoreCase" IgnoreCase)
  (vlax-put-property reg "Global" Global)
  (vlax-put-property reg "Pattern" Pattern)
  (setq nstr (vlax-invoke-method reg "Replace" Str1 Str2))
  (vlax-release-object reg)
  return nstr)
;;; replace* ����
(defun replace* (str alist) 
  (cond 
    ((nil? alist) str)
    ((nil? (cdr alist)) nil)
    (t (replace* (replace str (car alist) (cadr alist)) (cddr alist)))))
;;; Execute ����
(defun execute (Str1 Pattern / nstr each l pos len str reg) 
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
  (reverse l))
;;; execute- ����
;;������ֻ��ƥ�������б�
(defun execute- (Str1 Pattern / nstr each l reg) 
  (setq reg (vlax-create-object "Vbscript.RegExp"))
  (vlax-put-property reg "IgnoreCase" IgnoreCase)
  (vlax-put-property reg "Global" Global)
  (vlax-put-property reg "Pattern" Pattern)
  (setq nstr (vlax-invoke-method reg "Execute" Str1))
  (vlax-release-object reg)
  (vlax-for each 
            nstr
            (setq str (vlax-get-property each "value"))
            (setq l (cons str l)))
  (reverse l))
;;; test ����
(defun test (Str1 Pattern / nstr reg) 
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
(princ "\nQuaint �Ѽ��ء�\n")
(princ)