;;;	����
;;;	ѡ��
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
;;;	�ӿ�
(defun c:v0 () (ai_tiledvp 1 nil))	;;;	v0 v1 v2 v3 �ı��ӿ�����
(defun c:v1 () (command "-vports" "j"))
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
(defun c:ji () (calc-text))	;;;	����text����
;;;	����
(defun c:satt () (search-att))
(defun c:gatt () (get-att))
;;;	�ı�����
(defun c:a1 () (align-textangle))	;;;	������תָ���Ƕ�
(defun c:wzad () (text-join*))	;;;	���ֺϲ�
(defun c:wzap () (text-add-app*))
(defun c:wzc () (text-copy*))
;;;	�����
(defun c:sbil () (search-block-inlayer*))	;;;	ѡ��ͼ�������п�
(defun c:gb () (copy-to-block))	;;;	����Ϊ��
(defun c:br () (radom-named-block))	;;;	����Ϊ��������Ŀ�


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
(defun radom-named-block (/ A p)
	(setq A (ssget))
	(setq p (getpoint "ָ�����㣺"))
	(command "copybase" p A "")
	(command "pasteblock" p)
	(command "erase" A "")
	(princ))




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
(defun text-copy* ()
	(text-copy (car (entsel)) (car (entsel)))
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