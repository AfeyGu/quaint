;;	����	
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
(defun c:a1 () (align-textangle))
(defun c:satt () (search-att))
(defun c:v1 () (command "-vports" "j"));;;	v1 v2 v3 �ı��ӿ�
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
(defun c:ji () (calc-text))
(defun c:gatt () (get-att))
(defun c:wzad () (text-join*))
(defun c:wzap () (text-add-app*))


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

;;; ��ѯ��ѡ��������
(defun search-att ()
	(princ (entget (ssname (ssget) 0))))
;;; ��ѯ��ѡ�����Ӧ����	
(defun get-att (/ num)
	(setq num (getint "\nDXF:"))
	(princ (get-obj-att (ssname (ssget) 0) num)))




;;; nil?
(defun nil? (/ a) (= nil a))
;;;	get attribute of object
(defun get-obj-att (Obj num)
	(cdr (assoc num (entget Obj))))
;;;	set attribute of object
(defun set-obj-att (Obj num att)
	(entmod (subst
			(cons num att)
			(assoc num (entget Obj))
			(entget Obj)))) 