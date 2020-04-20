;;	����	
(defun c:zzselect () (c:xselect))
(defun c:xs () (c:xselect))
(defun c:a1 () (align-textangle))
(defun c:satt () (search-att))
(defun c:v1 () (command "-vports" "j"));;;	v1 v2 v3 �ı��ӿ�
(defun c:v2 () (ai_tiledvp 2 "_V"))
(defun c:v3 () (command "-vports" "3" "V"))
(defun c:ji () (calc-text))


;;	��ѡ
;;	������ռ��ж�
;;	���ѡ���겻��ʾ������
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


;;; ��������ת����ѡ�Ƕ�
;;;	50-�Ƕȣ�51-��б�Ƕ�
;;;	TODO��ѡ�����
(defun align-textangle (/ A B)
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


;;;	�����ı����ʽֵ
(defun calc-text (/ A)
	(setq A (ssget))
	(cal (cdr (assoc 1 (entget (ssname A 0))))))


;;; ��ѯ��ѡ��������
(defun search-att ()
	(princ (entget (ssname (ssget) 0)))) 