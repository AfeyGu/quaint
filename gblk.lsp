;;; nil?
 (defun nil? (/ a) (= nil a))
 (defun != (/ a b) (not (eq a b)))
;	not eq
;;;	get attribute of object
 (defun get-obj-att (Obj num)
	(cdr (assoc num (entget Obj))))
;;;	set attribute of object
 (defun set-obj-att (Obj num att)
	(entmod
		(subst
			(cons num att)
			(assoc num (entget Obj))
			(entget Obj))))


;;;	quaint-set-var
(defun nil)

;;; 复合输入函数，自动判断数据类型
 (defun input (datatype inittext) ())


(defun get-near-beam (layer / p1 p2)
	(setq p1 (list
			(+ (car p) distan)
			(+ (car (cdr p)) distan)
			0)
	)
	(setq p2 (list
			(- (car p) distan)
			(- (car (cdr p)) distan)
			0)
	)
	(ssget "C" p1 p2 (list (cons 8 layer))))  ;;; 返回选择集
;|
(defun c:sgt
	(/ distan p1 p2 p)
	(setq distan 25)
	(setq p (getpoint "Select a point:   "))
	(princ p)
	(princ "\n")
	(setq p1 (list
			(+ (car p) distan)
			(+ (car (cdr p)) distan)
			0)
	)
	(setq p2 (list
			(- (car p) distan)
			(- (car (cdr p)) distan)
			0)
	)
	(sssetfirst nil (ssget "C" p1 p2))
	(princ))
|;