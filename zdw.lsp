;;;	柱定位
	
(defun dim-column (obj-rectangle obj-line-x obj-line-y)
	(setq p1 p2 p3 p4)
	(setq 
		x (car (get-obj-att obj-line-x 10))
		y (car (cdr get-obj-att obj-line-y 10)))
	(din-2point p1 (list (car p1) y 0) 1000 +)
	(din-2point p2 (list (car p2) y 0) 1000 +)
	(din-2point p2 (list x (car (cdr p2)) 0) 1000 -)
	(din-2point p3 (list x (car (cdr p3)) 0) 1000 -)
	)
;;;	两点标注
(defun dim-2point (p1 p2 dist f)	;;;	p1 p2 点; dist 长度; f + - 顺时针或逆时针
	(defun c-point (c p1 p2 dist)
		(polar 
			p1
			(c (angle p1 p2) (/ pi 2))
			dist))
	(command "dimaligned" p1 p2 (c-point f p1 p2 dist)))



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



 (
	(-1 . <图元名: 1cbb68e80a0>)
	(0 . LWPOLYLINE)
	(330 . <图元名: 1cba17741f0>)
	(5 . 2BA)
	(100 . AcDbEntity)
	(67 . 0)
	(410 . Model)
	(8 . 0)
	(100 . AcDbPolyline)
	(90 . 4)
	(70 . 1)
	(43 . 0.0)
	(38 . 0.0)
	(39 . 0.0)
	(10 55975.2 1234.0)
	(40 . 0.0)
	(41 . 0.0)
	(42 . 0.0)
	(91 . 0)
	(10 33849.0 1234.0)
	(40 . 0.0)
	(41 . 0.0)
	(42 . 0.0)
	(91 . 0)
	(10 33849.0 21099.4)
	(40 . 0.0)
	(41 . 0.0)
	(42 . 0.0)
	(91 . 0)
	(10 55975.2 21099.4)
	(40 . 0.0)
	(41 . 0.0)
	(42 . 0.0)
	(91 . 0)
	(210 0.0 0.0 1.0)
)