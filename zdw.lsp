;;;	柱定位
; (defun dim-column (obj-rectangle obj-line-x obj-line-y)
	; (setq p1 p2 p3 p4)
	; (setq 
		; x (car (get-obj-att obj-line-x 10))
		; y (car (cdr get-obj-att obj-line-y 10)))
	; (din-2point p1 (list (car p1) y 0) 1000 +)
	; (din-2point p2 (list (car p2) y 0) 1000 +)
	; (din-2point p2 (list x (car (cdr p2)) 0) 1000 -)
	; (din-2point p3 (list x (car (cdr p3)) 0) 1000 -)
	; )
;;;	两点标注
(defun dim-2point (p1 p2 dist f / c-point)	;;;	p1 p2 点; dist 长度; f + - 顺时针或逆时针
	(defun c-point (c p1 p2 dist)
		(polar 
			p1
			(c (angle p1 p2) (/ pi 2))
			dist))
	(command "dimaligned" p1 p2 (c-point f p1 p2 dist))
	t)
;;; 列表标注
;;; (dim-list (list '(0 0 0) '(0 1000 0) '(0 2000 0) '(0 3000 0) '(0 4000 0)) 1000 +)
(defun dim-list (alist dist f / dl) 
  (setvar "cmdecho" 0)
  (defun dl (alist) 
    (if 
	  (nil? (cdr alist)) 
	  nil
	  (progn 
	    (dim-2point (car alist) (car (cdr alist)) dist f)
		(dl (cdr alist)))))
  (dl alist)
  (setvar "cmdecho" 1)
  (princ))
;;;
;;; xdote: '(p1 p2) ,when p1: '(x y z)
(foreach each (entget (ssname (ssget) 0)) (if (= 10 (car each)) (princ (car (car (cdr each))))))
;;; (dim-onecolumn '((0 0 0) (1000 0 0)) '((0 100 0) (0 1000 0)) (ssname (ssget) 0) 1000 + +)
(defun dim-onecolumn (xdote ydote rec dist f1 f2 / xlist ylist x y points1 points2)
  (setq xlist (list (car (cdr (car ydote)))))
  (setq ylist (list (car (car xdote))))
  (foreach each (entget rec)
    (if 
	  (= 10 (car each)) 
	  (progn 
	    ;(setq xlist (cons (car (cdr (cdr each))) xlist))
	    (if (member (car (cdr (cdr each))) xlist) t (setq xlist (cons (car (cdr (cdr each))) xlist)))
		(if (member (car (cdr each)) ylist) t (setq ylist (cons (car (cdr each)) ylist)))
		;(setq ylist (cons (car (cdr each)) ylist))
	  )
	  t))
  (setq xlist (vl-sort xlist '<))
  (setq ylist (vl-sort ylist '<))
  (cond 
    ((and (= f1 -) (= f2 +)) (setq x (car ylist)) (setq y (car xlist)))
	(nil nil)
	(nil nil)
	(nil nil)
  )
  (setq points1 nil)
  (setq points2 nil)
  (foreach each xlist
    (setq points1 (cons (list x each 0) points1)))
  (foreach each ylist
    (setq points2 (cons (list each y 0) points2)))
  (princ "last")
  (princ xlist)
  (princ ylist)
  (dim-list points1 dist f1)
  (dim-list points2 dist f2)
)



;;;	--------------
;;;	函数库
;;;	--------------
;;; nil?
(defun nil? (a) (= nil a))
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
;;; princs
(defun princs (clist)
	(foreach each clist (princ each))
	(princ))
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




;;; Math-------------------------------
;;; 直线交点
(defun 4point (p1 p2 p3 p4)
  nil
)