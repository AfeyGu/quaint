;;;	柱定位

;;;	两点标注
(defun dim-2point (p1 p2 dist f / c-point)	;;;	p1 p2 点; dist 长度; f + - 顺时针或逆时针
	(defun c-point (c p1 p2 dist)
		(polar 
			p1
			(c (angle p1 p2) (/ 3.1415926535 2))
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
;;;测试 dim-onecolumn
;;; xdote: '(p1 p2) ,when p1: '(x y z)
(defun c:qqq () 
  ;(foreach each (entget (ssname (ssget) 0)) (if (= 10 (car each)) (princ (car (cdr (cdr each))))))
  (dim-onecolumn '((0 0 0) (1000 0 0)) '((0 100 0) (0 1000 0)) (ssname (ssget) 0) 1000 - +)
  )
;;; dim-onecolumn
;;; (setq qqq (dim-onecolumn '((0 0 0) (1000 0 0)) '((0 100 0) (0 1000 0)) (ssname (ssget) 0) 1000 - +))
(defun dim-onecolumn (xdote ydote rec dist f1 f2 / xlist ylist x y points1 points2)
  ;(setq ylist (list (car (cdr (car xdote)))))
  (setq ylist xdote)
  ;(setq xlist (list (car (car ydote))))
  (setq xlist ydote)
  (foreach each (entget rec)
    (if 
	  (= 10 (car each)) 
	  (progn 
	    ;(setq xlist (cons (car (cdr (cdr each))) xlist))
	    (if (member (car (cdr (cdr each))) xlist) nil (setq xlist (cons (car (cdr (cdr each))) xlist)))
		(if (member (car (cdr each)) ylist) nil (setq ylist (cons (car (cdr each)) ylist)))
		;(setq ylist (cons (car (cdr each)) ylist))
	  )
	  t))
  (setq xlist (deduplication (vl-sort xlist '<)))
  (setq ylist (deduplication (vl-sort ylist '<)))
  (princ xlist)
  (princ "\n")
  (princ ylist)
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
  (princ "\nlast\n")
  (princ points1)
  (princ "\n")
  ;(princ points2)
  (dim-list points1 dist f1)
  (dim-list points2 dist f2)
)

;;; dim-colum
(defun dim-colum (/ xdote ydote rec ll dote)
  (setq ll (ssset->sslist (ssget)))
  (setq dote (sslist-filter ll 0 "LINE"))
  (setq rec (car (sslist-filter ll 0 "LWPOLYLINE")))
  (if (= (car (get-obj-att (car dote) 10)) (car (get-obj-att (car dote) 11))) 
    (setq xdote (car (get-obj-att (car dote) 11))) 
	(setq ydote (car (get-obj-att (car (cdr dote)) 11))))
  (if (= (car (get-obj-att (car (cdr dote)) 10)) (car (get-obj-att (car (cdr dote)) 11))) 
    (setq xdote (car (get-obj-att (car (cdr dote)) 11))) 
	(setq ydote (car (get-obj-att (car dote) 11))))
	(dim-onecolumn xdote ydote rec 1000 1 +)
)
(defun c:asd () (dim-colum))

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
;;; 去重
(defun deduplication (alist / de)
  (defun de (alist)
    (cond
	  ((nil? (cdr alist)) alist)
	  ((in? (car alist) (cdr alist)) (de (cdr alist)))
	  (t (cons (car alist) (de (cdr alist))))
	)
  )
  (defun in? (num alist)
    (cond
	  ((nil? alist) nil)
	  ((equal num (car alist) 0.1) t)
	  (t (in? num (cdr alist)))
	)
  )
  (de alist)
)



;;; Math-------------------------------
;;; 直线交点
(defun 4point (p1 p2 p3 p4)
  nil
)
(defun c:lzdw () (load "C:\\Users\\Syue\\Desktop\\zdw.lsp"))
