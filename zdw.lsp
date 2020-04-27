;;;	柱定位
	
(defun dim-2point (p1 p2 dist f)	;;;	p1 p2 点; dist 长度; f + - 顺时针或逆时针
	(defun c-point (c p1 p2 dist)
		(polar 
			p1
			(c (angle p1 p2) (/ pi 2))
			dist))
	(command "dimaligned" p1 p2 (c-point f p1 p2 dist)))



