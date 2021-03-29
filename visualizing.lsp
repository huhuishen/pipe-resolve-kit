;;; 绘图与统计材料模块分开 求解器生成不同数据适应各自工作 18-4-18
;;; 2021-2-8 更新以适应有实体管件的版本
(defun psk-get-customlayername (id / name)
  (setq name (cadr (assoc id $psk-layer-config)))

  (if (null name)
    (setq name id) ;_ 自定义图层未在layer-configration中定义时，采用图层索引号做为名称
  )
  name
)

;;;
(defun psk-set-customlayerbyid	(id / conf layer name regen)
  (setq	name (psk-get-customlayername id)
	conf (assoc id $psk-layer-config)
  )

  (p-layer-get name (cddr conf))

  (setq $addnew-layer name)
)


;;;;;; TODO: 当前层锁定时 添加失败


;;; BOOKMARK - 管件绘制

;;; 绘制双线风管
(defun psk-draw-duct (p1 p2 d / a)
  (setq a (angle p1 p2))
  (p-make-line
    (polar p1 (- a $pi/2) (* 0.5 d))
    (polar p2 (- a $pi/2) (* 0.5 d))
  )
  (p-make-line
    (polar p1 (+ a $pi/2) (* 0.5 d))
    (polar p2 (+ a $pi/2) (* 0.5 d))
  )
)
;;;  绘制双线弯头 p弯头两端管道中心线交点, a1,2 两端管道方向, d 管径, r 弯头半径
;;;  (psk-draw-elbow (getpoint) 0 $pi/2 500. 400.)
(defun psk-draw-elbow (p a1 a2 d r / a3 a4 a5 d1 p1 p2 p3 l)
  (setq	a3 (p-angle-regular (/ (- a2 a1) 2.0))
	d1 (abs (/ r (p-tan a3)))
	p1 (polar p a1 d1) ;_ p1 p2 弯头两管端
	p2 (polar p a2 d1)
	p3 (polar p (+ a1 a3) (abs (/ r (sin a3)))) ;_ 两管端垂线交点
	a4 (angle p3 p1)
	a5 (angle p3 p2)
	l  (* 0.5 d)
  )
  ;; 两端直线
  (p-make-line
    (polar p1 (- a1 $pi/2) (+ $PSK-duct-flextend l))
    (polar p1 (+ a1 $pi/2) (+ $PSK-duct-flextend l))
  )
  (p-make-line
    (polar p2 (- a2 $pi/2) (+ $PSK-duct-flextend l))
    (polar p2 (+ a2 $pi/2) (+ $PSK-duct-flextend l))
  )
  ;; 内外弧线
  (p-make-sharparc p3 (- r (* 0.5 d)) a4 a5)
  (p-make-sharparc p3 (+ r (* 0.5 d)) a4 a5)
)
;;; TODO: 图层锁定处理
;;;
;;;
;;;  绘制异径管, p1 p2两端点, a 两端管道方向, w1 w2 管径
;;;  (psk-draw-reducer  (getpoint) (getpoint) 0. 500. 250.)
(defun psk-draw-reducer	(p1 p2 a w1 w2 / p11 p12 p21 p22)
  (setq	p11 (polar p1 (- a $pi/2) (* 0.5 w1))
	p12 (polar p1 (+ a $pi/2) (* 0.5 w1))
	p21 (polar p2 (- a $pi/2) (* 0.5 w2))
	p22 (polar p2 (+ a $pi/2) (* 0.5 w2))
  )
  (p-make-line (polar p11 (- a $pi/2) $PSK-duct-flextend) (polar p12 (+ a $pi/2) $PSK-duct-flextend))
  (p-make-line (polar p21 (- a $pi/2) $PSK-duct-flextend) (polar p22 (+ a $pi/2) $PSK-duct-flextend))
  (p-make-line p11 p21)
  (p-make-line p12 p22)
)
;;; 垂直燕尾三通 （两分支均与主管垂直）
;;; (psk-draw-tee (getpoint)(getpoint)(getpoint) $pi/2 0 pi 1000 500 500 400 400)
(defun psk-draw-tee (p1 p2 p3 a1 a2 a3 d1 d2 d3 erf / a4 a8 c l1 l2 l3 p4 p5 p6 x y)
  (setq	l1 (* erf d2)
	l2 (+ (* (- erf 0.5) d2) (* 0.5 d1))
	l3 (+ (* (- erf 0.5) d3) (* 0.5 d1))
  )

  (setq	p4 (polar p2 a1 l1)
	p5 (polar p3 a1 (* erf d3))

	a4 (p-angle-reverse a1)
	a8 (angle p4 p5)

	c  (distance p4 p5)
	x  (/ (- (* (* (+ erf 0.5) d2) (* (+ erf 0.5) d2))
		 (* (* (+ erf 0.5) d3) (* (+ erf 0.5) d3))
		 (* c (- c))
	      )
	      c
	      2.
	   )
	y  (- (* (* (+ erf 0.5) d2) (* (+ erf 0.5) d2)) (* x x))
  )

  (if (> y 0)
    (if	(or (< (- pi) (- a1 a8) 0) (< pi (- a1 a8) $2pi))
      (setq p6 (polar (polar p4 a8 x) (+ $pi/2 a8) (sqrt y)))
      (setq p6 (polar (polar p4 a8 x) (- a8 $pi/2) (sqrt y)))
    )
    (setq p6 p1)
  )

  ;; 两端直线
  (p-make-line
    (polar p1 a2 (+ (* 0.5 d1) $PSK-duct-flextend))
    (polar p1 a3 (+ (* 0.5 d1) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p2 a1 (+ (* 0.5 d2) $PSK-duct-flextend))
    (polar p2 a1 (- (+ (* 0.5 d2) $PSK-duct-flextend)))
  )
  (p-make-line
    (polar p3 a1 (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 a1 (- (+ (* 0.5 d3) $PSK-duct-flextend)))
  )

  (if (not (equal d3 d2 1.))
    (p-make-line
      (polar p1 a3 (* 0.5 d1))
      (polar p5 a2 (* (- erf 0.5) d3))
    )
  )

  (p-make-sharparc p4 (* (- erf 0.5) d2) a3 a4)
  (p-make-sharparc p4 (* (+ erf 0.5) d2) (angle p4 p6) a4)
  (p-make-sharparc p5 (* (- erf 0.5) d3) a4 a2)
  (p-make-sharparc p5 (* (+ erf 0.5) d3) a4 (angle p5 p6))
)
;;;
;;;
;;; 三通 1 2在一直线上 3与其垂直
;; (psk-draw-tee-s '(0 0) '(0 360) '(310 160) (- $pi/2) $pi/2 0 500 400 200 0.8 "C")
(defun psk-draw-tee-s (p1 p2 p3 a1 a2 a3 d1 d2 d3 erf al / a3r l p11 p12 p21 p22 p4 p5 x)
  (setq	a3r (p-angle-reverse a3)
	p11 (polar p1 a3r (* 0.5 d1))
	p12 (polar p1 a3 (* 0.5 d1))

	p21 (polar p2 a3r (* 0.5 d2))
	p22 (polar p2 a3 (* 0.5 d2))

	l   (+ (* (- erf 0.5) d3) (* 0.5 d1))

	p4  (polar p1 a3 l)
  )


  (cond	((or (and (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
		  (= "R" al)
	     )
	     (and (equal 0. (p-angle-include (+ a1 $pi/2) a3) 1e-3)
		  (= "L" al)
	     )
	 )
	 (setq x  (+ (- d1 d2) (* (- erf 0.5) d3))
	       x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	       x  (+ 100. (* (+ erf 0.5) d3) (- x))
	       p5 (polar p22 a1 x)
	 )
	)
	((or (and (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
		  (= "L" al)
	     )
	     (and (equal 0. (p-angle-include (+ a1 $pi/2) a3) 1e-3)
		  (= "R" al)
	     )
	 )
	 (setq x  (* (- erf 0.5) d3)
	       x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	       x  (+ 100. (* (+ erf 0.5) d3) (- x))
	       p5 (polar p22 a1 x)
	 )
	)
	(t
	 (setq x  (+ (* (- erf 0.5) d3) (/ d1 2.) (/ d2 -2.))
	       x  (sqrt (abs (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x))))
	       x  (+ 100. (* (+ erf 0.5) d3) (- x))
	       p5 (polar p22 a1 x)
	 )
	)
  )

  ;; 接管口
  (p-make-line
    (polar p1 (- a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
    (polar p1 (+ a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p2 (+ a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
    (polar p2 (- a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p3 (- a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 (+ a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
  )

  ;; 直通段边线
;;;  (if (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;    (progn (p-make-line p21 p5)
;;;	   (p-make-line p22 p12)
;;;    )
;;;    (progn (p-make-line p21 p11)
;;;	   (p-make-line p22 p5)
;;;    )
;;;  )
  (p-make-line p11 p21)
  (p-make-line p22 p5)

  ;; 垂直段弧线
  (p-make-sharparc p4 (* (- erf 0.5) d3) a2 (p-angle-reverse a3))
  (p-make-sharparc p4 (* (+ erf 0.5) d3) a2 (angle p4 p5))
)
;;; 绘制直管上垂直分支处的外观
(defun psk-draw-branch (p3 a1 a3 d1 d3 erf / a2 p4 p5 x)
  (setq	p4 (polar p3 a1 (* erf d3))
	x  (* (- erf 0.5) d3)
	x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	x  (- x (* erf d3))
	a2 (p-angle-reverse a1)
	p5 (polar (polar p3 (p-angle-reverse a3) (* (- erf 0.5) d3)) a2 x)
  )

  ;; 接管口
  (p-make-line
    (polar p3 (- a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 (+ a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
  )

  ;; 垂直段弧线
  (p-make-sharparc p4 (* (- erf 0.5) d3) a2 (p-angle-reverse a3))
  (p-make-sharparc p4 (* (+ erf 0.5) d3) a2 (angle p4 p5))
)
;;; 任意角度三通
;;;(defun psk-draw-tee-s3 (a1 a2 a3 d1 d2 d3 erf)
;;;  (setq
;;;    a4	(abs (- pi (abs (- a1 a2))))
;;;    a5	(abs (- pi (abs (- a1 a3))))
;;;
;;;    l1	(/ (- (* erf d2) (* (cos a4) (+ (* (- erf 0.5) d2) (* 0.5 d1)))) (sin a4))
;;;    l2	(if (equal a4 $pi/2 1e-6)
;;;	  (+ (* (- erf 0.5) d2) (* 0.5 d1))
;;;	  (- (* (p-tan a4) erf d2) (/ l1 (cos a4)))
;;;	)
;;;
;;;    l1a	(/ (- (* erf d3) (* (cos a5) (+ (* (- erf 0.5) d3) (* 0.5 d1)))) (sin a5))
;;;    l3	(if (equal a5 $pi/2 1e-6)
;;;	  (+ (* (- erf 0.5) d3) (* 0.5 d1))
;;;	  (- (* (p-tan a5) erf d3) (/ l1a (cos a5)))
;;;	)
;;;
;;;    p1	(polar p a1 l1)
;;;    p2	(polar p a2 l2)
;;;    p3	(polar p a3 l3)
;;;  )
;;;)
(defun psk-draw-cross
       (p1 p2 p3 p4 a1 a2 a3 a4 d1 d2 d3 d4 erf / p11 p12 p21 p22 p5 p6 p7 p8 x pad l)
  (setq	p11 (polar p1 a3 (* 0.5 d1))
	p12 (polar p1 a4 (* 0.5 d1))

	p21 (polar p2 a3 (* 0.5 d2))
	p22 (polar p2 a4 (* 0.5 d2))

	p5  (polar p1 a3 (+ (* (- erf 0.5) d3) (* 0.5 d1)))
	p7  (polar p1 a4 (+ (* (- erf 0.5) d4) (* 0.5 d1)))

	pad (* erf (abs (- d4 d3)))
	l   (+ 100. (* (+ erf 0.5) (max d3 d4)))
  )

  (if (> d4 d3)
    (setq p5 (polar p5 a2 pad))
    (setq p7 (polar p7 a2 pad))
  )

  (setq	x  (+ (* (- erf 0.5) d3) (/ d1 2.) (/ d2 -2.))
	x  (sqrt (- (p-sqr (* (+ erf 0.5) d3)) (p-sqr x)))
	x  (- l
	      x
	      (if (> d4 d3)
		pad
		0.
	      )
	   )
;;;	p6 (if (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;	     (polar p21 a1 x)
;;;	     (polar p22 a1 x)
;;;	   )
	p6 (polar p21 a1 x)

	x  (+ (* (- erf 0.5) d4) (/ d1 2.) (/ d2 -2.))
	x  (sqrt (- (p-sqr (* (+ erf 0.5) d4)) (p-sqr x)))
	x  (- l
	      x
	      (if (> d3 d4)
		pad
		0.
	      )
	   )
;;;	p8 (if (equal 0. (p-angle-include (- a1 $pi/2) a4) 1e-3)
;;;	     (polar p21 a1 x)
;;;	     (polar p22 a1 x)
;;;	   )
	p8 (polar p22 a1 x)
  )


  ;; 接管口
  (p-make-line
    (polar p1 (- a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
    (polar p1 (+ a1 $pi/2) (+ (* 0.5 d1) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p2 (+ a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
    (polar p2 (- a2 $pi/2) (+ (* 0.5 d2) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p3 (- a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
    (polar p3 (+ a3 $pi/2) (+ (* 0.5 d3) $PSK-duct-flextend))
  )
  (p-make-line
    (polar p4 (- a4 $pi/2) (+ (* 0.5 d4) $PSK-duct-flextend))
    (polar p4 (+ a4 $pi/2) (+ (* 0.5 d4) $PSK-duct-flextend))
  )

  ;; 直通段边线
;;;  (if (equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;    (progn
;;;      (p-make-line p21 p6)
;;;      (p-make-line p22 p8)
;;;    )
;;;    (progn
;;;      (p-make-line p21 p8)
;;;      (p-make-line p22 p6)
;;;    )
;;;  )
  (p-make-line p21 p6)
  (p-make-line p22 p8)

  (if (> pad 0.)
    (if	(> d4 d3)
      (p-make-line p11 (polar p11 a2 pad))
      (p-make-line p12 (polar p12 a2 pad))
    )
  )
  
;;;  (if (> d3 d4)
;;;    (if	(equal 0. (p-angle-include (- a1 $pi/2) a3) 1e-3)
;;;      (p-make-line p12 (polar p12 a2 pad))
;;;      (p-make-line p11 (polar p11 a2 pad))
;;;    )
;;;  )

  ;; 垂直段弧线
  (p-make-sharparc p5 (* (- erf 0.5) d3) a2 (p-angle-reverse a3))
  (p-make-sharparc p5 (* (+ erf 0.5) d3) a2 (angle p5 p6))

  (p-make-sharparc p7 (* (- erf 0.5) d4) a2 (p-angle-reverse a4))
  (p-make-sharparc p7 (* (+ erf 0.5) d4) a2 (angle p7 p8))
)
;;;;;;(defun c:tt (/ en p ss view)
;;;;;;  (d-solve-selection (ssget '((-3 ("MYPROPS_ROUTER,MYPROPS_PARTSRC")))))
;;;;;;  (setq view (p-get-viewdir))
;;;;;;  (setq p (trans (getpoint "指定基点:")  1 0) 
;;;;;;	p (p-wcs->view p view))
;;;;;;
;;;;;;  (setq	en (entlast)
;;;;;;	ss (ssadd)
;;;;;;  )
;;;;;;
;;;;;;  (foreach e $PSK-solve-result
;;;;;;    (d-part-draw e view)
;;;;;;  )
;;;;;;
;;;;;;  (while (setq en (entnext en))
;;;;;;    (ssadd en ss)
;;;;;;  )
;;;;;;
;;;;;;  (command "_.MOVE" ss "" p)
;;;;;;;;;  (d-solve-selection (ssget '((-3 ("MYPROPS_ROUTER,MYPROPS_PART")))))
;;;;;;  (princ)
;;;;;;)
(defun psk-comp-draw (comp / d en p ports pts r uid)
  (psk-set-customlayerbyid
    (strcat (psk-path-getservice comp) "-DUCT")
  )

  (setq	en	      (p-get comp -1)
	uid	      (cons 1071 (p-uid))
	$addnew-xdata (list "PSK-DRAFT" uid)
  )

  (cond	((= "PATH" (psk-comp-getname comp))
	 (setq pts (p-dxf en '(10 11))
	       r   (psk-path-getportsize comp)
	 )

	 (psk-draw-duct
	   (car pts)
	   (cadr pts)
	   (if (vl-consp r)
	     ;; 风管宽
	     (car r)
	     r
	   )
	 )
	)
	((= "KEYPACK" (psk-comp-getname comp))
	  (psk-keypack-draw comp)
	)
	((= "FIT" (psk-comp-getname comp))
	 (setq tp (psk-comp-gettype comp))
	 (cond ((= "ELBOW" tp)
		(setq p	    (p-dxf en 10)
		      ports (psk-comp-getports comp)
		      pts   (mapcar 'psk-port-getangle ports)
		      d	    (psk-port-getsize (car ports))
		      d	    (if	(vl-consp d)
			      (car d)
			      d
			    )
		      ;;TODO
		      r	    (* (p-get comp "ERF") d)
		)
		(psk-draw-elbow p (car pts) (cadr pts) d r)
	       )
	       ((= "REDUCER" tp)
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-reducer
		   (psk-port-getpos (car ports))
		   (psk-port-getpos (last ports))
		   (psk-port-getangle (last ports))
		   (car (psk-port-getsize (car ports)))
		   (car (psk-port-getsize (last ports)))
		 )
	       )
	       ((= "TEE" tp)
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-tee
		   (psk-port-getpos (car ports))
		   (psk-port-getpos (cadr ports))
		   (psk-port-getpos (last ports))
		   (psk-port-getangle (car ports))
		   (psk-port-getangle (cadr ports))
		   (psk-port-getangle (last ports))
		   (car (psk-port-getsize (car ports)))
		   (car (psk-port-getsize (cadr ports)))
		   (car (psk-port-getsize (last ports)))
		   (p-get comp "ERF")
		 )
	       )
	       ((= "TEE-S" tp)
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-tee-s
		   (psk-port-getpos (car ports))
		   (psk-port-getpos (cadr ports))
		   (psk-port-getpos (last ports))
		   (psk-port-getangle (car ports))
		   (psk-port-getangle (cadr ports))
		   (psk-port-getangle (last ports))
		   (car (psk-port-getsize (car ports)))
		   (car (psk-port-getsize (cadr ports)))
		   (car (psk-port-getsize (last ports)))
		   (p-get comp "ERF")
		   (p-get comp "AL")
		 )
	       )
	       ((= "BRANCH" tp)
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-branch
		   (psk-port-getpos (cadr ports))
		   (psk-port-getangle (car ports))
		   (psk-port-getangle (cadr ports))
		   (car (psk-port-getsize (car ports)))
		   (car (psk-port-getsize (cadr ports)))
		   (p-get comp "ERF")
		 )
	       )
	       ((= "CROSS" tp)
		 (setq p     (p-dxf en 10)
		       ports (psk-comp-getports comp)
		 )
		 (psk-draw-cross
		   (psk-port-getpos (car ports))
		   (psk-port-getpos (cadr ports))
		   (psk-port-getpos (caddr ports))
		   (psk-port-getpos (last ports))
		   (psk-port-getangle (car ports))
		   (psk-port-getangle (cadr ports))
		   (psk-port-getangle (caddr ports))
		   (psk-port-getangle (last ports))
		   (car (psk-port-getsize (car ports)))
		   (car (psk-port-getsize (cadr ports)))
		   (car (psk-port-getsize (caddr ports)))
		   (car (psk-port-getsize (last ports)))
		   (p-get comp "ERF")
		 )
	       )
	 )
	)
  )
  (p-xdata-set en "PSK-DRAFT-OWNER" (list uid))
  (setq $addnew-xdata nil)
)


;; (psk-comps-draw (psk-comps-ssget))
(defun psk-comps-draw (comps /)
  (foreach comp	comps
    (psk-comp-draw comp)
  )
)
;;





;;; (psk-comp-getdrafts (psk-comp-load (car (entsel))))
(defun psk-comp-getdrafts (comp / r uid)
  (setq uid (cdar (p-xdata-get (p-get comp -1) "PSK-DRAFT-OWNER")))
  (foreach draft (psk-drafts-fromviewport)
    (if	(= uid (cdar (p-xdata-get draft "PSK-DRAFT")))
      (setq r (cons draft r))
    )
  )
  r
)


(defun psk-drafts-fromviewport ()
  (p-ss->enames
    (ssget "W" (trans (getvar "EXTMIN") 0 1) (trans (getvar "EXTMAX") 0 1) '((-3 ("PSK-DRAFT"))))
  )
)

;;;(psk-comp-redraw (psk-comp-load (car (entsel))))
(defun psk-comp-redraw (comp)
  (if comp
    (progn
      (foreach e (psk-comp-getdrafts comp)
	(entdel e)
      )
      (psk-comp-draw comp)
    )
  )
)




(defun d-draw-text (p text a / h)
  (setq a (rem a pi))
  (if (and (> a (* pi 0.51)) ;_ about 5 degree tolerance
	   (< a (* pi 1.51))
      )
    ;; 文字旋转超过90度但小于270度时要求将文字旋转180度以防止文字倒置
    (setq a (+ pi a))
  )
  (setq	h (* $psk-IDEN-TEXTHEIGHT $PSK-iden-scale)
	  ;; 文字偏离基线
	  ;;	p (polar p (+ (/ pi 2.0) a) (/ h 4.0))
  )
  (p-make-text text p "M" h $PSK-iden-widfactor a)
)
;;
;; (psk-comp-iden (psk-comp-load (car (entsel))))
(defun psk-comp-iden (comp / p ps str tmpl)
  ;; 采用entmake方式创建图层后首次执行显示颜色不正常或不显示的问题解决
  (psk-set-customlayerbyid
    (strcat (p-get comp "SERV") "-IDEN")
  )

  (setq str (p-get comp "TYPE"))
  (cond
    ((= "DUCT-ROUND" str)
     (setq tmpl "%%C{DD}")
    )
    ((= "DUCT-RECT" str)
     (setq tmpl "{W}x{H}")
    )
    ((= "PIPE" str)
     (setq tmpl "DN{DN}")
    )
  )

  (if (setq str (p-template-eval tmpl comp))
    (progn
      (setq ps (mapcar 'cadr (psk-path-getports comp))
	    p  (p-mid (car ps) (cadr ps))
      )

      (if (> (distance (car ps) (cadr ps)) $psk-iden-minlength)
	(progn
	  (setq en (d-draw-text p str (angle (car ps) (cadr ps))))
	  (if (null (setq uid
			   (cdar (p-xdata-get (p-get comp -1) "PSK-DRAFT-OWNER")
			   )
		    )
	      )
	    (progn
	      (setq uid (p-uid))
	      (p-xdata-set (p-get comp -1) "PSK-DRAFT-OWNER" (list (cons 1071 uid)))
	    )
	  )
	  (p-xdata-set en "PSK-IDEN" (list (cons 1071 uid)))
	)
      )
    )
  )
)
(defun psk-cmd-iden (/ comp comps done skip)
  (princ "\n选择需要标注的管线:")

  (p-startundomark)
  (setq	skip 0
	done 0
  )

  (foreach e (p-ss->enames
	       (ssget '((0 . "LINE,TEXT") (-3 ("PSK-PATH,PSK-IDEN"))))
	     )
    (if	(= "TEXT" (p-dxf e 0))
      (entdel e)
      (setq comps (cons e comps))
    )
  )

  (foreach comp	(mapcar 'psk-comp-load comps)
    (if	(psk-comp-iden comp)
      (setq done (1+ done))
      (setq skip (1+ skip))
    )
  )
  
  (princ (strcat "标注了 " (itoa done) " 个对象"))
  (if (> skip 0)
    (princ
      (strcat ", "
	      (itoa skip)
	      " 个因长度过小忽略标注 ($psk-iden-minlength = "
	      (rtos $psk-iden-minlength 2 0)
	      ")"
      )
    )
  )
  (p-endundomark)
  (princ)
)


(defun psk-cmd-redraw (/ comps ens)
  (setq ens (p-ss->enames (ssget '((-3 ("PSK-PATH,PSK-COMP,PSK-DRAFT"))))))
  (p-timer-start)
  (p-startundomark)
  (foreach en ens
    (if	(p-xdata-get-inner en "PSK-DRAFT")
      (entdel en)
      (setq comps (cons (psk-comp-load en) comps))
    )
  )
  (psk-comps-draw comps)
  (p-endundomark)
;;;    (princ
;;;      (strcat "\r生成进度 " (itoa (1+ n)) " / " (itoa count))
;;;    )
;;;  )

  (princ (strcat
	   "绘制 "
	   (itoa (length comps))
	   " 个管件, 耗时 "
	   (itoa (p-timer-stop))
	   ;;(rtos (/ (p-timer-stop) 1000.) 2 0)
	   " ms"
	 )
  )
)


;;;(defun LM:arc->bulge ( c a1 a2 r )
;;;    (list
;;;        (polar c a1 r)
;;;        (   (lambda ( a ) (/ (sin a) (cos a)))
;;;            (/ (rem (+ pi pi (- a2 a1)) (+ pi pi)) 4.0)
;;;        )
;;;        (polar c a2 r)
;;;    )
;;;)
;;;
;;;(defun c:tt ( / e )
;;;    (if (setq e (ssget "_+.:E:S" '((0 . "ARC"))))
;;;        (progn
;;;            (setq e (entget (ssname e 0)))
;;;            (entmake
;;;                (append
;;;                   '(
;;;                        (000 . "LWPOLYLINE")
;;;                        (100 . "AcDbEntity")
;;;                        (100 . "AcDbPolyline")
;;;                        (090 . 2)
;;;                        (070 . 0)
;;;                    )
;;;                    (mapcar 'cons '(10 42 10)
;;;                        (LM:arc->bulge
;;;                            (cdr (assoc 10 e))
;;;                            (cdr (assoc 50 e))
;;;                            (cdr (assoc 51 e))
;;;                            (cdr (assoc 40 e))
;;;                        )
;;;                    )
;;;                )
;;;            )
;;;        )
;;;    )
;;;    (princ)
;;;)