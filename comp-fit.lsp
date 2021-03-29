;; 返回管口数据（相较于块插入点的偏移量 管口方向 特征尺寸）
;; (psk-fit-getports (car (entsel)))
;; ((1 (43178.8 13384.1 0.0) (1.0 0.0 0.0) 10.0) (2 (42778.8 13784.1 0.0) (0.0 1.0 0.0) 10.0))
;;;_$ (p-benchmark '(psk-fit-getports en) 1000)
;;;"Benchmark loops = 1000, in 1938 ms, 516 invoke / s"
;;;_$ (p-benchmark '(psk-fit-getports en) 1000)
;;;"Benchmark loops = 1000, in 94 ms, 10638 invoke / s"
(defun psk-fit-getports	(fit / n p r r1)
  ;; 实体LINE代表的管口实现，因为性能太差，已废弃2021-2-9
;;;  (if (= 'ename (type fit))
;;;    (setq fit (psk-comp-load fit))
;;;  )
;;;  
;;;  (if (= "FIT" (psk-comp-getname fit))
;;;    (progn
;;;      (setq ename (p-get fit -1)
;;;	    geom  (p-refgeom ename)
;;;      )
;;;
;;;      (foreach e (p-block-items (p-dxf ename 2))
;;;	(if (setq props (p-xprop-getall e "PSK-COMP"))
;;;	  (progn
;;;	    (setq ep (p-dxf e '(-1 10))
;;;		  r  (cons (cons (car ep) (p-block-trans (cadr ep) geom))
;;;			   r
;;;		     )
;;;	    )
;;;	  )
;;;	)
;;;      )
;;;      r
;;;    )
;;;  )
  (if (/= 'ename (type fit))
    (setq fit (p-get fit -1))
  )
  (setq	p  (p-dxf fit 10)
	r1 (mapcar 'cdr (p-xdata-get fit "PSK-PORT"))
	n  0
  )

  (while r1
    (setq r  (cons (list
		     (setq n (1+ n))
		     (mapcar '+ p (car r1))
		     (cadr r1)
		     (caddr r1)
		   )
		   r
	     )
	  r1 (cdddr r1)
    )
  )
  (reverse r)
)

;; (psk-fit-moveport (psk-comp-load (car (entsel))) (getpoint )(getpoint ))
(defun psk-fit-moveport	(fit p newp / insert ports offset)
  (setq	insert (p-get fit -1)
	ports  (psk-ports-sort (psk-fit-getports insert) p)
	offset (mapcar '- newp (psk-port-getpos (car ports)))
  )

  (if (not (equal '(0. 0. 0.) offset 0.1))
    (progn
      (setq newp (mapcar '+ offset (p-dxf insert 10)))
      (p-entmod insert (cons 10 newp))
      
      (cons offset (cdr (mapcar 'psk-port-getpos ports)))
    )
  )
)


;;;(defun psk-make-port (p1 p2 prop)
;;;  (entmake
;;;    (list
;;;      '(0 . "LINE")
;;;      (cons 10 p1)
;;;      (cons 11 p2)
;;;      (list -3
;;;	    (cons "PSK-COMP" (p-xprop->xdata prop nil))
;;;      )
;;;    )
;;;  )
;;;)
(defun psk-fit-create (p a func param ports prop / en name)
  (psk-set-customlayerbyid
    (strcat (psk-path-getservice prop) "-CL")
  )
  (setq	$addnew-block-base p
	name		   (p-make-block "*U" func param)
	$addnew-block-base '(0 0 0)
  )

  (psk-comp-save
    prop
    (setq en (p-make-insert name p 1. 1. 1. a))
    "PSK-COMP"
  )

  (p-xdata-set en "PSK-PORT" ports)
  en
)



;; 弯头类管件 (psk-fit-create-elbow 路径交点 ..)
(defun psk-create-elbow	(p a1 a2 d erf prop / a3 a4 a5 d1 p1 p2 p3 r)
  (setq	a3 (p-angle-regular (/ (- a2 a1) 2.0))
	r  (* erf
	      (if (vl-consp d)
		(car d)
		d
	      )
	   )
	d1 (abs (/ r (p-tan a3)))
	p1 (polar p a1 d1)
	p2 (polar p a2 d1)
  )

  (setq	p3 (polar p
		  (+ a1 a3)
		  (abs (/ r (sin a3)))
	   )
	a4 (angle p3 p1)
	a5 (angle p3 p2)
  )

  (list	(psk-fit-create
	  p
	  0.
	  (function p-make-sharparc)
	  (list p3 r a4 a5)
	  (list	(cons 1013 (polar '(0 0) a1 d1))
		(cons 1013 (polar '(0 0) a1 1.))
		(psk-port-packsize d)
		(cons 1013 (polar '(0 0) a2 d1))
		(cons 1013 (polar '(0 0) a2 1.))
		(psk-port-packsize d)
	  )
	  prop
	)
	p1
	p2
  )
)
;;

(defun psk-create-reducer (p len a d1 d2 al prop / p1 p2 v1 v2)
  (setq
    p1 (polar p a (/ len -2.))	
    p2 (polar p a (/ len 2.))
    v1 (polar '(0 0) a -1.)
    v2 (polar '(0 0) a 1.)
  )
  (cond	((= "L" al)
	 (setq p2 (polar p2 (+ a $pi/2) (/ (abs (- (car d1) (car d2))) 2.)))
	)
	((= "R" al)
	 (setq p2 (polar p2 (- a $pi/2) (/ (abs (- (car d1) (car d2))) 2.)))
	)
  )


  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p1 p2)
	   )
	  nil
	  (list	(cons 1013 (mapcar '- p1 p))
		(cons 1013 v1)
		(psk-port-packsize d1)
		(cons 1013 (mapcar '- p2 p))
		(cons 1013 v2)
		(psk-port-packsize d2)
	  )
	  prop
	)
	p1
	p2
  )
)
;;


(defun psk-create-tee (p a1 a2 a3 d1 d2 d3 erf prop / p1 p2 v1 v2)
  (setq
    p1 (polar p a1 (* erf (max (car d2) (car d3))))
    p2 (polar p a2 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d2))))
    p3 (polar p a3 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d3))))
    v1 (polar '(0 0) a1 1.)
    v2 (polar '(0 0) a2 1.)
    v3 (polar '(0 0) a3 1.)
  )

  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p p1)
	     (p-make-line p p2)
	     (p-make-line p p3)
	   )
	  nil
	  (list	(cons 1013 (mapcar '- p1 p))
		(cons 1013 v1)
		(psk-port-packsize d1)
		(cons 1013 (mapcar '- p2 p))
		(cons 1013 v2)
		(psk-port-packsize d2)
		(cons 1013 (mapcar '- p3 p))
		(cons 1013 v3)
		(psk-port-packsize d3)
	  )
	  prop
	)
	p1
	p2
	p3
  )
)
;;


(defun psk-create-tee2 (p a1 a3 d1 d2 d3 erf al prop / p1 p2 v1 v2)
  (setq
    a2 (p-angle-reverse a1)
    p1 (polar p a1 (* erf (car d3)))
    p2 (polar p a2 (+ 100. (* 0.5 (car d3))))
    p3 (polar p a3 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d3))))
    v1 (polar '(0 0) a1 1.)
    v2 (polar '(0 0) a2 1.)
    v3 (polar '(0 0) a3 1.)
  )
  (cond	((= "L" al)
	 (setq p2 (polar p2 (+ a2 $pi/2) (/ (abs (- (car d1) (car d2))) 2.)))
	)
	((= "R" al)
	 (setq p2 (polar p2 (- a2 $pi/2) (/ (abs (- (car d1) (car d2))) 2.)))
	)
  )
  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p p1)
	     (p-make-line p p2)
	     (p-make-line p p3)
	   )
	  nil
	  (list	(cons 1013 (mapcar '- p1 p))
		(cons 1013 v1)
		(psk-port-packsize d1)
		(cons 1013 (mapcar '- p2 p))
		(cons 1013 v2)
		(psk-port-packsize d2)
		(cons 1013 (mapcar '- p3 p))
		(cons 1013 v3)
		(psk-port-packsize d3)
	  )
	  prop
	)
	p1
	p2
	p3
  )
)
;;

(defun psk-create-branch (p a1 a2 d1 d2 erf prop /)
  (setq	p2 (polar p a2 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d2))))
	v1 (polar '(0 0) a1 1.)
	v2 (polar '(0 0) a2 1.)
  )

  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p p2)
	   )
	  nil
	  (list	(cons 1010 '(0 0))
		(cons 1013 v1)
		(psk-port-packsize d1)
		(cons 1013 (mapcar '- p2 p))
		(cons 1013 v2)
		(psk-port-packsize d2)
	  )
	  prop
	)
	p2
  )
)
;;

(defun psk-create-cross	(p a1 a2 a3 a4 d1 d2 d3 d4 erf prop / p1 p2 p3 p4 v1 v2 v3 v4)
  (setq
    p1 (polar p a1 (* erf (max (car d3) (car d4))))
    p2 (polar p a2 (+ 100. (* 0.5 (max (car d3) (car d4)))))
    p3 (polar p a3 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d3))))
    p4 (polar p a4 (+ (/ (car d1) 2.) (* (- erf 0.5) (car d4))))
    v1 (polar '(0 0) a1 1.)
    v2 (polar '(0 0) a2 1.)
    v3 (polar '(0 0) a3 1.)
    v4 (polar '(0 0) a4 1.)
  )

  (list	(psk-fit-create
	  p
	  0.
	  '(lambda ()
	     (p-make-line p p1)
	     (p-make-line p p2)
	     (p-make-line p p3)
	     (p-make-line p p4)
	   )
	  nil
	  (list	(cons 1013 (mapcar '- p1 p))
		(cons 1013 v1)
		(psk-port-packsize d1)
		(cons 1013 (mapcar '- p2 p))
		(cons 1013 v2)
		(psk-port-packsize d2)
		(cons 1013 (mapcar '- p3 p))
		(cons 1013 v3)
		(psk-port-packsize d3)
		(cons 1013 (mapcar '- p4 p))
		(cons 1013 v4)
		(psk-port-packsize d4)
	  )
	  prop
	)
	p1
	p2
	p3
	p4
  )
)
;;

;;;(defun c:en () (setq en (car (entsel))))