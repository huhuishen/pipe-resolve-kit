;;; 布置管道路径
(defun psk-editor-createpath (/ p path paths rt ss)
  (if (= 0 (psk-createvalue-prompt))
    (vl-exit-with-value 0)
  )
  (while (/= "" (car paths))
    (if	(null paths)
      ;; 指定起点时的提示信息
      (progn (initget "G")
	     (setq p (getpoint "\n指定起点或 [从已有管线提取(G)]:"))
      )
      ;; 指定下一点时的提示信息
      (progn (initget "C S P R U")
	     (setq p
		    (getpoint
		      (trans (cadar paths) 0 1)
		      "\n指定下一个点或 [设置(C)/重新指定起点(S)/坐标(P)/相对(R)/后退(U)]:"
		    )
	     )
      )
    )
    (cond
      ((null p)
       (setq paths (cons "" paths)) ;_ 将结束绘制
      )
      ((= p "C") (psk-createvalue-prompt))
      ((and (= p "G") (setq r (psk-paths-pick 1)))
       (setq comp		   (psk-comp-load (car r))
	     $psk-path-createvalue (cdr comp)
       )
      )
;;;      ((= p "F")
;;;       (if rt
;;;	 (progn
;;;	   (setq $property-profile-name (strcase rt))
;;;	   (d-set-property-profile)
;;;	 )
;;;       )
;;;      )
      ((= p "S") (setq paths nil))
      ((= p "U")
       (setq paths (cdr paths)
	     p	   (car paths)
       )
       (command "._UNDO" "B")
      )
      ;; 用户指定了一个有效的点
      ((or (= p "P") (= p "R") (and p (listp p)))
       (cond
;;;	 ((= p "C")
;;;	  (setq	p (car (car paths))
;;;		p (dlg_xyzcreate_show p)
;;;	  )
;;;	 )
;;;	 ((= p "R")
;;;	  (setq rpt (list (getpoint "\n指定定位点 <回车结束>:")))
;;;	  (while (car rpt)
;;;	    (setq rpt (cons (getpoint (car rpt) "\n指定定位点 <回车结束>:") rpt))
;;;	  )
;;;
;;;	  (if (cadr rpt)
;;;	    (setq p (cadr rpt))
;;;	    (setq p (car (car paths)))
;;;	  )
;;;	 )
       )
       (setq p (trans p 1 0))
       (if (null paths)
	 ;; 指定第一个点
	 (progn
;;;	   (psk-createvalue-prompt)
	   (setq bst (psk-comps-buildbst (psk-comps-fromviewport) 0.1)
		 rt  (p-bst-find p bst 0.1)
	   )

	   (if (and rt
		    (= 1 (length rt))
	       )
	     ;; 和已有路径开放管口相连
	     (setq paths (cons (list (psk-comp-load (car rt)) p) paths))
	     (setq paths (cons (list nil p) paths))
	   )
	 )
	 ;; 指定第2+点
	 (progn
	   ;; 创建管道
	   (command "._UNDO" "M")

	   (setq path  (psk-path-create (cadar paths) p $psk-path-createvalue)
		 paths (cons (list path p) paths)
	   )
	   (psk-comp-redraw path)

	   (if (and (>= (length paths) 2)
		    (vl-consp (caar paths))
		    (vl-consp (caadr paths))
	       )
	     (psk-paths-connect
	       (list (car paths)
		     (list (caadr paths)
			   (psk-port-getpos (cadr (psk-ports-sort (psk-comp-getports (caadr paths)) (cadadr paths))))
		     )
	       )
	       t
	     )
	   )
	 )
       )
      )
    )
  )
  (princ)
)
;;





;; 将指定直线设置为管口
;;;(defun d-setport (/ en map p ss str)
;;;  (if (and
;;;	(setq en (entsel "选择接管:"))
;;;	(setq str (getstring "输入管道系统类型:"))
;;;      )
;;;    (p-xprop-set
;;;      (car en)
;;;      "MYPROPS_PORT"
;;;      (list (cons "SERV" (strcase str)))
;;;      nil
;;;    )
;;;  )
;;;)
;;;)


(defun p-get-ports (en / name r sys)
  (setq	name (cdr (assoc 2 (entget en)))
	r    (cdr (assoc name $p-ports-cache))
  )
  (if (null r)
    (progn
      (vlax-for	e (p-get-block name)
	(setq e	  (p-ensure-ename e)
	      sys (p-xprop-get e "MYPROPS_PORT" "SERVICE")
	)
	(if sys
	  (setq	r
		 (cons (list (cdr (assoc 10 (entget e)))
			     (cdr (assoc 11 (entget e)))
			     sys
		       )
		       r
		 )
	  )
	)
      )
      (setq $p-ports-cache (cons (cons name r) $p-ports-cache))
    )
  )
  r
)
;; 获取块实例中管口直线起终点
;; (p-get-portsforinsert (car (entsel)))
(defun p-get-portsforinsert (en / e geom ports)
  (if (setq ports (p-get-ports en))
    (progn
      (setq geom  (p-refgeom en)
	    ports (mapcar '(lambda (e)
			     (list (p-block-trans (car e) geom)
				   (p-block-trans (cadr e) geom)
				   (caddr e)
			     )
			   )
			  ports
		  )
      )
      ports
    )
  )
)
;;; 自动连接管口与干管
(defun d-connect-ports (/     a	    dxf	  en	ints  line  n	  p
			p1    p2    p3	  p4	port  ports ss	  offset
			remain	    sys
		       )
  (princ "选择块\n")
  (if (and (setq ss (ssget '((0 . "INSERT")))))
    (progn

      (repeat (setq n (sslength ss))
	(setq en (ssname ss (setq n (1- n))))
	(setq ports (append ports (p-get-portsforinsert en)))
      )
      (setq offset 150.)

      (while (and ports
		  (setq line (entsel "选择主管:"))
	     )
	(setq p	   (trans (cadr line) 1 0)
	      line (car line)
	      a	   (p-line-getangle line)
	      p	   (cdr (p-line-getendnear line p))
	      dxf  (entget line '("*"))
	      dxf  (vl-remove-if '(lambda (e) (member e '(5 10 11))) dxf)
	      sys  (p-xprop-get line "PSK-PATH" "SERV")
	)

	;; 绘制每个到设备的支管
	(foreach port ports
	  (if (= sys (caddr port))
	    (progn
	      (setq p1	 (car port)
		    p2	 (cadr port)
		    p3	 (polar p2 (angle p1 p2) offset)
		    p4	 (vlax-curve-getclosestpointto line p3 t)
		    ints (cons (cons p4 (distance p4 p)) ints)
	      )
	      (entmake
		(append	dxf
			(list (cons 10 p2) (cons 11 p3))
		)
	      )
	      (entmake
		(append	dxf
			(list (cons 10 p3) (cons 11 p4))
		)
	      )


	      (setq
		ints
		 (vl-sort ints
			  '(lambda (e1 e2) (< (cdr e1) (cdr e2)))
		 )
	      )
	    )
	    (progn
	      (setq remain (cons port remain))
	    )
	  )
	)

	;; 干管与每个支管交点处打断
	(entdel line)
	(while ints
	  (if (> (distance p (caar ints)) 1.)
	    (entmake
	      (append dxf
		      (list (cons 10 p) (cons 11 (caar ints)))
	      )
	    )
	  )
	  (setq	p    (caar ints)
		ints (cdr ints)
	  )
	)
	(setq ports  remain
	      remain nil
	      offset (+ 150. offset)
	)
      )
    )
  )
)
;;;;;
;;;
;;;(defun c:addpro	(/ en n ss)
;;;  (if (and (setq ss (ssget (list '(0 . "INSERT")))))
;;;    (progn
;;;      (repeat (setq n (sslength ss))
;;;	(setq en (ssname ss (setq n (1- n)))
;;;	)
;;;
;;;	(p-xprop-set en "MYPROPS_EQUIP" '(("LOAD" . 2.2)) nil)
;;;      )
;;;    )
;;;  )
;;;)


;;;
;;;_$ (get-near-number 12700 '(7100 12500 14000) 0.1 0)
;;;12500
;;; 返回'(7100 12500 14000)列表中最接近12700的数，允许返回的数比指定的数小 10 % 或大 0 %
;;;(defun get-near-number (n nl lt gt / l g r)
;;;  (setq	l (* n (- 1.0 lt))
;;;	g (* n (+ 1.0 gt))
;;;  )
;;;  (while nl
;;;    (if	(and (>= (car nl) l) (<= (car nl) g))
;;;      (setq r  (car nl)
;;;	    nl nil
;;;      )
;;;    )
;;;    (setq nl (cdr nl))
;;;  )
;;;  r
;;;)


;;;(defun d-insert-router (p func params layer props / en name old rs)
;;;  (setq	old (p-make-setenv
;;;	      (list "0" 0 "ByBlock" -2 nil p)
;;;	    )
;;;  )
;;;
;;;  (setq name (p-make-block "*U" func params))
;;;
;;;  (p-make-setenv
;;;    (list layer
;;;	  256
;;;	  "ByLayer"
;;;	  -1
;;;    )
;;;  )
;;;
;;;  (setq en (p-make-insert name p 1. 1. 1. 0.))
;;;  (p-set-routerproperty en props)
;;;  (p-make-setenv old)
;;;)



;;;(defun d-remove-router (/)
;;;
;;;
;;;  (while (setq td (ssget ":E:S" '((-3 ("PSK-COMP")))))
;;;    (setq ss (ssget "W"
;;;		    (trans (getvar "EXTMIN") 0 1)
;;;		    (trans (getvar "EXTMAX") 0 1)
;;;		    '((-3 ("PSK-COMP")))
;;;	     )
;;;    )
;;;
;;;    (setq bst (p-build-bstforlines ss $PSK-router-fuzz))
;;;
;;;    (setq td (ssname td 0))
;;;    (setq p (cdr (assoc 10 (entget td))))
;;;    (setq rs (p-find-bst p bst $PSK-router-fuzz))
;;;
;;;    (setq p (cdr (assoc 11 (entget td))))
;;;    (setq rs (append rs (p-find-bst p bst $PSK-router-fuzz)))
;;;
;;;    (setq rs (vl-remove-if '(lambda (e) (equal e td)) rs))
;;;
;;;    (setq p (p-line-getinters (car rs) (last rs)))
;;;
;;;    (if	p
;;;      (progn
;;;	(entdel td)
;;;	(d-set-lineendpoint (car rs) p p)
;;;	(d-set-lineendpoint (last rs) p p)
;;;      )
;;;    )
;;;  )
;;;)





;;
(setq $psk-alignmode "C")
(defun psk-editor-getalignmode (/ r)
  (initget "L R C")
  (if (setq r
	     (getkword (strcat "\n选择偏心管件对齐方式 [左(L)/居中(C)/右(R)]: <" $psk-alignmode ">")
	     )
      )
    (setq $psk-alignmode r)
  )
  $psk-alignmode
)
;;




;;; BOOKMARK 变量处理辅助函数
(defun psk-propbox-change (name /)
  (propbox-update
    (psk-get-createvalue name)
    (psk-get-desc name)
  )
)
;;;_$ (psk-get-createvalue "PIPE")
;;;(("TYPE" . "DUCT-RECT") ("SERV" . "EA") ("DN" . 500) ("ERF" . 0.8) ("EL" . 0.0) ("AL") ("FLR") ("FLVR"))
;;;_$ (psk-get-createvalue "DUCT-RECT")
;;;(("TYPE" . "DUCT-RECT") ("SERV" . "EA") ("W" . 500) ("H" . 320) ("ERF" . 0.8) ("EL" . 0.0) ("AL") ("FLR") ("FLV"))
(defun psk-get-createvalue (tp / e r v)
  (setq
    r (p-get $psk-path-types tp)
    ;;$psk-path-createvalue ("TYPE" "SERV" "DN" "ERF" "EL" "AL" "FLR" "FLVR")

    r
     (mapcar (function list) r)
    ;; (("TYPE") ("SERV") ("DN") ("ERF") ("EL") ("AL") ("FLR") ("FLVR"))

    $psk-path-createvaluelast
     (p-set $psk-path-createvaluelast
	    (cons "TYPE" tp)
     )

    r (mapcar (function
		(lambda	(e)
		  (if
		    (setq v (p-get $psk-path-createvaluelast (car e)))
		     (cons (car e) v)
		     e
		  )
		)
	      )
	      r
      )
  )
  (if (wcmatch tp "DUCT*")
    (p-set r
	   (cons "SERV" (p-get $psk-path-createvaluelast "SERVD"))
    )
    r
  )
)
;;(psk-get-desc "PIPE")
;;(psk-get-desc "DUCT-ROUND")
(defun psk-get-desc (tp / desc)
  (if (null tp)
    nil
    (progn
      (setq desc (p-get1 $psk-prop-defination (p-get $psk-path-types tp))
	    serv (p-get1 desc "SERV")
      )
      (if (wcmatch tp "DUCT*")
	(p-set desc (append serv (list $psk-services-duct)))
	(p-set desc (append serv (list $psk-services)))
      )
    )
  )
)


(defun psk-createvalue-prompt (/ change k)

  (setq	k		      (p-get $psk-path-createvaluelast "TYPE")
	$psk-path-createvalue (psk-get-createvalue k)
  )
  (setq	change (propertybag-edit
		 $psk-path-createvalue
		 (psk-get-desc k)
		 '$psk-sel-path-create
		 nil
	       )
  )

  (if (/= 0 change)
    (if	change
      (progn
	(if (setq k (p-get $psk-path-createvaluelast "TYPE"))
	  (setq $psk-path-createvalue (psk-get-createvalue k))
	)
	(setq $psk-path-createvalue
	       (p-set $psk-path-createvalue change)
	)

	;; 保存历史记录方便下次使用
	(foreach e change
	  (if (and (= "SERV" (car e))
		   (wcmatch (p-get $psk-path-createvalue "TYPE")
			    "DUCT*"
		   )
	      )
	    (setq
	      $psk-path-createvaluelast
	       (p-set $psk-path-createvaluelast
		      (cons "SERVD" (cdr e))
	       )
	    )
	    (setq
	      $psk-path-createvaluelast
	       (p-set $psk-path-createvaluelast
		      e
	       )
	    )
	  )
	)

	(vlax-ldata-put
	  "PSK"
	  "CREATEVALUELAST"
	  $psk-path-createvaluelast
	)
	$psk-path-createvalue
      )
    )
    0 ;_ 用户取消了对话框
  )
)
;;

(defun psk-comps-modify	(/ comps r rt v)
  (if (and (setq comps (psk-comps-ssget)))
    (progn
      (foreach comp comps
	(setq
	  ;; 去掉-1 实体名
	  comp (cdr comp)
	  ;; 去掉私有属性
	  comp (vl-remove-if
		 '(lambda (e) (= "." (substr (car e) 1 1)))
		 comp
	       )
	)
	(if (null r)
	  (setq r comp)
	  (progn
	    (foreach e comp
	      (setq v (p-get r (car e)))
	      (if (null v)
		(setq r (p-set r e))
		(if (not (equal v (cdr e)))
		  (setq r (p-set r (cons (car e) "*多种*")))
		)
	      )
	    )
	  )
	)
      )
      (if (= "*多种*" (p-get r "TYPE"))
	(princ "\n暂不支持不同类别管件批量编辑")
	(if (/=	0
		(setq rt (propertybag-edit
			   r
			   (psk-get-desc (p-get r "TYPE"))
			   nil
			   nil
			 )
		)
	    )
	  (progn
	    (foreach comp comps
	      ;; 控制类型修改将控制线放置到相应的图层中
	      (if (and (setq sys (p-get rt "SERV"))
		       (/= sys (p-get comp "SERV"))
		  )
		(progn
		  (psk-set-customlayerbyid (strcat sys "-CL"))
		  (vla-put-layer
		    (p-ensure-object (psk-comp-getename comp))
		    $addnew-layer
		  )
		)
	      )
	      ;; DD DN W 属性设置额外处理TODO
	      (psk-comp-set (psk-comp-getename comp) rt)
	      
	      (if (and (setq r (psk-comp-gettype comp))
		       (/= "PSK-BLOCK" r)
		  )
		(psk-comp-redraw
		  (psk-comp-load (psk-comp-getename comp))
		)
	      )
	    )
	  )
	)
      )
    )
  )
)
;;