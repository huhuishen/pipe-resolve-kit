
;;; 直线相互连接形成之拓朴结构的相关操作 2018-4-1
;;; 2021-2-17

;; (setq bst (psk-comps-buildbst (psk-comps-ssget) 0.1))
;; (setq bst (psk-comps-buildbst (psk-comps-all) 0.1))
;; (p-bst-find (getpoint) bst 0.1)
;; (p-bst-find (getpoint) (psk-comps-buildbst (psk-comps-all) 0.1) 0.1)
;; (<图元名: 7ff415f1e150> <图元名: 7ff415f1af10>)
(defun psk-comps-buildbst (comps tolerance / pcs ports)
  (foreach comp	comps
    (if	(setq ports (psk-comp-getports comp))
      (progn
	(setq ports (mapcar 'psk-port-getpos ports))
	(foreach p ports
	  (setq pcs (cons (list p (psk-comp-getename comp)) pcs))
	)
      )
    )
  )

  (p-bst-build (p-sort-points pcs tolerance))
)
;;
(defun psk-comp-nextp (res bst ename p / ports nextps r)
  (setq	ports (psk-comp-getports ename)
	ports (psk-ports-sort ports p)
  )

  (if (not (ssmemb ename $psk-comps-walked))
    (progn
      (setq res		      (cons (p-dxf ename 5) res)
	    $psk-comps-walked (ssadd ename $psk-comps-walked)
	    nextps	      (cdr (mapcar 'cadr ports))
      )
;;;      (redraw ename 3)

      (cond ((= 1 (length nextps))
	     (setq res (psk-comp-next res bst (car nextps)))
	    )
	    ((< 1 (length nextps))
	     (foreach p	nextps
	       (setq r (cons (psk-comp-next '() bst p) r))
	     )
	     (setq res (cons r res))
	    )
      )
    )
  )
  res
)
;;
(defun psk-comp-next (res bst p / ename enames nextps r r1)
  (setq	enames (p-bst-find p bst 0.1)
	enames (vl-remove-if '(lambda (e) (ssmemb e $psk-comps-walked)) enames)
  )
  (cond
    ;; 只有一个后续管件
    ((= 1 (length enames))
     (setq res (psk-comp-nextp res bst (car enames) p))
    )
    ;; 多个后续管件
    ((< 1 (length enames))
     ;; 优先遍历管道
;;;     (setq enames (vl-sort enames
;;;			   '(lambda (e1 e2)
;;;			      (if (p-xdata-get e1 "PSK-PATH")
;;;				t
;;;			      )
;;;			    )
;;;		  )
;;;     )
     (foreach ename enames
       (setq r (cons (psk-comp-nextp '() bst ename p) r))
     )
     (setq res (cons r res))
    )
  )
  res
)
;; 建立路径图
;; (psk-comp-buildmap (getpoint))
;; ((("92D" "92F") ("922" "921" "923" "91B" "91C" "91A")) "919")
(defun psk-comp-buildmap (p / bst)
  (setq	bst		  (psk-comps-buildbst (psk-comps-all) 0.1)
	$psk-comps-walked (ssadd)
  )
  (psk-comp-next '() bst p)
)
;;

(defun psk-map-enum-inner (map root /)
  (setq map (reverse map))
  (while map
    (cond
      ((atom (car map))
       (setq root (append root (list (car map))))
       (if (null (cdr map))
	 (setq psk-map-routers (cons root psk-map-routers))
       )
      )
      ((vl-consp (car map))
       (psk-map-enum-inner (car map) root)
      )
    )
    (setq map (cdr map))
  )
  root
)
;; 生成各分支(从指定起点到各末端点)的所有路径
;; (psk-map-enum '((("92D" "92F") ("922" "921" "923" "91B" "91C" "91A")) "919"))
;; (("919" "922" "921" "923" "91B" "91C" "91A") ("919" "92D" "92F"))
;; (psk-map-enum '((((((("83B") ("83A")) "83C" "800") ((("814") ("811")) "82A" "801")) "802" "7FF") nil) "891" "888"))
;; (psk-map-enum (psk-comp-buildmap (getpoint)))
(defun psk-map-enum (map / psk-map-routers)
  (psk-map-enum-inner map '())
  psk-map-routers
)
;;; 遍历所有支管
(defun psk-show-branchs	(p / c routers ss)
  (setq	routers	(psk-map-enum (psk-comp-buildmap p))
	c	0
  )
  (foreach router routers
    (setq ss (ssadd))
    (foreach e router
      (if e
	(setq ss (ssadd (handent e) ss))
      )
    )
    (sssetfirst ss ss)

    (getstring (strcat "\n "
		       (itoa (setq c (1+ c)))
		       " / "
		       (itoa (length routers))
		       " 下一个:"
	       )
    )
    (setq ss nil)
    (sssetfirst nil nil)
  )
  (princ)
)
;; (psk-path-sum '((("9F6" "9F3") ("9F9")) "9F0" "9FC") 100.)
;; (psk-path-sum (psk-comp-buildmap (getpoint)) (getreal "输入末端流量"))
(defun psk-path-sum (map fs / v)
  (foreach node	map
    (cond ((null node)
	  )
	  ((vl-consp node)
	   (if (null v)
	     (setq v 0.)
	   )
	   (setq v (+ v (psk-path-sum node fs)))
	  )
	  ((atom node)
	   (if (null v)
	     (setq v fs)
	   )
	   (psk-comp-set (handent node) (list (cons "FLR" v)))
	  )
    )
  )
  v
)
;;
;;;(defun c:hh (/ en)
;;;;;;  (princ (p-dxf (car (entsel)) 5))
;;;  (setq en (car (entsel)))
;;;  (princ (psk-comp-get en "FLR"))
;;;  (princ)
;;;)







;;; 累计管道流量
(defun d-sum-routers (/ e en line map n p ports ss ss2 ss3)
  (if (setq ss (ssget '((0 . "LINE,INSERT") (-3 ("MYPROPS_*")))))
    (while (setq line (entsel "选择分支起点:"))
      (setq ss2 (ssadd))

      (repeat (setq n (sslength ss))
	(setq en (ssname ss (setq n (1- n))))
	(if (= "INSERT" (cdr (assoc 0 (entget en))))
	  (setq
	    ports (append
		    (mapcar '(lambda (e) (list e en))
			    (mapcar 'cadr (p-get-portsforinsert en))
		    )
		    ports
		  )
	  )
	  (ssadd en ss2)
	)
      )

      (setq
	p    (cadr line)
	line (car line)
	p    (p-line-getendnear line p)
	map  (psk-comps-buildmap
	       (p-build-bstforlines ss2 0.1)
	       line
	       p
	       0.1
	     )
      )

      (setq ss3 (ssadd))
      (psk-path-sum map (p-build-bst (p-sort-points ports 0.1)))

      (p-ss-highlight ss3)
      (getstring
	(strcat	"\n确认亮显设备 n="
		(itoa (sslength ss3))
		", Q="
		(rtos (p-property-get line "MYPROPS_ROUTER" "LOAD") 2 4)
		"<继续> :"
	)
      )
      (p-ss-highlight nil)
      (setq ss3 nil)
    )
  )

  (princ)
)
;;;(defun psk-path-sum (map bst / en v)
;;;  (setq v 0.)
;;;  (cond
;;;    ((atom (car map))
;;;     ;; (setq ss3 (ssadd (car map) ss3))
;;;     (if (null (cdr map))
;;;       (progn
;;;	 ;; 这是最后一段
;;;	 (or (setq
;;;	       en
;;;		(p-bst-find (cdr (assoc 10 (entget (car map)))) bst 0.1)
;;;	     )
;;;	     (setq
;;;	       en
;;;		(p-bst-find (cdr (assoc 11 (entget (car map)))) bst 0.1)
;;;	     )
;;;	 )
;;;
;;;	 (if en
;;;	   (progn
;;;	     (setq ss3 (ssadd (car en) ss3))
;;;	     (setq v (p-property-get (car en) "MYPROPS_EQUIP" "LOAD"))
;;;	   )
;;;	 )
;;;       )
;;;       (progn
;;;	 (setq v (psk-path-sum (cdr map) bst))
;;;       )
;;;     )
;;;     (p-xprop-set
;;;       (car map)
;;;       "MYPROPS_ROUTER"
;;;       (list (cons "LOAD" v))
;;;     )
;;;    )
;;;
;;;    (t
;;;     (while (vl-consp (car map))
;;;       (setq v	 (+ v (psk-path-sum (car map) bst))
;;;	     map (cdr map)
;;;       )
;;;     )
;;;    )
;;;  )
;;;  v
;;;)
;;


;;;(defun c:rr (/ rv)
;;;  (initget "C W E P")
;;;  (setq	rv (getkword
;;;	     "\n[连接设备(C)/水力计算(E)/遍历支管(W)/设置管口(P)]:"
;;;	   )
;;;  )
;;;
;;;  (cond
;;;    ((= "C" rv)
;;;     (d-connect-ports)
;;;    )
;;;    ((= "W" rv)
;;;     (d-walk-all-branchs)
;;;    )
;;;    ((= "E" rv)
;;;     (d-sum-routers)
;;;    )
;;;    ((= "P" rv)
;;;     (d-setport)
;;;    )
;;;  )
;;;)