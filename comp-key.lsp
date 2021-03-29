

;; (setq key (psk-key-create "DUCT-STOR" '(("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300))))
;;;(defun psk-key-create (en key /)
;;;  (cons (cons "NAME" name) prop)
;;;)

(defun psk-key-getdef (name)
  (p-get $psk-keys name)
)

;;;_$ (psk-key-gettype (psk-key-getdef "SS"))
;;;"ATTACH"
(defun psk-key-gettype (keydef)
  (p-get keydef "TYPE")
)

;;;_$ (psk-key-getdrawfunc (psk-key-getdef "SS"))
;;;PSK-DRAW-SINGLESHUTTER
(defun psk-key-getdrawfunc (keydef)
  (car (p-get keydef "DRAW"))
)

;;;_$ (psk-key-getdrawparam (psk-key-getdef "SS"))
;;;("A" "B")
(defun psk-key-getdrawparam (keydef)
  (cadr (p-get keydef "DRAW"))
)

(defun psk-key-getparam	(keydef)
  (mapcar 'car (p-get keydef "PARAM"))
)

;; (psk-key-getkeytemplate (psk-key-getdef key))
(defun psk-key-getkeytemplate (keydef)
  (p-get keydef "KEY")
)

;; (psk-key-evalkey '(("NAME" . "DUCT-STOR") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300)))
(defun psk-key-evalkey (key / keydef templ)
  (setq	keydef (psk-key-getdef (p-get key "NAME"))
	templ  (psk-key-getkeytemplate keydef)
  )
  (p-template-eval templ key)
)


;; (setq $psk-block-base '(0 0) $psk-block-angle 0.)
;; (psk-key-draw '(("NAME" . "DUCT-STOR") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300)))
(defun psk-key-draw (key / e func keydef param)
;;;  (psk-set-customlayerbyid
;;;    (strcat (p-get key "SERV") "-DUCT")
;;;  )

  (setq
;;;	uid	      (cons 1071 (p-uid))
;;;	$addnew-xdata (list "PSK-DRAFT" uid)

    keydef (psk-key-getdef (p-get key "NAME"))
    func   (psk-key-getdrawfunc keydef)
    ;; ("W" 50) 处理 字符进行求值 数字不处理
    param  (mapcar '(lambda (e)
		      (if (= 'str (type e))
			(p-get key e)
			(if (= 'sym (type e))
			  (vl-symbol-value e)
			  e
			)
		      )
		    )
		   (psk-key-getdrawparam keydef)
	   )
  )

  (if (vl-catch-all-error-p (vl-catch-all-apply func param))
    (princ (strcat "\nerror in psk-key-draw "
		   (vl-princ-to-string (cons func param))
	   )
    )
  )
;;;  (p-xdata-set en "PSK-DRAFT-OWNER" (list uid))
;;;  (setq $addnew-xdata nil)
)

(defun psk-key-drawblock (key / name)
  (setq	$psk-block-base	 '(0 0)
	$psk-block-angle 0.
  )
  (p-make-block "*U" 'psk-key-draw (list key))
)
;; (psk-key-createblock (getpoint) 0. '(("NAME" . "DUCT-STOR") ("W" . 500) ("H" . 250) ("D" . 320) ("L" . 300)))
;; (psk-key-createblock (getpoint) 0. '(("NAME" . "DS") ("A" . 500) ("B" . 250)))
;; (psk-key-createblock (getpoint) 0. '(("NAME" . "AXIAL-FAN") ("D" . 320) ("L" . 320)))
(defun psk-key-createblock (p a key ktype en / en name r)
  (setq r (p-make-setenv $addnew-default))

  (setq name (psk-key-drawblock key))

  (p-make-setenv r)

  ;; 创建新块或修改已有块
  (if (null en)
    (setq en (p-make-insert name p 1. 1. 1. a))
    (p-entmod en (cons 2 name))
  )

  (cond	((= "ATTACH" ktype)
	 (p-xdata-set
	   en
	   "PSK-PORT"
	   (list (cons 1010 '(0 0 0))
		 (cons 1013 '(0 0 1))
		 (psk-port-packsize 0)
	   )
	 )
	)
	((= "INLINE" ktype)
	 (setq len (p-get key "L"))
	 (p-xdata-set
	   en
	   "PSK-PORT"
	   (list (cons 1010 '(0 0 0))
		 (cons 1013 (polar '(0 0) a -1.))
		 (psk-port-packsize 0)
		 (cons 1013 (polar '(0 0) a len))
		 (cons 1013 (polar '(0 0) a 1.))
		 (psk-port-packsize 0)
	   )
	 )
	)
	((= "END" ktype)
	 (p-xdata-set
	   en
	   "PSK-PORT"
	   (list (cons 1010 '(0 0 0))
		 (cons 1013 (polar '(0 0) a 1.))
		 (psk-port-packsize 0)
	   )
	 )
	)
  )

  (p-xprop-set en "PSK-BLOCK" key)
  en
)
;;


;;; 在路径上插入实体部件(管件或管件包)
;;;(defun d-insert-pack (router p func params / en name old rs)
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
;;;
;;;  (command "_.BREAK" en p "@")
;;;  (p-xprop-set en "MYPROPS_PACK" props)
;;;  (p-make-setenv old)
;;;)

;;;_$ (psk-key-getnames)
;;;("DUCT-STOR" "AXIAL-FAN" "FDC-70" "CAP" "DS" "FL-PL")
(defun psk-key-getnames	()
  (mapcar 'car $psk-keys)
)
;;;_$ (psk-key-gettext "DS")
;;;"双层百叶风口"
(defun psk-key-gettext (name)
  (p-get (p-get $psk-keys name) "DESC")
)
;;;_$ (psk-key-getparam "DS")
;;;"双层百叶风口"
(defun psk-key-getparam	(name)
  (p-get (p-get $psk-keys name) "PARAM")
)
;;;_$ (psk-key-getdesc "DS")
;;;(("NAME"...) ("A" 1070 "风口长度") ("B" 1070 "风口宽度"))
(defun psk-key-getdesc (name)
  (append (list
	    (list "NAME"
		  1000
		  "件号"
		  "NAME\n"
		  (mapcar
		    (function
		      (lambda (e)
			(list e
			      (psk-key-gettext e)
			      (strcat "(psk-key-change \"" e "\")")
			)
		      )
		    )
		    (psk-key-getnames)
		  )
	    )
	  )
	  (psk-key-getparam name)
  )
)
(defun psk-key-getcreatevalues (name / r)
  (if (setq r (p-get $psk-key-lastvalues name))
    r
    nil
  )
)
(defun psk-key-change (name /)
  (propbox-update
    (psk-key-getcreatevalues name)
    (psk-key-getdesc name)
  )
)
(setq $psk-key-lastname	  (vlax-ldata-get "PSK" "$PSK-KEY-LASTNAME")
      $psk-key-lastvalues (vlax-ldata-get "PSK" "$PSK-KEY-LASTVALUES")
)
(if (null $psk-key-lastname)
  (setq $psk-key-lastname "DS")
)
(defun psk-key-createvalue-prompt (/ change)
  (setq	change (propertybag-edit
		 (psk-key-getcreatevalues $psk-key-lastname)
		 (psk-key-getdesc $psk-key-lastname)
		 nil
		 t
	       )
  )

  (if (/= 0 change)
    (if	change
      (progn
	(setq $psk-key-lastname	  (p-get change "NAME")
	      $psk-key-lastvalues (p-set $psk-key-lastvalues
					 (cons $psk-key-lastname change)
				  )
	)
	(vlax-ldata-put
	  "PSK"
	  "$PSK-KEY-LASTNAME"
	  $psk-key-lastname
	)
	(vlax-ldata-put
	  "PSK"
	  "$PSK-KEY-LASTVALUES"
	  $psk-key-lastvalues
	)
	change
      )
    )
  )
)
;; (psk-selectd '("A" "B"))
;;;_$ (psk-selectd '(("A" . "A desc..") ("B" . "B desc..")))
;;;("B" . "B desc..")
(defun psk-selectd (lst selkey / cursel id r)
  (if
    (and (>= (setq id (load_dialog (psk-get-filename "\\propertyedit.dcl")))
	     0
	 )
	 (new_dialog "psk_selectbox" id)
    )
     (progn
       (start_list "LIST")
       (mapcar (function (lambda (e)
			   (if (atom e)
			     (add_list e)
			     (add_list (cdr e))
			   )
			 )
	       )
	       lst
       )
       (end_list)

       (if selkey
	 (setq cursel (vl-position (assoc selkey lst) lst))
	 (setq cursel 0)
       )
       (set_tile "LIST" (itoa cursel))
       (action_tile "LIST" "(setq cursel (atoi $value)) (if (= 4 $reason) (done_dialog 1))")
       (action_tile "OK" "(done_dialog 1)")

       (setq r (start_dialog))
       (unload_dialog id)

       (if (= r 1)
	 (nth cursel lst)
       )
     )
  )
)
(defun psk-prop-prompt (prop desc / r)
  (setq
    prop (mapcar (function (lambda (e)
			     (if (null (p-get prop (car e)))
			       (cons (car e)
				     (cond ((= 1040 (cadr e)) 0.)
					   ((= 1070 (cadr e)) 0)
				     )
			       )
			       (p-get1 prop (car e))
			     )
			   )
		 )
		 desc
	 )
  )
  (foreach v desc
    (initget 6) ;_ >=0
    (setq r (p-edit-value (strcat "\n" (caddr v)) (p-get prop (car v))))
    (setq prop (p-set1 prop (car v) r))
  )

  prop
)
(defun psk-posangle-prompt (path p / a do line p2 ports)
  (setq	line (psk-comp-getename path)
	p    (p-line-closestpoint line p t)
	do   t
  )

  (while do
    (initget "N")
    (setq p2 (getpoint p "\n指定管件方向或 [选择最近管端(N)]:"))

    (cond
      ((= "N" p2)
       (setq ports (psk-path-getports path)
	     ports (psk-ports-sort ports p)
	     a	   (psk-port-getangle (car ports))
	     p	   (psk-port-getpos (car ports))
	     do	   nil
       )
      )
      (p2
       (setq p2	(p-line-closestpoint line p2 t)
	     a	(angle p p2)
	     do	nil
       )
      )
    )
  )
  (list p a)
)
(defun psk-path-createkey
       (/  keydef p param paramnames path prop tp)
  (if (and
	(setq
	  ;; (("DUCT-STOR" . "方圆变径") ...)
	  $psk-key-lastname
	   (car
	     (psk-selectd
	       (mapcar
		 (function
		   (lambda (e) (cons (car e) (p-get (cdr e) "DESC")))
		 )
		 $psk-keys
	       )
	       $psk-key-lastname
	     )
	   )
	)
	(setq path (psk-paths-pick 1))
      )
    (progn
      (setq keydef     (p-get $psk-keys $psk-key-lastname)
	    tp	       (p-get keydef "TYPE")
	    param      (p-get keydef "PARAM")
	    paramnames (mapcar 'car param)

	    ;; 将路径属性传递给管件包对象
	    prop       (psk-key-getcreatevalues $psk-key-lastname)
      )

      (foreach e (p-get1 (caar path) paramnames)
	(if (cdr e)
	  (setq prop (p-set prop e))
	)
      )

      (setq prop		(psk-prop-prompt prop param)
	    $psk-key-lastvalues	(p-set $psk-key-lastvalues
				       (cons $psk-key-lastname prop)
				)
      )
      (vlax-ldata-put
	"PSK"
	"$PSK-KEY-LASTNAME"
	$psk-key-lastname
      )
      (vlax-ldata-put
	"PSK"
	"$PSK-KEY-LASTVALUES"
	$psk-key-lastvalues
      )

      (setq prop (p-set1 prop "NAME" $psk-key-lastname))
      (psk-set-customlayerbyid
	(strcat (psk-path-getservice (caar path)) "-CPNT")
      )

      (cond ((= "ATTACH" tp)
	      (psk-path-createattach (caar path) (cadar path) prop)
	    )
	    ((= "INLINE" tp)
	      (setq p (psk-posangle-prompt (caar path) (cadar path)))

	      (psk-path-createinline (caar path) (car p) (cadr p) prop)
	    )
	    ((= "END" tp)
	     (psk-path-createend (caar path) (cadar path) prop)
	    )
      )
    )
  )

  (princ)
)
;;
(defun c:iv ()
  (p-commandrun '(psk-path-createkey))
)


;;;(defun psk-key-remove (/ key)
;;;
;;;  (while (null
;;;	   (setq key (ssget ":E:S" '((0 . "INSERT") (-3 ("PSK-BLOCK")))))
;;;	 )
;;;  )
;;;
;;;  (setq key (ssname path 0))



  
;;;  (if (and (setq path (psk-paths-pick 1))
;;;	   (setq prop (psk-key-createvalue-prompt))
;;;;;;	   (setq p (getpoint "指定插入点"))
;;;      )
;;;    (psk-path-createattach
;;;      (caar path)
;;;      (p-line-closestpoint
;;;	(psk-comp-getename (caar path))
;;;	(cadar path)
;;;	t
;;;      )
;;;      prop
;;;    )
;;;  )

;;;  (princ)
;;;)
;; (psk-key-set (car (entsel)) '("A" . 1000))
(defun psk-key-set (en prop / a p)
  (setq	prop (p-set (p-xprop-getall en "PSK-BLOCK") prop)
	p    (p-dxf en 10)
	a    (p-dxf en 50)
  )

  (psk-key-createblock
    p
    a
    prop
    (psk-key-gettype (psk-key-getdef (p-get prop "NAME")))
    en
  )
)
;;


;;;(psk-key-evalkey '(("NAME" . "FL-PL") ("PN" . "2.5") ("DN" . 100)))

(defun psk-csv-readline	(file)
  (vl-string-trim " \t\n," (read-line file))
)
;;;_$ (p-string-tokenize "OD:0-FlangeDiameter" ":-")
;;;("OD" "0" "FlangeDiameter")
;;;_$ (p-string-tokenize "BoltArrangeDiameter" ":-")
;;;("BoltArrangeDiameter")
;; (psk-key-fromcsvfile "D:/Profile/desktop/dd3/bin/config/sizes/Flange PN.csv" "FL-PL-2.5-700")
(defun psk-key-fromcsvfile (filename key / e e1 e2 file head line r wc)
  (if (setq file (open filename "r"))
    (progn
      (setq head (psk-csv-readline file)
	    head (p-string-tokenize head ",")
	    head (mapcar '(lambda (e) (p-string-tokenize e ":-")) head)
	    line (psk-csv-readline file)
	    wc	 (strcat key "*")
      )

      (while line
	(if (wcmatch line wc)
	  (setq	line (p-string-tokenize line ",")
		r    (mapcar '(lambda (e1 e2)
				(cons (car e1)
				      (cond ((= "0" (cadr e1))
					     (atoi e2)
					    )
					    ((= "1" (cadr e1))
					     (atof e2)
					    )
					    (t
					     e2
					    )
				      )
				)
			      )
			     head
			     line
		     )
		line nil
	  )
	  (setq line (psk-csv-readline file))
	)
      )
      (close file)
    )
    (princ
      (strcat "\nError (psk-key-fromcsvfile): 无法打开文件 \""
	      filename
	      "\""
      )
    )
  )
  r
)