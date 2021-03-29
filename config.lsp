(setq $psk-about "管道求解组件 PSK 0.1")

(setq $psk-regroot	"HKEY_CURRENT_USER\\Software\\InkPaint Computing\\PSK"
      $psk-install-path	(vl-registry-read $psk-regroot "Install Path")
)

(if (or (null $psk-install-path) (= $psk-install-path ""))
  (progn (princ "\n未设置应用程序安装路径。")
	 (vl-registry-write $psk-regroot "Install Path" "")
	 (vl-exit-with-value 1)
  )
)


;;; BOOKMARK - 变量初始化

;;;_$ (psk-get-filename "a")
;;;"C:\\Users\\hhs\\Desktop\\dd2\\a"
;;;_$ (psk-get-filename "\\a")
;;;"C:\\Users\\hhs\\Desktop\\dd2\\a"
;;;_$ (psk-get-filename "/a")
;;;"C:\\Users\\hhs\\Desktop\\dd2/a"
(defun psk-get-filename	(name / lead)
  (setq lead (substr name 1 1))
  (if (and (/= "\\" lead)
	   (/= "/" lead)
      )
    (setq name (strcat "\\" name))
  )
  (strcat $psk-install-path name)
)

;;;(defun d-combian-systems (/ rt)
;;;  (foreach pref	$psk-services
;;;    (foreach sub $PSK-subsystem
;;;      (setq rt (cons (strcat (car pref) "-" (car sub)) rt))
;;;    )
;;;  )
;;;  (reverse rt)
;;;)
;;;(d-output-list (d-combian-systems) (strcat $psk-install-path "/comb.csv"))
;;;(defun d-output-list (lst filename / file)
;;;  (if (setq file (open filename "w"))
;;;    (progn (foreach e lst (write-line (vl-princ-to-string e) file))
;;;	   (close file)
;;;    )
;;;  )
;;;)


(setq
;;;  $psk-customdir		      "config/profiles/default/"
  $psk-customdir     "config/profiles/hanjia/"
  $psk-services	     (p-csvfile-read
		       (psk-get-filename
			 (strcat $psk-customdir "services.csv")
		       )
		     )
  $psk-services-duct (p-csvfile-read
		       (psk-get-filename
			 (strcat $psk-customdir "services-duct.csv")
		       )
		     )
  $psk-subsystem     (p-csvfile-read
		       (psk-get-filename
			 (strcat $psk-customdir "subsystem.csv")
		       )
		     )
  $psk-layer-config  (p-csvfile-read
		       (psk-get-filename
			 (strcat $psk-customdir "layer-configration.csv")
		       )
		     )
)


(setq $psk-keypacks (p-load-lispfile (psk-get-filename "keypacks.lsp")))
(setq $psk-keys (p-load-lispfile (psk-get-filename "keys.lsp")))

;; 加载系统变量
(eval
  (p-load-lispfile (psk-get-filename "var.lsp"))
)

(setq $psk-prop-defination
       (eval
	 (p-load-lispfile (psk-get-filename "prop.lsp"))
       )
)


(defun psk-setting-load	(/ e settings)
  ;; 导入保存于每个dwg中的设置
  (if (setq settings (vlax-ldata-get "PSK" "SETTINGS"))
    (foreach e settings
      (set (read (car e)) (cdr e))
    )
  )

  ;; 程序因更新可能增加的变量 用默认值补充定义
  (foreach e $psk-settings-default
    (if	(null (vl-symbol-value (read(car e))))
      (set (read (car e)) (cdr e))
    )
  )

  ;; 对可能新增的变量保存
  (psk-setting-save)
)


(defun psk-setting-save	(/ e settings)
  (vlax-ldata-put
    "PSK"
    "SETTINGS"
    (mapcar '(lambda (e) (cons e (vl-symbol-value (read e))))
	    (mapcar 'car $psk-settings-default)
    )
  )
)
(defun psk-createvaluelast-load	(/ r)
  (if (setq r (vlax-ldata-get "PSK" "CREATEVALUELAST"))
    (setq $psk-path-createvaluelast (p-set $psk-path-createvaluelast r))
  )
)




(setq $psk-settings-desc
       '(("$PSK-IDEN-TEXTHEIGHT"
	  1040
	  "标注文字高度"
	  ""
	  (2.5 3.0 4.0 5.0 "")
	 )
	 ("$PSK-IDEN-WIDFACTOR"
	  1040
	  "宽度因子"
	  ""
	  (0.5 0.6 0.7 0.75 0.8 0.9 1.0 "")
	 )
	 ("$PSK-IDEN-SCALE" 1040 "全局比例" "" (50. 100. 150. 200. ""))
	 ("$PSK-IDEN-TEXTSTYLE" 1000 "标注文字样式")
	 ("$PSK-IDEN-MINLENGTH" 1040 "标注管线长度限值")
	 ("$PSK-AUTOREDRAW"
	  1000
	  "自动更新图面"
	  "绘制管道时自动更新图面"
	  (("Y" "是") ("N" "否"))
	 )
	)
)
(setq $psk-settings-default
       '(("$PSK-IDEN-TEXTHEIGHT" . 3.0) ;_标注文字默认高度
	 ("$PSK-IDEN-WIDFACTOR" . 0.7) ;_标注文字宽度比例
	 ("$PSK-IDEN-SCALE" . 100.) ;_全局比例影响标注文字
	 ("$PSK-IDEN-TEXTSTYLE" . "HJ-GBXWXT")
	 ("$PSK-IDEN-MINLENGTH" . 1000.0) ;_小于该长度的直线不标注
	 ("$PSK-AUTOREDRAW" . "Y")
	 ("$PSK-DUCT-FLEXTEND" . 50.) ;_法兰边突出长度
	)
      $psk-sel-settings
       "$PSK-IDEN-TEXTHEIGHT"
      $psk-sel-path-create
       "TYPE"
)


(defun psk-settings-change (/ change k)
  (setq	change
	 (propertybag-edit
	   (mapcar '(lambda (e) (cons e (vl-symbol-value (read e))))
		   (mapcar 'car $psk-settings-default)
	   )
	   $psk-settings-desc
	   '$psk-sel-settings
	   nil
	 )
  )
  (if (and change (/= 0 change))
    (progn
      (foreach e change
	(set (read (car e)) (cdr e))
      )
      (psk-setting-save)
    )
  )
)


(psk-setting-load)
(psk-createvaluelast-load)

(setq $addnew-textstyle $PSK-iden-textstyle)


(p-textstyle-get
  $addnew-textstyle
  "gbxwxt.shx"
  "gbhzfs.shx"
)
(setvar "LTSCALE" 1000.)