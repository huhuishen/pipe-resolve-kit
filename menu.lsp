(vl-load-com)



;;(gxl-RemoveMenuItem POPName) 移除下拉菜单，成功返回T
;;; 例如: (gxl-RemoveMenuItem "CASS工具") 移除 “CASS工具” 菜单
(defun gxl-removemenuitem (popname / menubar n i menuitem menu tag)
  (setq menubar (vla-get-menubar (vlax-get-acad-object)))
  ;; 找菜单 Item 
  (setq menuitem (gxl-catchapply 'vla-item (list menubar popname)))
  (if menuitem
    (gxl-catchapply 'vla-removefrommenubar (list menuitem))
  )
)
;;函数 Gxl-AddCassMenu 添加CASS菜单
;;;语法: (Gxl-AddCassMenu MenuGroupName POPName PopItems InsertBeforeItem) 
;;MenuGroupName 要插入的菜单组名称
;;POPName 下拉菜单名称
;;PopItems 下拉菜单列表
;;   如 '((标签 命令 帮助字串 次级子项)...) 表为下拉菜单列表，注意命令后要有一个空格
;;InsertBeforeItem 在该菜单条名称之前插入，例如 "工具箱"，若为 nil,则插在最后
(defun gxl-addcassmenu (menugroupname		  popname      popitems	    insertbeforeitem
			/	     menubar	  n	       i	    menuitem
			popupmenu    k		  tmp	       subpopupmenu
		       )
  ;;卸载原有菜单
  (gxl-removemenuitem popname)

  (setq menubar (vla-get-menubar (vlax-get-acad-object)))
  (if insertbeforeitem
    (progn
      ;; 查找菜单“工具箱”
      (setq n (vla-get-count menubar))
      (setq i (1- n))
      (while
	(and (>= i 0)			; 没有超过上限
	     (/= insertbeforeitem
		 (vla-get-name (setq menuitem (vla-item menubar i)))
	     )				; 找到"工具箱"菜单条
	)
	 (setq i (1- i))
      )
      (if (< i 0)			; 如果没有文件菜单, 取最后一条菜单菜单
	(setq i (vla-get-count menubar))
      )
    )
    (setq i (vla-get-count menubar)) ;_  取最后一条菜单菜单
  )
  ;;创建"CASS工具"菜单条
  (if (not
	(setq popupmenu
	       (gxl-catchapply
		 'vla-item
		 (list
		   (vla-get-menus
		     (vla-item
		       (vla-get-menugroups (vlax-get-acad-object))
		       menugroupname ;_ "测量工具集" 菜单组名称
		     )
		   )
		   popname ;_ "CASS工具" 下拉菜单名称
		 )
	       )
	)
      )
    (setq popupmenu
	   (vla-add
	     (vla-get-menus
	       (vla-item (vla-get-menugroups (vlax-get-acad-object))
			 menugroupname ;_ "测量工具集" 菜单组名称
	       )
	     )
	     popname ;_ "CASS工具" 下拉菜单名称
	   )
    )
  )
  ;;清除Menu子项
  (vlax-for popupmenuitem popupmenu
    (vla-delete popupmenuitem)
  )
  ;;插入"CASS工具"菜单条
  (vla-insertinmenubar popupmenu i)
  (gxl-insertpopmenuitems popupmenu popitems)
  (princ)
)

;;函数 gxl-insertPopMenuItems 逐项插入菜单条
;;语法: (gxl-insertPopMenuItems popupmenu PopItems)
;;popupmenu 菜单条vla对象
;;PopItems 下拉菜单列表
;;   如 '((标签 命令 帮助字串 次级子项)...) 表为下拉菜单列表，注意命令后要有一个空格
(defun gxl-insertpopmenuitems (popupmenu popitems / k tmp)
  (setq k 0)
  ;;插入"CASS工具"菜单子项目
  (mapcar
    (function
      (lambda (x / label cmdstr hlpstr subitems tmp)
	(setq label    (car x)
	      cmdstr   (cadr x)
	      hlpstr   (caddr x)
	      subitems (cadddr x)
	)
	(if (= label "--")
	  ;; 插入分隔符
	  (vla-addseparator
	    popupmenu
	    (setq k (1+ k))
	  )
	  (if (and label cmdstr)
	    ;; 插入菜单条
	    (progn
	      (setq tmp
		     (vla-addmenuitem
		       popupmenu
		       (setq k (1+ k))
		       label
		       cmdstr
		     )
	      )
	      (vla-put-helpstring tmp hlpstr)
	    )
	    ;; 插入下一级子菜单
	    (progn
	      (setq tmp
		     (vla-addsubmenu
		       popupmenu
		       (setq k (1+ k))
		       label
		     )
	      )
	      (if subitems ;_ 添加子级菜单
		(gxl-insertpopmenuitems tmp subitems)
	      )
	    )
	  )
	)
      )
    )
    ;;'((标签 命令 帮助字串 次级菜单项)) 表为菜单项，注意命令后要有一个空格
    popitems
  )
)
;;函数 gxl-CatchApply 重定义 VL-CATCH-ALL-APPLY 
;;语法: (gxl-CatchApply fun args)
;;参数 fun 函数 如 distance or 'distance
;;     args 函数的参数表
;;返回值: 如函数运行错误返回nil,否则返回函数的返回值
(defun gxl-catchapply (fun args / result)
  (if
    (not
      (vl-catch-all-error-p
	(setq result
	       (vl-catch-all-apply
		 (if (= 'sym (type fun))
		   fun
		   (function fun)
		 )
		 args
	       )
	)
      )
    )
     result
  )
)


;;程序功能:自动添加菜单项 By Gu_xl 明经通道
;;菜单加载测试测试
(defun hj-menugroupload	(/ items items1)
  ;;'((标签 命令 帮助字串 次级菜单项)) 表为菜单项，注意命令后要有一个空格

  (gxl-addcassmenu
    "ACAD" ;_ 已有菜单组名称
    "PSK" ;_ 显示的Pop菜单项名称
    '(("布置管道(&A)..." "ar " "布置管道")
       ("连接"
	 "\003\003dc "
	 "根据选定路径的数量位置等信息生成管道连接件，三通四通的第一二选择应为直通段"
       )
       ("移动管道"
	 "\003\003movepath "
	 "标注选定的管道"
       )
       ("移动伸缩管道"
	 "\003\003movepathresize "
	 "标注选定的管道"
       )
       ("标注管道" "\003\003bz " "标注选定的管道")
       ("重绘" "\003\003dr " "清除选定的绘制结果并重绘选定的管件")
       ("--" nil nil)
       ("插入部件" "\003\003iv " "插入管道部件")
       ("插入部件包" "\003\003(psk-keypack-createinpath (psk-paths-pick 1) \"AXIAL-FAN-GROUP\") " "插入管道部件包")
       ("--" nil nil)
       ("查询编辑(&C)..." "\003\003cx " "查询编辑选定的管件参数")
       ("遍历分支"
	 "\003\003showbranchs "
	 "显示从指定管口到所有末端的管道分支"
       )
       ("汇总流量"
	 "\003\003sumbranchs "
	 "指定末端流量，并计算所有管道到指定总管口的流量"
       )
;;;       ("材料清单" "bom " "材料清单")
       ("--" nil nil)
       ("设置(&S)..." "\003\003pskconfig " "系统变量设置")
       ("关于..." "\003\003pskabout " "关于PSK")
     )
;;;	  '("--" nil nil) ;_ "--" 表示插入分隔符
;;;	  (list	"图层"
;;;		nil
;;;		nil
;;;		(list '("标准化"
;;;			"\003\003(load \"X:/09HTools/HLayerstd.VLX\") _HLayerstd "
;;;			"说明文字"
;;;		       )
;;;		      '("标准图层"
;;;			"\003\003(load \"X:/09HTools/HLayers.VLX\") _HLayers "
;;;			"说明文字"
;;;		       )
;;;		      '("提资处理"
;;;			"\003\003(load \"X:/09HTools/HExlayers.VLX\") _HExlayers "
;;;			"说明文字"
;;;		       )
;;;		)
;;;	  ) ;_ 含下一级子菜单，命令若为nil，表示还有下级子菜单
;;;    )
    nil ;_ 在菜单项"工具箱"之前插入，若为 nil,则插在最后
  )
  (princ)
)


(defun hj-menugroupunload ()
  (gxl-removemenuitem "协同工具")
)


(hj-menugroupload)


;;;(vlisp-compile 'lsm "D:\\Support\\Desktop\\lisp\\协同软件2019\\hj-menuload.lsp" )

(princ)