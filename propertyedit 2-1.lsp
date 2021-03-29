;; 通用属性编辑对话框 v2.0 2017-1-12
;; 通用属性编辑对话框 v2.1 2017-9-28
;; 通用属性编辑对话框 v3.0 2021-2-22 需要funcions.lsp


(defun desc-item (key idx / des)
  (setq des (assoc key _desc))
  (if des
    (nth idx des)
  )
)
;; (setq options '(("AL" 1000 "对齐方式" "AL\n风管对齐方式" (("T" "顶") ("C" "中心") ("B" "底") ""))))
;;;_$ (desc-gettype "AL")
;;;"对齐方式"
(defun desc-gettype (key / r)
  (if (setq r (desc-item key 1))
    r
    (progn
      (setq tp (type (prop-get key)))
      (cond ((= 'str tp)
	     1000
	    )
	    ((= 'real tp)
	     1040
	    )
	    ((= 'int tp)
	     1070
	    )
	    ((= 'list tp)
	     1010
	    )
      )
    )
  )
)
;;;_$ (desc-getnameshow "AL")
;;;"对齐方式"
(defun desc-getnameshow	(key)
  (desc-item key 2)
)
;;;_$ (desc-gettip "AL")
;;;"AL\n风管对齐方式"
(defun desc-gettip (key)
  (desc-item key 3)
)
;; 选项表最后一项为""时 ，表示允许用户输入列表外的数据，否则仅能选择列表中的内容
;;;_$ (desc-getoptions "AL")
;;;(("T" "顶") ("C" "中心") ("B" "底") "")
(defun desc-getoptions (key)
  (desc-item key 4)
)

(defun desc-getfunc (key)
  (desc-item key 5)
)

(defun ctrl-disable (e)
  (mode_tile e 1)
)

(defun ctrl-enable (e)
  (mode_tile e 0)
)
;; (prop-rowtext '("A" . 1))
(defun prop-rowtext (prop / key text value mark options)
  (setq	key  (car prop)
	mark "" ;_修改标记
  )
  (if (and key (/= "" key))
    (progn
      ;; 属性名称
      (if (null (setq text (desc-getnameshow key)))
	;; 属性显示名称未提供时 使用属性名称做为显示名称
	(setq text key)
      )
      ;; 属性值
      (if (setq value (p-get _changes key))
	;; 属性在本次会话中已修改
	(setq mark "\t*") ;_ 修改标记
	(setq value (p-get _props key))
      )

      (setq options (desc-getoptions key))
      (if (and options (vl-every 'vl-consp options))
	(if (cadr (setq value (assoc value options)))
	  (setq value (cadr value))
	  (setq value (car value))
	)
      )

      (setq text (strcat " " ;_ 开头缩进
			 text
			 "\t"
			 (if value
			   (vl-princ-to-string value)
			   ""
			 )
			 mark
		 )
      )
    )
  )
)
(defun prop-get	(key / value)
  (if key
    (progn
      (setq value (p-get _changes key))
      (if value
	value
	(p-get _props key)
      )
    )
  )
)
(defun prop-set	(key newvalue / oldvalue p value func)
  (if key
    (progn
      (if (setq oldvalue (p-get _changes key))
	;; 找到该属性的修改记录
	(if (= newvalue (p-get _props key))
	  ;; 属性还原为初始值（消除修改记录）
	  (setq _changes (p-unset _changes key))
	  ;; 添加修改记录
	  (setq _changes (p-set _changes (cons key newvalue)))
	)
	;; 该属性尚未修改
	(if (/= newvalue (p-get _props key))
	  (setq _changes (p-set _changes (cons key newvalue)))
	)
      )
      ;; 执行自定义过程
;;;	(if (and
;;;	      (setq func (assoc key _desc))
;;;	      (setq func (nth 5 func))
;;;	    )
;;;	  (eval func)
;;;	)
    )
  )
)
(defun propbox-pos (key / cur pos)
  (setq	cur (assoc key _props)
	pos (vl-position cur _props)
  )
)
(defun propbox-update (props desc / propitems)
  ;; 点号表示隐藏的属性，不会出现在属性列表中
  (setq
    _desc     desc
;;;      _props	(vl-remove-if
;;;		  '(lambda (e) (= "." (substr (car e) 1 1)))
;;;		  props
;;;		)
    _props    props
    _props    (p-set (mapcar 'list (mapcar 'car desc)) _props)
;;;      _changes	(p-unset _changes "SERV")
    propitems (mapcar 'prop-rowtext _props)
  )

  ;; 填充属性框
  (start_list "PROP_LIST")
  (mapcar 'add_list propitems)
  (end_list)

  (if (null _selkey)
    (setq _cursel "0")
    (if	(= 'sym (type _selkey))
      (if (setq _cursel (propbox-pos (vl-symbol-value _selkey)))
	(setq _cursel (itoa _cursel))
	(setq _cursel "0")
      )
      (setq _cursel (itoa (propbox-pos _selkey)))
    )
  )
  (set_tile "PROP_LIST" _cursel)
)
(defun propbox-updaterow (key setpos / cur pos)
  (setq	pos (propbox-pos key)
	cur (assoc key _props)
  )
  (start_list "PROP_LIST" 1 pos) ;_ 修改指定位置上的数据
  (add_list (prop-rowtext cur))
  (end_list)

  (if setpos
    (set_tile "PROP_LIST" (itoa pos))
  )
)
(defun propbox-onchange	(newsel / tip value)
  ;; 默认禁用编辑控件
  (mapcar 'ctrl-disable '("VALUE_TEXT" "VALUE_LIST"))

  (if (setq _cursel newsel)
    (setq _curkey (car (nth (atoi _cursel) _props)))
  )

;;;    (if	(= "" _curkey)
;;;      ;; 为分隔符或无效属性时
;;;      (progn
;;;	(setq _options nil)
;;;	(valuebox-update "")
;;;	(optionbox-update nil)
;;;	(infobox-update "")
;;;      )
  (if (null _desc)
    (progn
      ;; 未提供descption时，默认启用文本控件
      (ctrl-enable "VALUE_TEXT")
      (valuebox-update (prop-get _curkey)) ;_ 文本框中值同步更新
      (setq _options nil)
      (optionbox-update nil)
    )
    (progn
      (setq _options (desc-getoptions _curkey))
      (if (and _options (listp _options))
	;; 设置了有效的选择列表时
	(progn
	  (ctrl-enable "VALUE_LIST") ;_ 启用列表控件

	  (if (= "" (last _options))
	    (ctrl-enable "VALUE_TEXT") ;_ 未设置仅限定在列表中选择时启用文本控件
	  )

	  (if (vl-every 'vl-consp _options)
	    (setq value (cadr (assoc (prop-get _curkey) _options)))
	    (setq value (prop-get _curkey))
	  )

	  (valuebox-update value)
	  (optionbox-update (prop-get _curkey))
	)

	;; 未设置选择列表同时该属性非公式时默认启用文本控件
	(progn
	  (if (not (desc-getfunc _curkey))
	    (ctrl-enable "VALUE_TEXT")
	  )
	  (valuebox-update (prop-get _curkey))
	  (setq _options nil)
	  (optionbox-update nil)
	)
      )

      (if (setq tip (desc-gettip _curkey))
	(infobox-update tip)
      )
    )
  )
)
;; 选项框内容更新
(defun optionbox-update	(selvalue / e pos)
  (if (and selvalue (vl-consp _options))
    (progn
      (start_list "VALUE_LIST")
      (mapcar (function add_list)
	      (mapcar (function	(lambda	(e)
				  (if (vl-consp e)
				    (if	(cadr e) ;_ ("SA" "送风") ("SA")
				      (cadr e) ;_"送风"
				      (car e) ;_"SA"
				    )
				    (vl-princ-to-string e) ;_"SA"
				  )
				)
		      )
		      _options
	      )
      )
      (end_list)

      (if (and _options (vl-every 'vl-consp _options))
	(setq pos (vl-position (assoc selvalue _options) _options))
	(setq pos (vl-position selvalue _options))
      )

      (if pos
	(set_tile "VALUE_LIST" (itoa pos)) ;_ 设置选择列表中的当前值为选定状态
      )
    )
    (progn
      ;; 为公式或无效数据时清空列表框
      (start_list "VALUE_LIST")
      (end_list)
    )
  )
)
;; 选项框选定项变更
(defun optionbox-onchange (idx / newvalue)
  (setq newvalue (nth (atoi idx) _options))
  (if (vl-consp newvalue)
    (progn
      (prop-set _curkey (car newvalue)) ;_ ("SA" "送风")
      (valuebox-update
	(if (cadr newvalue)
	  (cadr newvalue)
	  (car newvalue)
	)
      )

      (if (caddr newvalue)
	;; 运行函数
	(eval (read (caddr newvalue)))
      )
    )
    (progn
      (prop-set _curkey newvalue) ;_ "SA"
      (valuebox-update newvalue)
    )
  )

  (propbox-updaterow _curkey t)
  (expr-update)
)
;;
(defun valuebox-update (text)
  (if text
    (setq text (vl-princ-to-string text))
    (setq text "")
  )
  (set_tile "VALUE_TEXT" text)
)
(defun valuebox-onchange (text / tp)
  (setq tp (desc-gettype _curkey))
  (cond
    ((= 1070 tp) (setq text (atoi text)))
    ((= 1040 tp) (setq text (atof text)))
    ((= 1010 tp) (setq text (p-string-tokenize text ", ")))
  )
  (prop-set _curkey text)
  (propbox-updaterow _curkey t)
  (expr-update)
)
;;
(defun infobox-update (msg)
  (if msg
    (set_tile "VALUE_INFO" msg)
    (set_tile "VALUE_INFO" "")
  )
)

(defun expr-update (/ func)
  (foreach e _props
    (if	(setq func (desc-getfunc (car e)))
      ;; 计算公式
      (progn
	(prop-set (car e) (expr-eval func))
	(propbox-updaterow (car e) nil)
      )
    )
  )
)

(defun expr-eval (expr /)
  (while (vl-string-search "{" expr)
    (setq expr (vl-string-subst "(prop-get \"" "{" expr))
  )
  (while (vl-string-search "}" expr)
    (setq expr (vl-string-subst "\")" "}" expr))
  )
  (vl-catch-all-apply 'eval (list (read expr)))
)
;;(expr-eval "(/ {Flow} (/ (* pi {Diameter} {Diameter}) 4.e-6) (/ 1. 3600.))")


(defun propertybag-edit	(props	   _desc     _selkey   returnwhole
			 /	   _props    _changes  _cursel
			 _options  id	     rt
			)



  (setq id (load_dialog (psk-get-filename "propertyedit.dcl")))
  (if (and (>= id 0)
	   (new_dialog "PropertyEdit" id)
      )
    (progn
      (propbox-update props _desc)
      (propbox-onchange _cursel) ;_ 强制更新列表及文本控件内容

      (action_tile "PROP_LIST" "(propbox-onchange $VALUE)")
      (action_tile "VALUE_TEXT" "(valuebox-onchange $VALUE)")
      (action_tile "VALUE_LIST" "(optionbox-onchange $VALUE)")
      (action_tile "OK" "(done_dialog 1)")

      (setq rt (start_dialog))
      (unload_dialog id)

      ;; 保存当前编辑属性的序号到指定符号中，可做为下次打开对话框时的默认选中条目
      (if (and _selkey (= 'sym (type _selkey)))
	(set _selkey _curkey)
      )
      ;; OK关闭对话框时返回一个修改内容列表，否则返回nil
      (setq _changes
	     (vl-remove-if
	       (function (lambda (e) (vl-catch-all-error-p (cdr e)))
	       )
	       _changes
	     )
      )
      (if (= rt 1)
	(if returnwhole
	  (p-set _props _changes)
	  _changes
	)
	rt
      )
    )
  )
)