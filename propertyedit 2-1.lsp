;; ͨ�����Ա༭�Ի��� v2.0 2017-1-12
;; ͨ�����Ա༭�Ի��� v2.1 2017-9-28
;; ͨ�����Ա༭�Ի��� v3.0 2021-2-22 ��Ҫfuncions.lsp


(defun desc-item (key idx / des)
  (setq des (assoc key _desc))
  (if des
    (nth idx des)
  )
)
;; (setq options '(("AL" 1000 "���뷽ʽ" "AL\n��ܶ��뷽ʽ" (("T" "��") ("C" "����") ("B" "��") ""))))
;;;_$ (desc-gettype "AL")
;;;"���뷽ʽ"
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
;;;"���뷽ʽ"
(defun desc-getnameshow	(key)
  (desc-item key 2)
)
;;;_$ (desc-gettip "AL")
;;;"AL\n��ܶ��뷽ʽ"
(defun desc-gettip (key)
  (desc-item key 3)
)
;; ѡ������һ��Ϊ""ʱ ����ʾ�����û������б�������ݣ��������ѡ���б��е�����
;;;_$ (desc-getoptions "AL")
;;;(("T" "��") ("C" "����") ("B" "��") "")
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
	mark "" ;_�޸ı��
  )
  (if (and key (/= "" key))
    (progn
      ;; ��������
      (if (null (setq text (desc-getnameshow key)))
	;; ������ʾ����δ�ṩʱ ʹ������������Ϊ��ʾ����
	(setq text key)
      )
      ;; ����ֵ
      (if (setq value (p-get _changes key))
	;; �����ڱ��λỰ�����޸�
	(setq mark "\t*") ;_ �޸ı��
	(setq value (p-get _props key))
      )

      (setq options (desc-getoptions key))
      (if (and options (vl-every 'vl-consp options))
	(if (cadr (setq value (assoc value options)))
	  (setq value (cadr value))
	  (setq value (car value))
	)
      )

      (setq text (strcat " " ;_ ��ͷ����
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
	;; �ҵ������Ե��޸ļ�¼
	(if (= newvalue (p-get _props key))
	  ;; ���Ի�ԭΪ��ʼֵ�������޸ļ�¼��
	  (setq _changes (p-unset _changes key))
	  ;; ����޸ļ�¼
	  (setq _changes (p-set _changes (cons key newvalue)))
	)
	;; ��������δ�޸�
	(if (/= newvalue (p-get _props key))
	  (setq _changes (p-set _changes (cons key newvalue)))
	)
      )
      ;; ִ���Զ������
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
  ;; ��ű�ʾ���ص����ԣ���������������б���
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

  ;; ������Կ�
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
  (start_list "PROP_LIST" 1 pos) ;_ �޸�ָ��λ���ϵ�����
  (add_list (prop-rowtext cur))
  (end_list)

  (if setpos
    (set_tile "PROP_LIST" (itoa pos))
  )
)
(defun propbox-onchange	(newsel / tip value)
  ;; Ĭ�Ͻ��ñ༭�ؼ�
  (mapcar 'ctrl-disable '("VALUE_TEXT" "VALUE_LIST"))

  (if (setq _cursel newsel)
    (setq _curkey (car (nth (atoi _cursel) _props)))
  )

;;;    (if	(= "" _curkey)
;;;      ;; Ϊ�ָ�������Ч����ʱ
;;;      (progn
;;;	(setq _options nil)
;;;	(valuebox-update "")
;;;	(optionbox-update nil)
;;;	(infobox-update "")
;;;      )
  (if (null _desc)
    (progn
      ;; δ�ṩdescptionʱ��Ĭ�������ı��ؼ�
      (ctrl-enable "VALUE_TEXT")
      (valuebox-update (prop-get _curkey)) ;_ �ı�����ֵͬ������
      (setq _options nil)
      (optionbox-update nil)
    )
    (progn
      (setq _options (desc-getoptions _curkey))
      (if (and _options (listp _options))
	;; ��������Ч��ѡ���б�ʱ
	(progn
	  (ctrl-enable "VALUE_LIST") ;_ �����б�ؼ�

	  (if (= "" (last _options))
	    (ctrl-enable "VALUE_TEXT") ;_ δ���ý��޶����б���ѡ��ʱ�����ı��ؼ�
	  )

	  (if (vl-every 'vl-consp _options)
	    (setq value (cadr (assoc (prop-get _curkey) _options)))
	    (setq value (prop-get _curkey))
	  )

	  (valuebox-update value)
	  (optionbox-update (prop-get _curkey))
	)

	;; δ����ѡ���б�ͬʱ�����Էǹ�ʽʱĬ�������ı��ؼ�
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
;; ѡ������ݸ���
(defun optionbox-update	(selvalue / e pos)
  (if (and selvalue (vl-consp _options))
    (progn
      (start_list "VALUE_LIST")
      (mapcar (function add_list)
	      (mapcar (function	(lambda	(e)
				  (if (vl-consp e)
				    (if	(cadr e) ;_ ("SA" "�ͷ�") ("SA")
				      (cadr e) ;_"�ͷ�"
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
	(set_tile "VALUE_LIST" (itoa pos)) ;_ ����ѡ���б��еĵ�ǰֵΪѡ��״̬
      )
    )
    (progn
      ;; Ϊ��ʽ����Ч����ʱ����б��
      (start_list "VALUE_LIST")
      (end_list)
    )
  )
)
;; ѡ���ѡ������
(defun optionbox-onchange (idx / newvalue)
  (setq newvalue (nth (atoi idx) _options))
  (if (vl-consp newvalue)
    (progn
      (prop-set _curkey (car newvalue)) ;_ ("SA" "�ͷ�")
      (valuebox-update
	(if (cadr newvalue)
	  (cadr newvalue)
	  (car newvalue)
	)
      )

      (if (caddr newvalue)
	;; ���к���
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
      ;; ���㹫ʽ
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
      (propbox-onchange _cursel) ;_ ǿ�Ƹ����б��ı��ؼ�����

      (action_tile "PROP_LIST" "(propbox-onchange $VALUE)")
      (action_tile "VALUE_TEXT" "(valuebox-onchange $VALUE)")
      (action_tile "VALUE_LIST" "(optionbox-onchange $VALUE)")
      (action_tile "OK" "(done_dialog 1)")

      (setq rt (start_dialog))
      (unload_dialog id)

      ;; ���浱ǰ�༭���Ե���ŵ�ָ�������У�����Ϊ�´δ򿪶Ի���ʱ��Ĭ��ѡ����Ŀ
      (if (and _selkey (= 'sym (type _selkey)))
	(set _selkey _curkey)
      )
      ;; OK�رնԻ���ʱ����һ���޸������б����򷵻�nil
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