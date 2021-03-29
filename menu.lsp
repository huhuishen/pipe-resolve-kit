(vl-load-com)



;;(gxl-RemoveMenuItem POPName) �Ƴ������˵����ɹ�����T
;;; ����: (gxl-RemoveMenuItem "CASS����") �Ƴ� ��CASS���ߡ� �˵�
(defun gxl-removemenuitem (popname / menubar n i menuitem menu tag)
  (setq menubar (vla-get-menubar (vlax-get-acad-object)))
  ;; �Ҳ˵� Item 
  (setq menuitem (gxl-catchapply 'vla-item (list menubar popname)))
  (if menuitem
    (gxl-catchapply 'vla-removefrommenubar (list menuitem))
  )
)
;;���� Gxl-AddCassMenu ���CASS�˵�
;;;�﷨: (Gxl-AddCassMenu MenuGroupName POPName PopItems InsertBeforeItem) 
;;MenuGroupName Ҫ����Ĳ˵�������
;;POPName �����˵�����
;;PopItems �����˵��б�
;;   �� '((��ǩ ���� �����ִ� �μ�����)...) ��Ϊ�����˵��б�ע�������Ҫ��һ���ո�
;;InsertBeforeItem �ڸò˵�������֮ǰ���룬���� "������"����Ϊ nil,��������
(defun gxl-addcassmenu (menugroupname		  popname      popitems	    insertbeforeitem
			/	     menubar	  n	       i	    menuitem
			popupmenu    k		  tmp	       subpopupmenu
		       )
  ;;ж��ԭ�в˵�
  (gxl-removemenuitem popname)

  (setq menubar (vla-get-menubar (vlax-get-acad-object)))
  (if insertbeforeitem
    (progn
      ;; ���Ҳ˵��������䡱
      (setq n (vla-get-count menubar))
      (setq i (1- n))
      (while
	(and (>= i 0)			; û�г�������
	     (/= insertbeforeitem
		 (vla-get-name (setq menuitem (vla-item menubar i)))
	     )				; �ҵ�"������"�˵���
	)
	 (setq i (1- i))
      )
      (if (< i 0)			; ���û���ļ��˵�, ȡ���һ���˵��˵�
	(setq i (vla-get-count menubar))
      )
    )
    (setq i (vla-get-count menubar)) ;_  ȡ���һ���˵��˵�
  )
  ;;����"CASS����"�˵���
  (if (not
	(setq popupmenu
	       (gxl-catchapply
		 'vla-item
		 (list
		   (vla-get-menus
		     (vla-item
		       (vla-get-menugroups (vlax-get-acad-object))
		       menugroupname ;_ "�������߼�" �˵�������
		     )
		   )
		   popname ;_ "CASS����" �����˵�����
		 )
	       )
	)
      )
    (setq popupmenu
	   (vla-add
	     (vla-get-menus
	       (vla-item (vla-get-menugroups (vlax-get-acad-object))
			 menugroupname ;_ "�������߼�" �˵�������
	       )
	     )
	     popname ;_ "CASS����" �����˵�����
	   )
    )
  )
  ;;���Menu����
  (vlax-for popupmenuitem popupmenu
    (vla-delete popupmenuitem)
  )
  ;;����"CASS����"�˵���
  (vla-insertinmenubar popupmenu i)
  (gxl-insertpopmenuitems popupmenu popitems)
  (princ)
)

;;���� gxl-insertPopMenuItems �������˵���
;;�﷨: (gxl-insertPopMenuItems popupmenu PopItems)
;;popupmenu �˵���vla����
;;PopItems �����˵��б�
;;   �� '((��ǩ ���� �����ִ� �μ�����)...) ��Ϊ�����˵��б�ע�������Ҫ��һ���ո�
(defun gxl-insertpopmenuitems (popupmenu popitems / k tmp)
  (setq k 0)
  ;;����"CASS����"�˵�����Ŀ
  (mapcar
    (function
      (lambda (x / label cmdstr hlpstr subitems tmp)
	(setq label    (car x)
	      cmdstr   (cadr x)
	      hlpstr   (caddr x)
	      subitems (cadddr x)
	)
	(if (= label "--")
	  ;; ����ָ���
	  (vla-addseparator
	    popupmenu
	    (setq k (1+ k))
	  )
	  (if (and label cmdstr)
	    ;; ����˵���
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
	    ;; ������һ���Ӳ˵�
	    (progn
	      (setq tmp
		     (vla-addsubmenu
		       popupmenu
		       (setq k (1+ k))
		       label
		     )
	      )
	      (if subitems ;_ ����Ӽ��˵�
		(gxl-insertpopmenuitems tmp subitems)
	      )
	    )
	  )
	)
      )
    )
    ;;'((��ǩ ���� �����ִ� �μ��˵���)) ��Ϊ�˵��ע�������Ҫ��һ���ո�
    popitems
  )
)
;;���� gxl-CatchApply �ض��� VL-CATCH-ALL-APPLY 
;;�﷨: (gxl-CatchApply fun args)
;;���� fun ���� �� distance or 'distance
;;     args �����Ĳ�����
;;����ֵ: �纯�����д��󷵻�nil,���򷵻غ����ķ���ֵ
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


;;������:�Զ���Ӳ˵��� By Gu_xl ����ͨ��
;;�˵����ز��Բ���
(defun hj-menugroupload	(/ items items1)
  ;;'((��ǩ ���� �����ִ� �μ��˵���)) ��Ϊ�˵��ע�������Ҫ��һ���ո�

  (gxl-addcassmenu
    "ACAD" ;_ ���в˵�������
    "PSK" ;_ ��ʾ��Pop�˵�������
    '(("���ùܵ�(&A)..." "ar " "���ùܵ�")
       ("����"
	 "\003\003dc "
	 "����ѡ��·��������λ�õ���Ϣ���ɹܵ����Ӽ�����ͨ��ͨ�ĵ�һ��ѡ��ӦΪֱͨ��"
       )
       ("�ƶ��ܵ�"
	 "\003\003movepath "
	 "��עѡ���Ĺܵ�"
       )
       ("�ƶ������ܵ�"
	 "\003\003movepathresize "
	 "��עѡ���Ĺܵ�"
       )
       ("��ע�ܵ�" "\003\003bz " "��עѡ���Ĺܵ�")
       ("�ػ�" "\003\003dr " "���ѡ���Ļ��ƽ�����ػ�ѡ���Ĺܼ�")
       ("--" nil nil)
       ("���벿��" "\003\003iv " "����ܵ�����")
       ("���벿����" "\003\003(psk-keypack-createinpath (psk-paths-pick 1) \"AXIAL-FAN-GROUP\") " "����ܵ�������")
       ("--" nil nil)
       ("��ѯ�༭(&C)..." "\003\003cx " "��ѯ�༭ѡ���Ĺܼ�����")
       ("������֧"
	 "\003\003showbranchs "
	 "��ʾ��ָ���ܿڵ�����ĩ�˵Ĺܵ���֧"
       )
       ("��������"
	 "\003\003sumbranchs "
	 "ָ��ĩ�����������������йܵ���ָ���ܹܿڵ�����"
       )
;;;       ("�����嵥" "bom " "�����嵥")
       ("--" nil nil)
       ("����(&S)..." "\003\003pskconfig " "ϵͳ��������")
       ("����..." "\003\003pskabout " "����PSK")
     )
;;;	  '("--" nil nil) ;_ "--" ��ʾ����ָ���
;;;	  (list	"ͼ��"
;;;		nil
;;;		nil
;;;		(list '("��׼��"
;;;			"\003\003(load \"X:/09HTools/HLayerstd.VLX\") _HLayerstd "
;;;			"˵������"
;;;		       )
;;;		      '("��׼ͼ��"
;;;			"\003\003(load \"X:/09HTools/HLayers.VLX\") _HLayers "
;;;			"˵������"
;;;		       )
;;;		      '("���ʴ���"
;;;			"\003\003(load \"X:/09HTools/HExlayers.VLX\") _HExlayers "
;;;			"˵������"
;;;		       )
;;;		)
;;;	  ) ;_ ����һ���Ӳ˵���������Ϊnil����ʾ�����¼��Ӳ˵�
;;;    )
    nil ;_ �ڲ˵���"������"֮ǰ���룬��Ϊ nil,��������
  )
  (princ)
)


(defun hj-menugroupunload ()
  (gxl-removemenuitem "Эͬ����")
)


(hj-menugroupload)


;;;(vlisp-compile 'lsm "D:\\Support\\Desktop\\lisp\\Эͬ���2019\\hj-menuload.lsp" )

(princ)