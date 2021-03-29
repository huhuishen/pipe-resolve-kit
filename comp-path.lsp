;;;2016-9-30
;;;2021-2-4

;;; ·���Ĵ������ΪLINE��CIRCLE���������ɹܵ��ಿ������ˮ�ܡ���ܵ�
;;; ·����������������ʵ�����������Ĺܿ���� ��LINE����� 10 11�����ʾ


(defun psk-path-getservice (path)
;;;  (if (wcmatch (p-get path "TYPE") "DUCT*")
;;;    (p-get path "SERVD")
;;;    (p-get path "SERV")
;;;  )
  (p-get path "SERV")
)
;;;(psk-path-create
;;;  (setq p (getpoint))
;;;  (getpoint p)
;;;  '((".CLASS" . "PATH") ("TYPE" . "RECTANGULAR") ("SERV" . "OA") ("W" . 500) ("H" . 320) ("ERF" . 0.8) ("FLOW" . 0.))
;;;)
;; ((-1 . <ͼԪ��: 7ff4e823f070>) (".CLASS" . "PATH") ("TYPE" . "RECTANGULAR") ("SERV" . "OA") ("W" . 500) ("H" . 320) ("ERF" . 0.8) ("FLOW" . 0.0))
(defun psk-path-create (p1 p2 prop / comp line)
;;;  (if (p-get prop "SERVD")
;;;    (progn
;;;      (setq prop (p-set prop (cons "SERV" (p-get prop "SERVD"))))
;;;      (setq prop (p-unset prop "SERVD"))
;;;    )
;;;  )
  (psk-set-customlayerbyid
    (strcat (psk-path-getservice prop) "-CL")
  )
  (setq	line (p-make-line p1 p2)
	comp (cons '(".CLASS" . "PATH")
		   prop
	     )
  )

  (p-xprop-set line "PSK-PATH" comp)
  (cons (cons -1 line) comp)
)
;; (psk-path-getports (car (entsel)))
;; ((10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0) (11 (45632.5 13384.1 0.0) (1.0 0.0) 0.0))
(defun psk-path-getports (path / p1 p2)
  (if (/= 'ename (type path))
    (setq path (psk-comp-getename path))
  )
  (setq	p1 (p-dxf path '(10 11))
	p2 (cadr p1)
	p1 (car p1)
  )
  (list	(list 10 p1 (polar '(0 0) (angle p2 p1) 1.) 0.)
	(list 11 p2 (polar '(0 0) (angle p1 p2) 1.) 0.)
  )
)

(defun psk-path-getlength (path /)
  (if (/= 'ename (type path))
    (setq path (psk-comp-getename path))
  )

  (apply 'distance (p-dxf path '(10 11)))
)
;; ��·������ָ����Ķ˵��Ƶ��µ�λ�ã����غ����ƶ�������Ҫ�ƶ��ĵ�
;; (psk-path-moveport (car (entsel)) (trans (getpoint) 1 0) (trans (getpoint) 1 0) t)
;; ((0.0 -172.645 0.0) (2714.66 2453.51 0.0) ...)
(defun psk-path-moveport (path p np resize / np2 offset p2 ports rp)
  (if (/= 'ename (type path))
    (setq path (psk-comp-getename path))
  )

  (setq ports (psk-ports-sort (psk-path-getports path) p))

  (if resize
    (progn
      ;; ���������ܵ�
      ;; ���ƶ��㵽�µ㣬�Ժ������Ӧ�÷������ƫ�Ʒ���
      (setq offset (mapcar '-
			   np
			   (vlax-curve-getclosestpointto
			     (p-ensure-object path)
			     np
			     t
			   )
		   )
      )

      (setq p2	(psk-port-getpos (cadr ports))
	    np2	(mapcar '+ offset p2)
      )
      (p-entmod
	path
	(list
	  (cons (psk-port-getid (car ports)) np)
	  (cons (psk-port-getid (cadr ports)) np2)
	)
      )

      (if (not (equal '(0. 0. 0.) offset 0.1))
	(setq rp (list offset p2))
      )
    )
    (progn
      ;; ���ܵ���Ϊ�������崦��
      (setq offset (mapcar '- np (psk-port-getpos (car ports))))

      (if (not (equal '(0. 0. 0.) offset 0.1))
	(progn
	  (setq	p2  (psk-port-getpos (cadr ports))
		np2 (mapcar '+ offset p2)
		rp  (list offset p2)
	  )
	  (p-entmod path
		    (list
		      (cons (psk-port-getid (car ports)) np)
		      (cons (psk-port-getid (cadr ports)) np2)
		    )
	  )
	)
      )
    )
  )
  rp
)




;; (psk-path-getportsize (car (entsel)))
(defun psk-path-getportsize (path / tp)
  (if (= 'ename (type path))
    (setq path (psk-comp-load path))
  )
  (setq tp (p-get path "TYPE"))
  (cond
    ((= tp "DUCT-RECT")
     (list (p-get path "W") (p-get path "H"))
    )
    ((= tp "DUCT-ROUND") (p-get path "DD"))
    ((= tp "PIPE") (p-get path "DN"))
  )
)

;; utils
;; (psk-paths-pick 2)
(defun psk-paths-pick (limit / path paths r)
;;;  (while (setq path (ssget ":E:S" '((0 . "LINE") (-3 ("PSK-COMP")))))
  (princ "\n")
  (while (and
	   (not (initget "  "))
	   (< (length paths) limit)
	   (/= "" (setq r (entsel "ѡ��·��:")))
	 )
    (if	(and r
	     (setq path (psk-comp-load (car r)))
	     (not (member path (mapcar 'car paths)))
	)
      (progn
	(setq paths (cons (list path (p-line-closestpoint (car r) (cadr r) nil)) paths))
	(redraw (psk-comp-getename path) 3)
      )
    )
    (princ (strcat "\rѡ���� " (itoa (length paths)) " ������, "))
  )

  (mapcar '(lambda (e) (redraw (psk-comp-getename (car e)) 4)) paths)
  (reverse paths)
)
;; (psk-line-keepend (car (entsel)) (getpoint) (getpoint))
;; ��ֱ����p�㴦�ֶϣ�����pkeep���ڵĲ���
;; ����һ����� ��ǰ������Ҫ�ƶ���p�ĵ�
(defun psk-line-keepend	(line p pkeep / a e ps)
  (setq	p  (p-line-closestpoint line p t)
	ps (p-dxf line '(10 11))
	a  (angle p pkeep)
  )

  (mapcar
    'cdr
    (vl-sort
      (mapcar 'cons
	      (mapcar '(lambda (e) (abs (- e a)))
		      (mapcar '(lambda (e) (angle p e))
			      ps
		      )
	      )
	      ps
      )
      '(lambda (e1 e2)
	 (if (equal (car e1) (car e2) 1e-3)
	   (< (distance p (cdr e1)) (distance p (cdr e2)))
	   (> (car e1) (car e2))
	 )
       )
    )
  )
)
;;



(setq $psk-reducer-length 700)

;; (psk-paths-connect (psk-paths-pick 4) t)
(defun psk-paths-connect (paths draw /)
  (setq
    picks (mapcar 'cadr paths)
    paths (mapcar 'car paths)
    lines (mapcar 'psk-comp-getename paths)
;;;    picks (mapcar '(lambda (line p)
;;;		     (p-line-closestpoint line p nil)
;;;		   )
;;;		  lines
;;;		  picks
;;;	  )
  )
  (cond
    ((= 2 (length paths))
     (if (equal	(p-line-getangle (car lines))
		(p-line-getangle (last lines))
		1e-3
	 )
       ;; �����쾶
       (progn
	 (setq ports1 (psk-comp-getports (car paths))
	       p      (p-line-closestpoint
			(car lines)
			(mapcar '(lambda (e) (/ e 2.)) (mapcar '+ (car picks) (cadr picks)))
			t
		      )
	       ports1 (psk-ports-sort ports1 p)
	 )
	 (setq r (psk-create-reducer
		   p
		   (setq $psk-reducer-length (p-edit-value "�����쾶����" $psk-reducer-length))
		   (psk-port-getangle (car ports1))
		   (psk-path-getportsize (car paths))
		   (psk-path-getportsize (last paths))
		   (setq al (psk-editor-getalignmode))
		   (list '(".CLASS" . "FIT")
			 '(".TYPE" . "REDUCER")
			 (cons "SERV" (p-get (car paths) "SERV"))
			 (cons "AL" al)
		   )
		 )
	 )
	 (psk-path-moveport
	   (car paths)
	   (car (psk-line-keepend (car lines) p (car picks)))
	   (cadr r)
	   t
	 )
	 (psk-path-moveport
	   (last paths)
	   (car (psk-line-keepend (last lines) p (last picks)))
	   (caddr r)
	   t
	 )
       )
       ;; ������ͷ
       (progn
	 (setq p1  (p-dxf (car lines) '(10 11))
	       p2  (p-dxf (last lines) '(10 11))
	       p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

	       a1  (angle p (car picks))
	       a2  (angle p (cadr picks))

	       d   (psk-path-getportsize (car paths))
	       erf (p-get (car paths) "ERF")
	 )

	 (setq r (psk-create-elbow
		   p
		   a1
		   a2
		   d
		   erf
		   (list '(".CLASS" . "FIT")
			 '(".TYPE" . "ELBOW")
			 (cons "SERV" (p-get (car paths) "SERV"))
			 (cons "ERF" erf)
		   )
		 )
	 )

	 (psk-path-moveport
	   (car paths)
	   (car (psk-line-keepend (car lines) p (car picks)))
	   (cadr r)
	   t
	 )
	 (psk-path-moveport
	   (last paths)
	   (car (psk-line-keepend (last lines) p (last picks)))
	   (caddr r)
	   t
	 )
       )
     )
    )
    ((= 3 (length paths))
     (cond
       ;; ��β��ͨ
       ((and (equal $pi/2
		    (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (cadr lines)))
		    1e-3
	     )
	     (equal $pi/2
		    (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (caddr lines)))
		    1e-3
	     )
	)
	(setq p1  (p-dxf (car lines) '(10 11))
	      p2  (p-dxf (last lines) '(10 11))
	      p	  (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

	      a1  (angle p (car picks))
	      a2  (angle p (cadr picks))
	      a3  (angle p (caddr picks))

	      d1  (psk-path-getportsize (car paths))
	      d2  (psk-path-getportsize (cadr paths))
	      d3  (psk-path-getportsize (caddr paths))
	      erf (p-get (car paths) "ERF")
	)
	(setq r	(psk-create-tee
		  p
		  a1
		  a2
		  a3
		  d1
		  d2
		  d3
		  erf
		  (list	'(".CLASS" . "FIT")
			'(".TYPE" . "TEE")
			(cons "SERV" (p-get (car paths) "SERV"))
			(cons "ERF" erf)
		  )
		)
	)
	(psk-path-moveport
	  (car paths)
	  (car (psk-line-keepend (car lines) p (car picks)))
	  (cadr r)
	  t
	)
	(psk-path-moveport
	  (cadr paths)
	  (car (psk-line-keepend (cadr lines) p (cadr picks)))
	  (caddr r)
	  t
	)
	(psk-path-moveport
	  (caddr paths)
	  (car (psk-line-keepend (caddr lines) p (caddr picks)))
	  (cadddr r)
	  t
	)
       )
       ;; ��֧��ͨ
       ((and (equal 0.
		    (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (cadr lines)))
		    1e-3
	     )
	     (equal $pi/2
		    (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (caddr lines)))
		    1e-3
	     )
	)
	(setq
	  p1  (p-dxf (car lines) '(10 11))
	  p2  (p-dxf (last lines) '(10 11))
	  p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

	  a1  (angle p (car picks))
	  a3  (angle p (caddr picks))

	  erf (p-get (car paths) "ERF")

	  d1  (psk-path-getportsize (car paths))
	  d2  (psk-path-getportsize (cadr paths))
	  d3  (psk-path-getportsize (caddr paths))
	)

	(if (not (equal d1 d2))
	  (progn
	    (setq al (psk-editor-getalignmode))
	    (setq r (psk-create-tee2
		      p
		      a1
		      a3
		      d1
		      d2
		      d3
		      erf
		      al
		      (list '(".CLASS" . "FIT")
			    '(".TYPE" . "TEE-S")
			    (cons "SERV" (p-get (car paths) "SERV"))
			    (cons "ERF" erf)
			    (cons "AL" al)
		      )
		    )
	    )
	    (psk-path-moveport
	      (car paths)
	      (car (psk-line-keepend (car lines) p (car picks)))
	      (cadr r)
	      t
	    )
	    (psk-path-moveport
	      (cadr paths)
	      (car (psk-line-keepend (cadr lines) p (cadr picks)))
	      (caddr r)
	      t
	    )
	    (psk-path-moveport
	      (caddr paths)
	      (car (psk-line-keepend (caddr lines) p (caddr picks)))
	      (cadddr r)
	      t
	    )
	  )
	  (progn
	    (setq r (psk-create-branch
		      p
		      a1
		      a3
		      d1
		      d3
		      erf
		      (list '(".CLASS" . "FIT")
			    '(".TYPE" . "BRANCH")
			    (cons "SERV" (p-get (car paths) "SERV"))
			    (cons "ERF" erf)
		      )
		    )
	    )
	    (psk-path-moveport
	      (car paths)
	      (car (psk-line-keepend (car lines) p (car picks)))
	      p
	      t
	    )
	    (psk-path-moveport
	      (cadr paths)
	      (car (psk-line-keepend (cadr lines) p (cadr picks)))
	      p
	      t
	    )
	    (psk-path-moveport
	      (caddr paths)
	      (car (psk-line-keepend (caddr lines) p (caddr picks)))
	      (cadr r)
	      t
	    )
	  )
	)
       )
     )
    )
    ((= 4 (length paths))
     (if
       (and (equal 0.0
		   (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (cadr lines)))
		   1e-3
	    )
	    (equal $pi/2
		   (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (caddr lines)))
		   1e-3
	    )
	    (equal $pi/2
		   (p-angle-include (p-line-getangle (car lines)) (p-line-getangle (last lines)))
		   1e-3
	    )
       )
	(progn
	  (setq	p1  (p-dxf (car lines) '(10 11))
		p2  (p-dxf (last lines) '(10 11))
		p   (inters (car p1) (cadr p1) (car p2) (cadr p2) nil)

		a1  (angle p (car picks))
		a2  (angle p (cadr picks))
		a3  (angle p (caddr picks))
		a4  (angle p (last picks))

		d1  (psk-path-getportsize (car paths))
		d2  (psk-path-getportsize (cadr paths))
		d3  (psk-path-getportsize (caddr paths))
		d4  (psk-path-getportsize (last paths))

		erf (p-get (car paths) "ERF")
	  )

	  (setq	r (psk-create-cross
		    p
		    a1
		    a2
		    a3
		    a4
		    d1
		    d2
		    d3
		    d4
		    erf
		    (list '(".CLASS" . "FIT")
			  '(".TYPE" . "CROSS")
			  (cons "SERV" (p-get (car paths) "SERV"))
			  (cons "ERF" erf)
		    )
		  )
	  )

	  (psk-path-moveport
	    (car paths)
	    (car (psk-line-keepend (car lines) p (car picks)))
	    (cadr r)
	    t
	  )
	  (psk-path-moveport
	    (cadr paths)
	    (car (psk-line-keepend (cadr lines) p (cadr picks)))
	    (caddr r)
	    t
	  )
	  (psk-path-moveport
	    (caddr paths)
	    (car (psk-line-keepend (caddr lines) p (caddr picks)))
	    (cadddr r)
	    t
	  )
	  (psk-path-moveport
	    (last paths)
	    (car (psk-line-keepend (last lines) p (last picks)))
	    ;; TODO:
	    (last r)
	    t
	  )
	)
     )
    )
  )
  (if (and draw r)
    (progn
      ;; �������ɵĹܼ�
      (psk-comp-redraw (psk-comp-load (car r)))
      ;; �������ɹܼ�������·��
      (foreach path paths
	(psk-comp-redraw path)
      )
    )
  )
)
;;


;; (psk-line-break (car (entsel)) (getpoint) (getpoint))
;; ����������¶��� ���ظö��� ���򷵻�nil
(defun psk-line-break (line p1 p2 / a1 a2 a3 dxf en p0 p3 pts)
  (setq	dxf (entget line '("*"))
	p0  (p-get dxf 10)
	p1  (p-line-closestpoint line p1 t)
	p2  (p-line-closestpoint line p2 t)
	p3  (p-get dxf 11)

	a1  (angle p0 p1)
	a2  (angle p0 p2)
	a3  (angle p0 p3)
	;; (0.707 10.25 (200 200 0) 10)
	pts (list (list a1 (distance p0 p1) p1)
		  (list a2 (distance p0 p2) p2)
		  (list a3 (distance p0 p3) p3 11)
	    )
	;; ����������
	pts (vl-sort pts '(lambda (e1 e2) (< (cadr e1) (cadr e2))))
	;; ����������
	pts (vl-sort pts
		     '(lambda (e1 e2)
			(and (not (equal (car e1) (car e2) 1e-6))
			     (< (car e1) (car e2))
			)
		      )
	    )
  )

  ;; ��ϵ���ֱ�߶˵����5�ֹ�ϵ ����3�ִ�ϵ�ֲ������ͬһ��
  (if (and
	;; ���ݽǶ��ж����е�ֲ������ͬһ��
	(equal (caar pts) (caadr pts) 1e-6)
	(equal (caar pts) (caaddr pts) 1e-6)
      )
    (if	(= 11 (last (last pts)))
      ;; ���1/3 ��ϵ������ֱ����
      (progn
	(p-entmod line (cons 11 (caddr (car pts))))
	(setq dxf (p-unset dxf '(-1 5)))
	(entmake (p-set dxf (cons 10 (caddr (cadr pts)))))
	(setq en (entlast))
      )
      (if (/= 11 (last (car pts)))
	;; ���2/3 ��һ����ϵ�����ֱ���ڣ�����ֱ�ߣ��������ڶ���ֱ��
	(p-entmod line (cons 11 (caddr (car pts))))
      )
      ;; ���3/3 ��ϵ������ֱ���ڣ�������
    )
    ;; ���1/2 ����ϵ����������
    (if	(equal a1 a3 1e-6)
      (p-entmod line (cons 10 p1))
      (if (equal a2 a3 1e-6)
	(p-entmod line (cons 10 p2))
      )
    )
    ;; ���2/2 ����ϵ�������⣨������
  )
  en
)


;;;(psk-line-breakat (car (entsel)) (getpoint))
;; ����������¶��� ���ظö��� ���򷵻�nil
(defun psk-line-breakat	(line p2 / a1 a2 dxf en p0 p1)
  (setq	dxf (entget line '("*"))
	p0  (p-get dxf 10)
	p1  (p-get dxf 11)
	p2  (p-line-closestpoint line p2 t)

	a1  (angle p0 p1)
	a2  (angle p0 p2)
  )

  ;; ��ϵ���ֱ�߶˵����3�ֹ�ϵ
  ;; ���账�����ֱ����һ�����
  (if (and (equal a1 a2 1e-6)
	   (< (distance p0 p2) (distance p0 p1))
      )
    (progn
      (p-entmod line (cons 11 p2))
      (setq dxf (p-unset dxf '(-1 5)))
      (entmake (p-set dxf (cons 10 p2)))
      (setq en (entlast))
    )
  )
  en
)
;; ����ֱ�ߺϲ� ����鹲��
;;;(p-line-join (car (entsel)) (car (entsel)))
(defun p-line-join (line1 line2 / dist p1 p2 p3 p4)
  (if (not (equal line1 line2))
    (progn
      (mapcar (function set) '(p1 p2) (p-dxf line1 '(10 11)))
      (mapcar (function set) '(p3 p4) (p-dxf line2 '(10 11)))

      ;; ��ϲ���ֱ�ߵ����˵㣨ֱ�����
      ;; ת�����㵽 p1Ϊԭ�� p1->p2Ϊx�������ϵͳ�Ϸ���Ƚ�
      (setq dist (mapcar '- p2 p1)
	    dist (list (list (last (trans p1 0 dist)) p1)
		       (list (last (trans p2 0 dist)) p2)
		       (list (last (trans p3 0 dist)) p3)
		       (list (last (trans p4 0 dist)) p4)
		 )
	    dist (vl-sort dist
			  (function (lambda (e1 e2)
				      (< (car e1) (car e2))
				    )
			  )
		 )
	    dist (mapcar 'cadr dist)
      )
      (p-entmod line1 (list (cons 10 (car dist)) (cons 11 (last dist))))
      (entdel line2)
    )
  )
)
;; (psk-path-createattach (caar (psk-paths-pick 1)) (getpoint "ָ�������") '(("NAME" . "DS") ("A" . 500) ("B" . 250)))
(defun psk-path-createattach (path p prop / a en line)
  (setq	line (psk-comp-getename path)
	p    (p-line-closestpoint line p t)
	a    (p-line-getangle line)
	en   (psk-key-createblock p a prop "ATTACH" nil)
  )

  (psk-line-breakat line p)
)
;;

(defun psk-path-createinline (path p a prop / a en len line p2)
  (setq	line (psk-comp-getename path)
	en   (psk-key-createblock p a prop "INLINE" nil)
	len  (p-get prop "L")
	p2   (polar p a len)
  )
  (psk-line-break line p p2)
)
;;

(defun psk-path-createend (path p prop / a en line name ports)
  (setq	line  (psk-comp-getename path)
	ports (psk-path-getports path)
	ports (psk-ports-sort ports p)
	a     (+ pi (psk-port-getangle (car ports)))
	p     (psk-port-getpos (car ports))
	en    (psk-key-createblock p a prop "END" nil)
  )
)