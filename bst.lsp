

;;; ֱ���໥�����γ�֮���ӽṹ����ز��� 2018-4-1

;; �Զ�ά���������ݼ���������֧����ά�㵫���Ե���ά����
;; (p-sort-points '(((0 0) "PT1")((1 1) "PT2")((2 3) "PT3")((10 -2) "PT4")((2 3) "PT5")) 1.)
;; (((0 0) "PT1") ((1 1) "PT2") ((2 3) "PT5" "PT3") ((10 -2) "PT4"))
(defun p-sort-points (ps tolerance / e lst p r)
  (setq
    ;; ��Y����
    ps (vl-sort ps '(lambda (e1 e2) (< (cadar e1) (cadar e2))))

    ;; ��X����
    ps (vl-sort	ps
		'(lambda (e1 e2)
		   (and	(not (equal (caar e1) (caar e2) tolerance))
			(< (caar e1) (caar e2))
		   )
		 )
       )
  )
  ;;  �ϲ���ͬ������������
  ;;  eg. ((34567.0 181785.0 0.0) <ͼԪ��: -21bd38> <ͼԪ��: -21bd40>)
  (while ps
    (setq e (car ps)
	  p (car e)
    )
    (while (equal (car e) p tolerance)
      (setq lst	(cons (cadr e) lst) ;_ ����ͬ��ϲ�
	    ps	(cdr ps)
	    e	(car ps)
      )
    )
    (setq r   (cons (cons p lst) r)
	  lst nil
    )
  )
  (reverse r)
)
;;

;; �����������һ���б��ƽ�������(BST)������Ԫ�ؼ����ٶ�
;; (p-bst-build '(1 2 3 4 5 6 7))
;; (4 (2 (1) (3)) (6 (5) (7)))
(defun p-bst-build (lst / i left n)
  (if (> (length lst) 1)
    (progn
      (setq n (/ (length lst) 2)
	    i 0
      )
      (while (< i n)
	(setq left (cons (car lst) left)
	      lst  (cdr lst)
	      i	   (1+ i)
	)
      )
      (list (car lst)
	    (p-bst-build (reverse left))
	    (p-bst-build (cdr lst))
      )
    )
    lst
  )
)
;;

;; �������ά���������ݼ����ж��ֲ��ң�֧���ݲ�
;;;_$ (p-bst-find '(2 2) (p-bst-build '(((0 0) "PT1")((1 1) "PT2")((2 3) "PT3")((10 -2) "PT4"))) 1)
;;;nil
;;;_$ (p-bst-find '(2 2) (p-bst-build '(((0 0) "PT1")((1 1) "PT2")((2 3) "PT3")((10 -2) "PT4"))) 2)
;;;("PT3")
(defun p-bst-find (point bst tolerance /)
  (if (null bst)
    nil
    (if	(equal point (caar bst) tolerance)
      (cdar bst)
      (if (or
	    (and (not (equal (car point) (caaar bst) tolerance))
		 (< (car point) (caaar bst))
	    )
	    (and (equal (car point) (caaar bst) tolerance)
		 (< (cadr point) (cadaar bst))
	    )
	  )
	(p-bst-find point (cadr bst) tolerance)
	(p-bst-find point (caddr bst) tolerance)
      )
    )
  )
)

;; Ϊֱ��ʵ�幹����ά������
;; (p-bst-buildforlines (ssget) 0.1)
;; 
;;;(((7035.45 7376.95 0.0) <ͼԪ��: -2569d0>)
;;;  (((5249.75 6739.56 0.0) <ͼԪ��: -2569e8> <ͼԪ��: -2569e0>)
;;;    (((2749.76 6739.56 0.0) <ͼԪ��: -2569e8>))
;;;    (((5249.75 8413.79 0.0) <ͼԪ��: -2569e0> <ͼԪ��: -2569d8>)
;;;    )
;;;  )
;;;  (((7724.22 8413.79 0.0) <ͼԪ��: -2569c8>)
;;;    (((7035.45 8413.79 0.0) <ͼԪ��:	-2569d8> <ͼԪ��: -2569d0> <ͼԪ��: -2569c8>)
;;;    )
;;;    nil
;;;  )
;;;)
(defun p-bst-buildforlines (ss tolerance / ad dxf en n)
  (if ss
    (progn
      (repeat (setq n (sslength ss))
	(setq en  (ssname ss (setq n (1- n)))
	      dxf (entget en)
	)
	(setq ad (cons (list (cdr (assoc 10 dxf)) en) ad)
	      ad (cons (list (cdr (assoc 11 dxf)) en) ad)
	)
      )
      (p-bst-build (p-sort-points ad tolerance))
    )
  )
)

;; (p-bst-find (getpoint) (p-bst-buildforlines (ssget "X") 0.1) 0.1)
;; (p-bst-find (getpoint) (p-bst-buildforlines (ssget "X" '((8 . "1"))) 0.1) 0.1)

