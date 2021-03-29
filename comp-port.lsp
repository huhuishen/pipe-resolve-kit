
;; PORTS ��ʽ����ÿһ��Ϊ (�ܿ�id, WCSλ��, ����, �����ߴ�)
;; �����ߴ� 
;; ((10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0) (11 (45632.5 13384.1 0.0) (1.0 0.0) 0.0))

(defun psk-port-getid (port)
  (car port)
)

(defun psk-port-getpos (port)
  (cadr port)
)
;; (psk-port-getdir '(10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0))
;; 4.71239
(defun psk-port-getdir (port)
  (caddr port)
)

(defun psk-port-getangle (port)
  (angle '(0. 0.) (psk-port-getdir port))
)

(defun psk-port-getsize (port)
  (cadddr port)
)

;; (psk-ports-sort '((10 (43178.8 13384.1 0.0) (-1.0 1.22465e-016) 0.0) (11 (45632.5 13384.1 0.0) (1.0 0.0) 0.0)) '(45632.5 13384.1 0.0))
;; ((11 3032.96 3824.84 0.0) (10 2707.08 3191.4 0.0))
;; ���ظ�ʽ((����ָ����Ĺܿ�) (ʣ��ܿ�1) (ʣ��ܿ�2) ...)
(defun psk-ports-sort (ports p / e e1 e2)
  (mapcar
    'cdr
    (vl-sort
      (mapcar 'cons
	      (mapcar '(lambda (e) (distance p e))
		      (mapcar 'psk-port-getpos ports)
	      )
	      ports
      )
      '(lambda (e1 e2) (< (car e1) (car e2)))
    )
  )
)

;; ���ܿڳߴ�����Ա���
;; ���η�ܱ���Ϊ����1010�ĵ㣬ʹ��ǰ��λ��ʾ�ܿڵĿ���
;; Բ����һ��1040�ĸ�������ʾ DN��ܾ�
(defun psk-port-packsize (size)
  (if (vl-consp size)
    (cons 1010 size)
    (cons 1040 size)
  )
)