;; -*- Mode: LISP; Package: MICRO-ASSEMBLER -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(in-package "MICRO-ASSEMBLER")

;(declare (cond ((status feature lispm))   ;do nothing on lisp machine.
;	       ((null (memq 'newio (status features)))
;		(break 'you-have-to-compile-this-with-qcompl t))
;	       ((null (get 'if-for-maclisp 'macro))
;		(load '(macros > dsk lispm))
;		(load '(defmac fasl dsk lispm2))
;		(load '(lmmac > dsk lispm2))
;		(macros t))))	;send over the rest of the macros in this file

(declaim (special special-out-file))

(declaim (special racmo radmo raamo 
	   area-list rm-area-sizes conslp-input conslp-output))

;(if-for-maclisp
; (defmacro ldb (ptr val) `(logldb ,ptr ,val)))
;(if-for-maclisp
; (defmacro dpb (newval ptr val) `(logdpb ,newval ,ptr ,val)))

(defun dump-mem-array (arrayp ra-org out-file)
  (prog (idx lim tem)
	(setq idx 0)
	(setq lim (length arrayp))
  l	(cond ((not (< idx lim))
		(return t))
	      ((setq tem (aref arrayp idx))
		(prin1 (+ ra-org idx) out-file)
		(princ '/  out-file)
		(prin-16 tem out-file)
		(terpri out-file)))
	(setq idx (1+ idx))
	(go l)))

(defun cons-dump-array (arrayp out-file)
  (prog (idx lim)
	(setq idx 0)
	(setq lim (length arrayp))
  l	(cond ((not (< idx lim))
		(terpri out-file)
		(return t)))
	(print (aref arrayp idx) out-file)
	(setq idx (1+ idx))
	(go l)))

(defun prin-16 (num out-file)
       (cond ((minusp num) (setq num (+ num #o40000000000))))
		;turn it into a 32 bit pos number
       (prin1 (ldb (byte 16 32) num) out-file)
       (princ '/  out-file)
       (prin1 (ldb (byte 16 16) num) out-file)
       (princ '/  out-file)
       (prin1 (ldb (byte 16 0) num) out-file)
       (princ '/  out-file))

(defun cons-dump-memories ()
  (with-open-file (out-file (format t "~A.uload" conslp-output)
			    :direction :output)
	(let ((*package* (find-package "MICRO-ASSEMBLER"))) ;reduce incidence of
	  ; :'s in output, cause mapatoms to win more.
	  (cond ((null (boundp 'racmo))
		 (load "cadreg")))
	  (dump-mem-array i-mem racmo out-file)
	  (dump-mem-array d-mem radmo out-file)
	  (dump-mem-array a-mem raamo out-file)
	  (terpri out-file)
	  (cond ((not (null (aref (micro-code-symbol-image 0)))) ;if have wiped symbol vector
		 (print -3 out-file)		;dump micro-code-symbol area
		 (print (cons-dump-find-area-origin 'micro-code-symbol-area) out-file)
		 (cons-dump-array micro-code-symbol-image out-file)))
	  (print -2 out-file)		;now dump symbols
	  (terpri out-file)
	  (cons-dump-symbols out-file)
	  (print -1 out-file))))

(defun cons-dump-find-area-origin (area)
  (prog (adr lst tem)
	(setq adr 0)
	(setq lst area-list)
   l	(cond ((null lst) (break 'cant-find-area-origin t))
	      ((eq (car lst) area) (return adr))
	      (t (or (setq tem (list-assq (car lst) rm-area-sizes))
				      (setq tem 1))))
	(setq adr (+ adr (* tem page-size)))
	(setq lst (cdr lst))
	(go l)))

(defun cons-dump-symbols (special-out-file)
  (do-symbols (x (find-package :micro-assembler))
	      (cons-lap-dump-symtab-element x)))

(defun cons-lap-dump-symtab-element (sym)
  (prog (val dmp-type tem)
	(setq val (get sym 'cons-lap-user-symbol))
    l	(cond ((null val) (return nil))
	      ((numberp val)
		(setq dmp-type 'number))
	      ((atom val)
		(setq val (cons-lap-symeval val))
		(go l))
             ((and (setq tem (assoc (car val)
			'( (i-mem jump-address-multiplier)
                           (d-mem dispatch-address-multiplier)
                           (a-mem a-source-multiplier)
                           (m-mem m-source-multiplier))
			:test #'eq))
                   (eq (caadr val) 'field)
                   (eq (cadadr val) (cadr tem)))
              (setq dmp-type (car val) val (caddr (cadr val))))
	     (t (return nil)))
	(prin1 sym special-out-file)
	(princ '/  special-out-file)
        (prin1 dmp-type special-out-file)
        (princ '/  special-out-file)
	(prin1 val special-out-file)
	(princ '/  special-out-file)
	(terpri special-out-file)
	(return t)))

