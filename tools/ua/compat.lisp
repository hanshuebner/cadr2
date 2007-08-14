;;;-*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(in-package "UA")

;(defun quoted-args fexpr (l)
;  (mapcar (function (lambda (x) 
;		      (putprop x '((1005 (fef-arg-opt fef-qt-qt))) 'argdesc))) l))

;(defun closed fexpr (ignore) nil)
;(defun notype fexpr (ignore) nil)
;(defun fixnum fexpr (ignore) nil)
;(defun array* fexpr (ignore) nil)

;(defun genprefix fexpr (ignore) nil)
;(defun expr-hash fexpr (ignore) nil)

;(if-in-maclisp
;(add-optimizer catch catch-macro)
;(defun catch-macro (x)
;  ((lambda (exp tag)
;    `(*catch ',tag ,exp))
;   (cadr x)
;   (caddr x)))

;(add-optimizer throw throw-macro)
;(defun throw-macro (x)
;	((lambda (exp tag)
;	  `(*throw ',tag ,exp))
;	(cadr x)
;	(caddr x))))

(defun list-assq (item in-list)
  (prog nil 
    l	(cond ((null in-list) (return nil))
	      ((eq item (car in-list))
		(return (cadr in-list))))
	(setq in-list (cddr in-list))
	(go l)))

;(defmacro assq (item in-list)
;  `(assoc ,item ,in-list :test #'eq))

(defmacro defprop (item val key)
  `(setf (get ',item ',key) ',val))

(declaim (special **indicator**))
(defun allremprop (**indicator**)
  (do-symbols (x (find-package :micro-assembler))
	      (remprop x **indicator**)))

;(defun include (&quote &rest ignore))

;(defmacro arraycall (array-type array &rest args)
;  `(funcall ,array . ,args))

;(defun sleep (x)
;  (process-sleep (fix (* x 60.))))

(defun assign-alternate (x)
  (do ((l x (cddr l)))
      ((null l))
      (set (car l) (cadr l))))

(defun get-alternate (x)
   (prog (y)
    l	(cond ((null x)
	       (return (reverse y))))
	(setq y (cons (car x) y))
	(setq x (cddr x))
	(go l)))

(defun assign-values (input-list shift)
   (do ((tem 0 (1+ tem)))
       ((null input-list)
	(return tem))
       (set (car input-list) (ash tem shift))
       (setq input-list (cdr input-list))))

(defun assign-values-init-delta (input-list shift init delta)
    (prog nil 
lp	(cond ((null input-list) (return init)))
	(set (car input-list) (ash init shift))
	(setq input-list (cdr input-list))
	(setq init (+ init delta))
	(go lp)))

(defun get-from-alternating-list (l key) 
"Retreive associated item from an alternating list
Like GET, but no initial CAR"
  (prog nil
     l	(cond ((null l) (return nil))
              ((eq key (car l))
               (return (cadr l))))
     (setq l (cddr l))
     (go l)))


