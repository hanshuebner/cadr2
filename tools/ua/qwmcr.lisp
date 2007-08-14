;-*- Mode:LISP; Package: MICRO-ASSEMBLER; BASE 8; IBASE 8 -*-
;Write out the output of CONSLP.
;This is the Lisp machine version of WMCR.  It's a different file
;because so much had to be changed.

(in-package "MICRO-ASSEMBLER")

;For now, the reading side is flushed.  It exists elsewhere anyway, doesn't it?

;An MCR file looks a lot like a microcode partition.  Each 36-bit word
;contains one 32-bit word, left-justified.  (Being left justified makes
;it a whole lot easier to gobble the file with the real machine).
;From the Lisp machine, we write this as 2 16-bit pieces

(declaim (special conslp-output-symbol-predicted-filepos
		  conslp-output-current-filepos
		  conslp-output version-number cons-disp-parity-bit))

(declaim (special i-mem-loc d-mem-loc a-mem-loc m-mem-loc 
		  a-constant-loc a-constant-base m-constant-loc m-constant-base 
		  d-mem-free-blocks m-constant-list a-constant-list))

(declaim (special assembler-saved-state))

(defun out16 (file word)
  (setq conslp-output-current-filepos (1+ conslp-output-current-filepos))
  (write-byte (ldb (byte 8 8) word) file)
  (write-byte (ldb (byte 8 0) word) file))

(defun out32 (file word)
  (out16 file (ldb (byte 16 16) word)) ;Note non-standard order of 16-bit bytes
  (out16 file (ldb (byte 16 0) word)))

(defun write-mcr (base-version-number)
  (with-open-file (file (make-pathname :name (string-downcase (symbol-name conslp-output)) :type "mcr")
		    :direction :output
		    :if-exists :supersede
		    :element-type '(unsigned-byte 8))
	  (setq conslp-output-current-filepos 0)
	  (cond (base-version-number
	    (out32 file 3)    ;a fake main memory block
	    (out32 file 0)    ; blocks to xfer
	    (out32 file 0)    ; normally relative disk block, 0 says base version follows
	    (out32 file base-version-number)))
    (write-i-mem i-mem 1 file)
    (write-d-mem d-mem 2 file)
    (write-micro-code-symbol-area-part-1 file)
    (write-a-mem a-mem 4 file)
    (write-micro-code-symbol-area-part-2 file))
  (write-symbol-table conslp-output))

(defun write-d-mem (array code file)
    (out32 file code)		;Code for this kind of section.
    (out32 file 0)		;Start address.
    (let ((size (length array)))
      (out32 file size)
      (do ((i 0 (1+ i)))
	  ((= i size) t)
	(let ((val (or (aref array i) 0)))
	  (out16 file	   ;High bit and parity bit
		 (dpb (do ((count 17 (1- count))
			   (x val (logxor val (ash x -1))))
			  ((= count 0)
			   (logxor 1 x)))	;odd parity
		      (byte 1 1)
		      (ldb (byte 1 2) val)))
	  (out16 file val) ;Low 16 bits
	  ))))

(defun write-a-mem (a-array code file)
    (out32 file code)		;Code for this kind of section.
    (out32 file 0)		;Start address.
    (let ((size (length a-array)))
      (out32 file size)
      (do ((i 0 (1+ i)))
	  ((= i size))
	(out32 file (or (aref a-array i) 0)))))

(defun write-i-mem (array code file)
    (out32 file code)		;Code for this kind of section.
    (out32 file 0)		;Start address.
    (let ((size (length array)) (tem))
      (do () ((not (null (aref array (1- size)))))
	(setq size (1- size)))
      (out32 file size)
      (do ((i 0 (1+ i)))
	  ((= i size) t)
	(setq tem (or (aref array i) 0))
	(out16 file (ldb (byte 16 48) tem))	;A high
	(out16 file (ldb (byte 16 32) tem))	;A low
	(out16 file (ldb (byte 16 16) tem))	;M high
	(out16 file (ldb (byte 16 0) tem))	;M low
	)))

(defun write-micro-code-symbol-area-part-1 (file)
  (out32 file 3)		;Code for main mem section.
  (out32 file (floor (length micro-code-symbol-image) #o400)) ;# of blocks
  (setq conslp-output-symbol-predicted-filepos
	(+ conslp-output-current-filepos
	   4 ;rest of this block
	   6 ;A/M header
	   #o4000 ;A/M data
	   ))
  (out32 file (floor (+ conslp-output-symbol-predicted-filepos #o777) #o1000)) ;Rel disk block #
  (out32 file (cons-dump-find-area-origin 'micro-code-symbol-area))) ;Phys mem address

;Call this after everything else, to put the micro code symbol area at the end
(defun write-micro-code-symbol-area-part-2 (file)
  (or (= conslp-output-current-filepos conslp-output-symbol-predicted-filepos)
      (break "lossage"))
  (do ((n (rem conslp-output-current-filepos #o1000) (1+ n)))
      ((or (zerop n) (= n #o1000)))	;Pad to page boundary
    (out16 file 0))
  (let ((array micro-code-symbol-image))
    (do ((i 0 (1+ i))
	 (n (length array))
	 (fixnum-data-type (dpb dtp-fix %%q-data-type 0)))
	((not (< i n)))
      (out32 file (+ fixnum-data-type (cond ((aref array i)) (t 0)))))))

;This writes an ascii file containing the symbol table
; Warning; this function also exists in LCADR;WMCR
(defun write-symbol-table (filename)
  (with-open-file (out-file (make-pathname :name (string-downcase (symbol-name filename)) :type "sym")
			    :direction :output
			    :if-exists :supersede)
    (print -4 out-file)	;assembler state info
    (print (make-assembler-state-list) out-file)
    (print -2 out-file)
    (cons-dump-symbols out-file)
    (print -1 out-file)))

(defun make-constant-list (lst)   ;flush usage count, last locn ref'ed at.
   (mapcar #'(lambda (x) (list (car x) (cadr x)))
   lst))

;cons-dump-symbols in cdmp
