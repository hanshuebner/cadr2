;;;; -*-LISP-*-

;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file contains all the definitions for the machine instruction set
;;; and some other stuff needed by the compiler.

(in-package "MICRO-ASSEMBLER")

;;; This section contains various information regarding the misc. instructions
;;; on the Lisp Machine.  Every entry is of the form:
;;; (DEFMIC <name> <opcode> <arglist> <lisp-function-p> <no-QINTCMP>)
;;;   <name> is the name of the instruction.  If the Lisp function name
;;; 		is different from the instruction name, this is a cons
;;;		of the function name and the instruction name (e.g. (CAR . M-CAR))
;;;   <opcode> is the number which appears in the macro-instruction.
;;;   <arglist> is a list resembling a lambda-list for the Lisp function
;;;             corresponding to the instruction.  & keywords not allowed.
;;;   <lisp-function-p> should be either T or NIL.  If T, then there
;;;             will be a Lisp function defined in the initial Lisp
;;;             environment (available in the interpreter) corresponding
;;;             to the instruction.
;;;   <no-QINTCMP> is OPTIONAL.  If it is not present it is taken to be NIL.
;;;             If it is non-NIL, then no QINTCMP property will be created
;;;             for the symbol.  Otherwise the QINTCMP property is created from
;;;             the length of <arglist>.  The QINTCMP property permits the
;;;		compiler to compile calls to this function as a misc instruction.

;240 241 FREE
(defmic (car . m-car) #o242 (x) t t)
(defmic (cdr . m-cdr) #o243 (x) t t)
(defmic (caar . m-caar) #o244 (x) t t)
(defmic (cadr . m-cadr) #o245 (x) t t)
(defmic (cdar . m-cdar) #o246 (x) t t)
(defmic (cddr . m-cddr) #o247 (x) t t)
(defmic caaar #o250 (x) t)
(defmic caadr #o251 (x) t)
(defmic cadar #o252 (x) t)
(defmic caddr #o253 (x) t)
(defmic cdaar #o254 (x) t)
(defmic cdadr #o255 (x) t)
(defmic cddar #o256 (x) t)
(defmic cdddr #o257 (x) t)
(defmic caaaar #o260 (x) t)
(defmic caaadr #o261 (x) t)
(defmic caadar #o262 (x) t)
(defmic caaddr #o263 (x) t)
(defmic cadaar #o264 (x) t)
(defmic cadadr #o265 (x) t)
(defmic caddar #o266 (x) t)
(defmic cadddr #o267 (x) t)
(defmic cdaaar #o270 (x) t)
(defmic cdaadr #o271 (x) t)
(defmic cdadar #o272 (x) t)
(defmic cdaddr #o273 (x) t)
(defmic cddaar #o274 (x) t)
(defmic cddadr #o275 (x) t)
(defmic cdddar #o276 (x) t)
(defmic cddddr #o277 (x) t)

;300-302 FREE
(defmic %data-type #o303 (x) t)
(defmic %pointer #o304 (x) t)
;305-307 FREE
(defmic %make-pointer #o310 (dtp address) t)
(defmic %spread #o311 (list) nil t)
(defmic %p-store-contents #o312 (pointer x) t)
(defmic %logldb #o313 (ppss word) t)    ;these dont complain about loading/clobbering sign
(defmic %logdpb #o314 (value ppss word) t)  ;result is always a fixnum
(defmic ldb #o315 (ppss word) t)
(defmic dpb #o316 (value ppss word) t)
(defmic %p-store-tag-and-pointer #o317 (pointer misc-fields pointer-field) t)

(defmic get #o320 (symbol propname) t)
(defmic getl #o321 (symbol proplist) t)
(defmic assq #o322 (x alist) t)
(defmic last #o323 (list) t)
(defmic length #o324 (list) t)
(defmic 1+ #o325 (n) t)
(defmic 1- #o326 (n) t)
(defmic rplaca #o327 (cons x) t)
(defmic rplacd #o330 (cons x) t)
(defmic zerop #o331 (number) t)
(defmic set #o332 (symbol x) t)
;333 free
;334 free
;335 free
;(defmic store #o336 )
(defmic xstore #o337 (newdata arrayref) t)

(defmic false #o340 () t)
(defmic true #o341 () t)
(defmic not #o342 (x) t)
(defmic (null . not) #o342 (x) t)
(defmic atom #o343 (x) t)
(defmic oddp #o344 (number) t)
(defmic evenp #o345 (number) t)
(defmic %halt #o346 () t)
(defmic get-pname #o347 (symbol) t)
(defmic lsh #o350 (n nbits) t)
(defmic rot #o351 (n nbits) t)
(defmic *boole #o352 (fn arg1 arg2) t)
(defmic numberp #o353 (x) t)
(defmic plusp #o354 (number) t)
(defmic minusp #o355 (number) t)
(defmic \\ #o356 (x y) t)
(defmic minus #o357 (number) t)
(defmic print-name-cell-location #o360 (symbol) t)
(defmic value-cell-location #o361 (symbol) t)
(defmic function-cell-location #o362 (symbol) t)
(defmic property-cell-location #o363 (symbol) t)
(defmic ncons #o364 (x) t)
(defmic ncons-in-area #o365 (x area) t)
(defmic cons #o366 (x y) t)
(defmic cons-in-area #o367 (x y area) t)
(defmic xcons #o370 (x y) t)
(defmic xcons-in-area #o371 (x y area) t)
;(defmic array-dimension-n #o372 (---) t)
(defmic symeval #o373 (symbol) t)
;(defmic %area-relative-contents #o374 (---) t)
(defmic make-list #o375 (area length) t)
(defmic %call-mult-value #o376 () nil t)
(defmic %call0-mult-value #o377 () nil t)
(defmic %return-2 #o400 () nil t)
(defmic %return-3 #o401 () nil t)
(defmic %return-n #o402 () nil t)
(defmic return-next-value #o403 (x) nil)
(defmic return-list #o404 (values) nil t)
;(defmic apply #o405 )
(defmic bind #o406 (pointer x) nil)
;(defmic get-macro-arg-desc-pointer #o407 )
(defmic memq #o410 (x list) t)
(defmic (< . m-<) #o411 (num1 num2) t)
(defmic (> . m->) #o412 (num1 num2) t)
(defmic (= . m-=) #o413 (num1 num2) t)
(defmic char-equal #o414 (ch1 ch2) t)
(defmic %string-search-char #o415 (char string start end) t)
(defmic %string-equal #o416 (string1 index1 string2 index2 count) t)
(defmic nth #o417 (n list) t)
(defmic nthcdr #o420 (n list) t)
(defmic (*plus . m-+) #o421 (num1 num2) t)
(defmic (*dif . m--) #o422 (num1 num2) t)
(defmic (*times . m-*) #o423 (num1 num2) t)
(defmic (*quo . m-/) #o424 (num1 num2) t)
(defmic (*logand . m-logand) #o425 (num1 num2) t)
(defmic (*logxor . m-logxor) #o426 (num1 num2) t)
(defmic (*logior . m-logior) #o427 (num1 num2) t)
(defmic array-leader #o430 (array index) t)
(defmic store-array-leader #o431 (x array index) t)
(defmic get-list-pointer-into-array #o432 (array) t)
(defmic array-push #o433 (array x) t)
(defmic apply #o434 (fn args) t)
;435-441 free
(defmic %p-flag-bit #o442 (pointer) t)
(defmic %p-cdr-code #o443 (pointer) t)
(defmic %p-data-type #o444 (pointer) t)
(defmic %p-pointer #o445 (pointer) t)
(defmic %page-trace #o446 (table) t)
(defmic %p-store-flag-bit #o447 (pointer flag-bit) t)
(defmic %p-store-cdr-code #o450 (pointer cdr-code) t)
(defmic %p-store-data-type #o451 (pointer data-type) t)
(defmic %p-store-pointer #o452 (pointer pointer) t)
;453-455 free
(defmic %catch-open #o456 () nil t)
(defmic %catch-open-mv #o457 () nil t)
;461, 462 free
(defmic %fexpr-call #o462 () nil t)
(defmic %fexpr-call-mv #o463 () nil t)
(defmic %lexpr-call #o464 () nil t)
(defmic %lexpr-call-mv #o465 () nil t)
(defmic *catch #o466 (tag form) t t)
;467 free
(defmic *throw #o470 (tag value) t)
(defmic %xbus-write-sync #o471 (io-addr word delay sync-loc sync-mask sync-val) t)
(defmic %p-ldb #o472 (ppss pointer) t)
(defmic %p-dpb #o473 (value ppss pointer) t)
(defmic mask-field #o474 (ppss fixnum) t)
(defmic %p-mask-field #o475  (ppss pointer) t)
(defmic deposit-field #o476 (value ppss fixnum) t)
(defmic %p-deposit-field #o477 (value ppss pointer) t)
(defmic copy-array-contents #o500 (from to) t)
(defmic copy-array-contents-and-leader #o501 (from to) t)
(defmic %function-inside-self #o502 () t)
(defmic array-has-leader-p #o503 (array) t)
(defmic copy-array-portion #o504 (from-array from-start from-end to-array to-start to-end) t)
(defmic find-position-in-list #o505 (x list) t)
;(defmic find-position-in-list-equal #o506 )
(defmic g-l-p #o507 (array) t)
(defmic find-position-in-vector #o510 (x list) nil)
;(defmic find-position-in-vector-equal #o511 )
(defmic ar-1 #o512 (array sub) t)
(defmic ar-2 #o513 (array sub1 sub2) t)
(defmic ar-3 #o514 (array sub1 sub2 sub3) t)
(defmic as-1 #o515 (value array sub) t)
(defmic as-2 #o516 (value array sub1 sub2) t)
(defmic as-3 #o517 (value array sub1 sub2 sub3) t)
(defmic %instance-ref #o520 (instance index) t)
(defmic %instance-loc #o521 (instance index) t)
(defmic %instance-set #o522 (val instance index) t)
;523-525 free
(defmic %gc-cons-work #o526 (nqs) t)
(defmic %p-contents-offset #o527 (pointer offset) t)
(defmic %disk-restore #o530 (partition-high-16-bits low-16-bits) t)
(defmic %disk-save #o531 (main-memory-size partition-high-16-bits low-16-bits) t)
(defmic %args-info #o532 (function) t)
(defmic %open-call-block #o533 (function adi-pairs destination) nil)
(defmic %push #o534 (x) nil)
(defmic %activate-open-call-block #o535 () nil)
(defmic %assure-pdl-room #o536 (room) nil)
(defmic stack-group-return #o537 (x) t)
;(defmic %stack-group-return-multi #o540 )
(defmic %make-stack-list #o541 (n) nil)
;542 free
(defmic %call-mult-value-list #o543 () nil t)
(defmic %call0-mult-value-list #o544 () nil t)
(defmic %gc-scav-reset #o545 (region) t)
(defmic %p-store-contents-offset #o546 (x pointer offset) t)
(defmic %gc-free-region #o547 (region) t)
(defmic %gc-flip #o550 (region) t)
(defmic array-length #o551 (array) t)
(defmic array-active-length #o552 (array) t)
(defmic %compute-page-hash #o553 (addr) t)
(defmic get-locative-pointer-into-array #o554 (array-ref) t)
(defmic %unibus-read #o555 (unibus-addr) t)
(defmic %unibus-write #o556 (unibus-addr word) t)
(defmic %gc-scavenge #o557 (work-units) t)
(defmic %chaos-wakeup #o560 () t)
(defmic %area-number #o561 (x) t)
(defmic *max #o562 (num1 num2) t)
(defmic *min #o563 (num1 num2) t)
(defmic closure #o565 (symbol-list function) t)
;(defmic downward-closure #o566 (symbol-list function) t)
(defmic listp #o567 (x) t)
(defmic nlistp #o570 (x) t)
(defmic symbolp #o571 (x) t)
(defmic nsymbolp #o572 (x) t)
(defmic arrayp #o573 (x) t)
(defmic fboundp #o574 (symbol) t)
(defmic stringp #o575 (x) t)
(defmic boundp #o576 (symbol) t)
(defmic \\ #o577 (num1 num2) t)
(defmic fsymeval #o600 (symbol) t)
(defmic ap-1 #o601 (array sub) t)
(defmic ap-2 #o602 (array sub1 sub2) t)
(defmic ap-3 #o603 (array sub1 sub2 sub3) t)
(defmic ap-leader #o604 (array sub) t)
(defmic %p-ldb-offset #o605 (ppss pointer offset) t)
(defmic %p-dpb-offset #o606 (value ppss pointer offset) t)
(defmic %p-mask-field-offset #o607 (ppss pointer offset) t)
(defmic %p-deposit-field-offset #o610 (value ppss pointer offset) t)
(defmic %multiply-fractions #o611 (num1 num2) t)
(defmic %divide-double #o612 (high-dividend low-dividend divisor) t)
(defmic %remainder-double #o613 (high-dividend low-dividend divisor) t)
(defmic haulong #o614 (num) t)
(defmic %allocate-and-initialize #o615 (return-dtp header-dtp header word2 area nqs) t)
(defmic %allocate-and-initialize-array #o616 (header index-length leader-length area nqs) t)
(defmic %make-pointer-offset #o617 (new-dtp pointer offset) t)
(defmic ^ #o620 (num expt) t)
(defmic %change-page-status #o621 (virt-addr swap-status access-and-meta) t)
(defmic %create-physical-page #o622 (phys-addr) t)
(defmic %delete-physical-page #o623 (phys-addr) t)
(defmic %24-bit-plus #o624 (num1 num2) t)
(defmic %24-bit-difference #o625 (num1 num2) t)
(defmic %24-bit-times #o626 (num1 num2) t)
(defmic abs #o627 (num) t)
(defmic %pointer-difference #o630 (ptr1 ptr2) t)
(defmic %p-contents-as-locative #o631 (pointer) t)
(defmic %p-contents-as-locative-offset #o632 (pointer offset) t)
(defmic (eq . m-eq) #o633 (x y) t)
(defmic %store-conditional #o634 (pointer old new) t)
(defmic %stack-frame-pointer #o635 () nil)
(defmic *unwind-stack #o636 (tag value frame-count action) t)
(defmic %xbus-read #o637 (io-addr) t)
(defmic %xbus-write #o640 (io-addr word) t)
(defmic package-cell-location #o641 (symbol) t)
(defmic move-pdl-top #o642 nil nil t)
(defmic shrink-pdl-save-top #o643 (value-to-move n-slots) nil t)
(defmic special-pdl-index #o644 nil nil t)
(defmic unbind-to-index #o645 (special-pdl-index) nil t)
(defmic unbind-to-index-move #o646 (special-pdl-index value-to-move) nil t)
(defmic fix #o647 (number) t)
(defmic float #o650 (number) t)
(defmic small-float #o651 (number) t)
(defmic %float-double #o652 (number number) t)
(defmic bignum-to-array #o653 (bignum base) t)
(defmic array-to-bignum #o654 (array base sign) t)
;655 free
(defmic %write-internal-processor-memories #o656 (code adr d-hi d-low) t)
;657 free
(defmic %region-number #o660 (ptr) t)
(defmic %find-structure-header #o661 (ptr) t)
(defmic %structure-boxed-size #o662 (ptr) t)
(defmic %structure-total-size #o663 (ptr) t)
(defmic %make-region #o664 (bits size) t)
(defmic bitblt #o665 (alu width height from-array from-x from-y to-array to-x to-y) t)
(defmic %disk-op #o666 (rqb) t)
(defmic %physical-address #o667 (ptr) t)
(defmic pop-open-call #o670 nil nil t)
(defmic %beep #o671 (half-wavelength duration) t)
(defmic %find-structure-leader #o672 (ptr) t)
(defmic bpt #o673 nil t)
(defmic %findcore #o674 () t)
(defmic %page-in #o675 (pfn vpn) t)
(defmic ash #o676 (n nbits) t)
;677 free
(defmic %draw-char #o700 (font-array char-code x-bitpos y-bitpos alu-function sheet) t)
(defmic %draw-rectangle #o701 (width height x-bitpos y-bitpos alu-function sheet) t)
(defmic %draw-line #o702 (x0 y0 x y alu draw-end-point sheet) t)
(defmic %draw-triangle #o703 (x1 y1 x2 y2 x3 y3 alu sheet) t)
(defmic %color-transform #o704 (n17 n16 n15 n14 n13 n12 n11 n10 n7 n6 n5 n4 n3 n2 n1 n0
			      width height array start-x start-y) t)

; FROM HERE TO 777 FREE

;;; The ARGDESC properties, telling the compiler special things about
;;; a few functions whose arguments would otherwise be compiled wrong.

;AN ARGDESC PROPERTY IS A LIST OF 2-LISTS.  THE FIRST ELEMENT OF EA
;2-LIST IS A REPEAT COUNT. THE SECOND IS A LIST OF ADL SPECIFIER TYPE TOKENS.

;Argdesc prop's below are for hand coded fctns that cant be called
; with misc instruction (which would be implied by having a QINTCMP property).
; (the reason why a misc call would lose is usually that a new frame
;  must be made).

(defprop *catch ((2 (fef-arg-req fef-qt-eval))) argdesc)

(defprop break ((1 (fef-arg-req fef-qt-qt))
		(1 (fef-arg-opt fef-qt-eval))) argdesc)

;MAKE SURE CALLS TO DEFPROP GET COMPILED RIGHT (IE SPREAD ARGS).  OTHERWISE,
; WOULD LOSE BECAUSE ITS A MACLISP FSUBR.

  (defprop defprop ((3 (fef-arg-req fef-qt-qt))) argdesc)
  (defprop fasload ((1 (fef-arg-req fef-qt-eval)) (1 (fef-arg-opt fef-qt-eval))) argdesc)
	;Likewise FASLOAD which is a SUBR in LISPM since strings self-evaluate.
  (defprop signp ((1 (fef-arg-req fef-qt-qt)) (1 (fef-arg-req fef-qt-eval))) argdesc)
      ;This is here because the compiler loses on QUOTE-HAIR functions.
  (defprop status ((1 (fef-arg-req fef-qt-qt))
                   (1 (fef-arg-opt fef-qt-qt))) argdesc)
  (defprop sstatus ((2 (fef-arg-req fef-qt-qt))) argdesc)

;MAKE SURE FUNCTIONAL ARGS TO MAPPING FUNCTIONS GET BROKEN OFF AND COMPILED
; EVEN IF QUOTE USED INSTEAD OF FUNCTION.  (HOWEVER, A POINTER TO THE 
;  BROKEN-OFF SYMBOL INSTEAD OF THE CONTENTS OF ITS FUNCTION CELL WILL BE PASSED
;  IF QUOTE IS USED).

  (defprop map    ((1 (fef-arg-req fef-qt-eval fef-functional-arg))
		   (1 (fef-arg-req fef-qt-eval))
		   (#o105 (fef-arg-opt fef-qt-eval)) ) argdesc)
  (defprop mapc   ((1 (fef-arg-req fef-qt-eval fef-functional-arg))
		   (1 (fef-arg-req fef-qt-eval))
		   (#o105 (fef-arg-opt fef-qt-eval)) ) argdesc)
  (defprop mapcar ((1 (fef-arg-req fef-qt-eval fef-functional-arg))
		   (1 (fef-arg-req fef-qt-eval))
		   (#o105 (fef-arg-opt fef-qt-eval)) ) argdesc)
  (defprop maplist ((1 (fef-arg-req fef-qt-eval fef-functional-arg))
		    (1 (fef-arg-req fef-qt-eval))
		    (#o105 (fef-arg-opt fef-qt-eval)) ) argdesc)
  (defprop mapcan ((1 (fef-arg-req fef-qt-eval fef-functional-arg))
		   (1 (fef-arg-req fef-qt-eval))
		   (#o105 (fef-arg-opt fef-qt-eval)) ) argdesc)
  (defprop mapcon ((1 (fef-arg-req fef-qt-eval fef-functional-arg))
		   (1 (fef-arg-req fef-qt-eval))
		   (#o105 (fef-arg-opt fef-qt-eval)) ) argdesc)
  (defprop apply ((2 (fef-arg-req fef-qt-eval))) argdesc)
	;Because LSUBR in Maclisp?

;;; Instructions and other symbols for LAP

(defprop call 0 qlval) 

(defprop call0 #o1000 qlval) 

(defprop move #o2000 qlval) 

(defprop car #o3000 qlval) 

(defprop cdr #o4000 qlval) 

(defprop cadr #o5000 qlval) 

(defprop cddr #o6000 qlval) 

(defprop cdar #o7000 qlval) 

(defprop caar #o10000 qlval) 

;ND1
;(DEFPROP UNUSED #o11000 QLVAL) ;NOT USED
(defprop *plus #o31000 qlval)  ;These used to be called +, -, etc. but those are now N-ARG
(defprop *dif #o51000 qlval)   ;While these seven are TWO-ARGUMENTS-ONLY (instructions).
(defprop *times #o71000 qlval) 
(defprop *quo #o111000 qlval) 
(defprop *logand #o131000 qlval)
(defprop *logxor #o151000 qlval)
(defprop *logior #o171000 qlval)

;ND2
(defprop = #o12000 qlval) 
(defprop > #o32000 qlval) 
(defprop < #o52000 qlval) 
(defprop eq #o72000 qlval)
;;; SETE CDR #o112000
;;; SETE CDDR #o132000
;;; SETE 1+ #o152000
;;; SETE 1- #o172000

;ND3
;;; 13000 unused, used to be BIND.
(defprop bindnil #o33000 qlval) 
(defprop bindpop #o53000 qlval) 
(defprop setnil #o73000 qlval) 
(defprop setzero #o113000 qlval) 
(defprop push-e #o133000 qlval)
(defprop movem #o153000 qlval) 
(defprop pop #o173000 qlval)

;;; 14 BRANCH
(defprop misc #o15000 qlval)

;;; - MISCELLANEOUS FUNCTIONS -
(defprop list 0 qlval)
(defprop list-in-area #o100 qlval)
(defprop unbind #o200 qlval)
 (defmic unbind-0 #o200 nil nil t)	;for ucons
 (defmic unbind-1 #o201 nil nil t)	;for ucons
 (defmic unbind-2 #o202 nil nil t)	;for ucons
 (defmic unbind-3 #o203 nil nil t)	;for ucons
 (defmic unbind-4 #o204 nil nil t)	;for ucons
 (defmic unbind-5 #o205 nil nil t)	;for ucons
 (defmic unbind-6 #o206 nil nil t)	;for ucons
 (defmic unbind-7 #o207 nil nil t)	;for ucons
 (defmic unbind-10 #o210 nil nil t)	;for ucons
 (defmic unbind-11 #o211 nil nil t)	;for ucons
 (defmic unbind-12 #o212 nil nil t)	;for ucons
 (defmic unbind-13 #o213 nil nil t)	;for ucons
 (defmic unbind-14 #o214 nil nil t)	;for ucons
 (defmic unbind-15 #o215 nil nil t)	;for ucons
 (defmic unbind-16 #o216 nil nil t)	;for ucons
 (defmic unbind-17 #o217 nil nil t)	;for ucons
(defprop poppdl #o220 qlval)
 (defmic poppdl-0 #o220 nil nil t)	;for ucons
 (defmic poppdl-1 #o221 nil nil t)	;for ucons
 (defmic poppdl-2 #o222 nil nil t)	;for ucons
 (defmic poppdl-3 #o223 nil nil t)	;for ucons
 (defmic poppdl-4 #o224 nil nil t)	;for ucons
 (defmic poppdl-5 #o225 nil nil t)	;for ucons
 (defmic poppdl-6 #o226 nil nil t)	;for ucons
 (defmic poppdl-7 #o227 nil nil t)	;for ucons
 (defmic poppdl-10 #o230 nil nil t)	;for ucons
 (defmic poppdl-11 #o231 nil nil t)	;for ucons
 (defmic poppdl-12 #o232 nil nil t)	;for ucons
 (defmic poppdl-13 #o233 nil nil t)	;for ucons
 (defmic poppdl-14 #o234 nil nil t)	;for ucons
 (defmic poppdl-15 #o235 nil nil t)	;for ucons
 (defmic poppdl-16 #o236 nil nil t)	;for ucons
 (defmic poppdl-17 #o237 nil nil t)	;for ucons
;The rest of these come from the DEFMIC table above.

;"BASE REGISTERS"
(defprop fef 0 qlval)

(defprop const-page #o400 qlval)

(defprop locblock #o500 qlval)

(defprop arg #o600 qlval)

(defprop lpdl #o700 qlval)

;Destinations
(defprop d-ignore 0 qlval)

(defprop d-inds 0 qlval)

(defprop d-pdl #o20000 qlval)

(defprop d-next #o40000 qlval)

(defprop d-last #o60000 qlval)

(defprop d-return #o100000 qlval)

;(defprop dest-arg-qtd #o60000 qlval) 		;added to d-next,d-last

(defprop d-next-list #o160000 qlval)

;;; Properties for the micro-compiler

(defprop m-car qma last-arg-in-t-entry)
(defprop m-cdr qmd last-arg-in-t-entry)
(defprop m-caar qmaa last-arg-in-t-entry)
(defprop m-cadr qmad last-arg-in-t-entry)
(defprop m-cdar qmda last-arg-in-t-entry)
(defprop m-cddr qmdd last-arg-in-t-entry)
(defprop caaar qmaaa last-arg-in-t-entry)
(defprop caadr qmaad last-arg-in-t-entry)
(defprop cadar qmada last-arg-in-t-entry)
(defprop caddr qmadd last-arg-in-t-entry)
(defprop cdaar qmdaa last-arg-in-t-entry)
(defprop cdadr qmdad last-arg-in-t-entry)
(defprop cddar qmdda last-arg-in-t-entry)
(defprop cdddr qmddd last-arg-in-t-entry)
(defprop caaaar qmaaaa last-arg-in-t-entry)
(defprop caaadr qmaaad last-arg-in-t-entry)
(defprop caadar qmaada last-arg-in-t-entry)
(defprop caaddr qmaadd last-arg-in-t-entry)
(defprop cadaar qmadaa last-arg-in-t-entry)
(defprop cadadr qmadad last-arg-in-t-entry)
(defprop caddar qmadda last-arg-in-t-entry)
(defprop cadddr qmaddd last-arg-in-t-entry)
(defprop cdaaar qmdaaa last-arg-in-t-entry)
(defprop cdaadr qmdaad last-arg-in-t-entry)
(defprop cdadar qmdada last-arg-in-t-entry)
(defprop cdaddr qmdadd last-arg-in-t-entry)
(defprop cddaar qmddaa last-arg-in-t-entry)
(defprop cddadr qmddad last-arg-in-t-entry)
(defprop cdddar qmddda last-arg-in-t-entry)
(defprop cddddr qmdddd last-arg-in-t-entry)

(defprop m-+ xtcadd last-arg-in-t-entry)	;checks input d.t. to assure fixed
(defprop m-- xtcsub last-arg-in-t-entry)
(defprop m-* xtcmul last-arg-in-t-entry)
(defprop m-/ xtcdiv last-arg-in-t-entry)
(defprop m-logand xtcand last-arg-in-t-entry)
(defprop m-logxor xtcxor last-arg-in-t-entry)
(defprop m-logior xtcior last-arg-in-t-entry)

;(defprop xtcadd xtadd no-type-checking-entry)	;one arg in t, one on pdl
;(defprop xtcsub xtsub no-type-checking-entry)
;(defprop xtcmul xtmul no-type-checking-entry)
;(defprop xtcdiv xtdiv no-type-checking-entry)
;(defprop xtcand xtand no-type-checking-entry)
;(defprop xtcxor xtxor no-type-checking-entry)
;(defprop xtcior xtior no-type-checking-entry)

;(defprop m-+ xtadd unboxed-num-in-t-entry)	;these guys dont really check anyway
;(defprop m-- xtsub unboxed-num-in-t-entry)
;(defprop m-* xtmul unboxed-num-in-t-entry)
;(defprop m-/ xtdiv unboxed-num-in-t-entry)
;(defprop m-logand xtand unboxed-num-in-t-entry)
;(defprop m-logxor xtxor unboxed-num-in-t-entry)
;(defprop m-logior xtior unboxed-num-in-t-entry)

;(defprop m-+ xmadd no-type-checking-entry)	;these are a bit faster
;(defprop m-- xmsub no-type-checking-entry)	;take 2 args on pdl
;(defprop m-* xmmul no-type-checking-entry)
;(defprop m-/ xmdiv no-type-checking-entry)
;(defprop m-logand xmand no-type-checking-entry)
;(defprop m-logxor xmxor no-type-checking-entry)
;(defprop m-logior xmior no-type-checking-entry)

;(defprop atom xtatom last-arg-in-t-entry)
;(defprop zerop xtzero last-arg-in-t-entry)
(defprop numberp xtnumb last-arg-in-t-entry)
;(defprop plusp xtplup last-arg-in-t-entry)
;(defprop minusp xtmnsp last-arg-in-t-entry)
;(defprop minus xtmns last-arg-in-t-entry)
;(defprop 1+ xt1pls last-arg-in-t-entry)
;(defprop 1- xt1mns last-arg-in-t-entry)
;(defprop symeval xtsyme last-arg-in-t-entry)
(defprop length xtleng last-arg-in-t-entry)

;(defprop zerop xbzero unboxed-num-in-t-entry)
;(defprop plusp xbplup unboxed-num-in-t-entry)
;(defprop minusp xbmnsp unboxed-num-in-t-entry)
;(defprop minus xbmns unboxed-num-in-t-entry)
;(defprop 1+ xb1pls unboxed-num-in-t-entry)
;(defprop 1- xb1mns unboxed-num-in-t-entry)

;;; Certain MISC-instructions make assumptions about what destinations
;;; they are used with.  Some require D-IGNORE, because they assume that
;;; there is no return address on the micro-stack.  Some do not allow D-IGNORE,
;;; because they popj and start a memory cycle.  Some are really random.
(defvar misc-instruction-required-destination-alist
	'( (%allocate-and-initialize d-pdl d-next d-last d-return d-next-list)
	   (%allocate-and-initialize-array d-pdl d-next d-last d-return d-next-list)
	   (%spread d-next d-last)
	   (return-list d-return)
	   (%open-call-block d-ignore d-inds)
	   (%activate-open-call-block d-ignore d-inds)
	   (%return-2 d-ignore d-inds)
	   (%return-3 d-ignore d-inds)
	   (%return-n d-ignore d-inds)
	   (%return-next-value d-ignore d-inds)))
