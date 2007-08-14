;;;; -*- Mode: LISP; Package: SYSTEM-INTERNALS -*-
;;;-- INITIAL SYMS  CADR VERSION
;;; FIELDS

(in-package "MICRO-ASSEMBLER")		; Not SI

(defprop function-source #x4000000 cons-lap-sym)
  (defprop function-source #x80000000 cons-lap-additive-constant)
(defprop byte-size (or (byte-instruction-p 0x20)
		       (dispatch-instruction-p 0x20)) cons-lap-sym)
(defprop byte-rotate 0 cons-lap-sym)
(defprop function-destination #x80000 cons-lap-sym)
(defprop a-source-multiplier #x100000000 cons-lap-sym)
(defprop m-source-multiplier #.(ash 1 26) cons-lap-sym)
(defprop a-destination-multiplier #.(ash 1 14) cons-lap-sym)
(defprop m-destination-multiplier #.(ash 1 14) cons-lap-sym)
(defprop jump-address-multiplier #.(ash 1 12) cons-lap-sym)
(defprop dispatch-address-multiplier #.(ash 1 12) cons-lap-sym)
(defprop alu-op (force-alu #.(ash 1 3)) cons-lap-sym)
(defprop alu-output-bus-selector-multiplier (force-alu #.(ash 1 12)) cons-lap-sym)

(defprop alu-inst (force-alu 0) cons-lap-sym)
(defprop dispatch (force-dispatch 0) cons-lap-sym)
(defprop byte-inst (force-byte 0) cons-lap-sym)
(defprop jump-inst (force-jump 0) cons-lap-sym)

(defprop dispatch-advance-instruction-stream #.(ash 1 24) cons-lap-sym)
(defprop dispatch-push-own-address #.(ash 1 25) cons-lap-sym)

(setq sr-bit #.(ash 1 12))
(defprop sr-bit (force-byte #.(ash 1 12)) cons-lap-sym)
(defprop mr-bit (force-byte #.(ash 1 13)) cons-lap-sym)

(defprop alu-carry-in-zero (force-alu #.(ash 0 2)) cons-lap-sym)
(defprop alu-carry-in-one (force-alu #.(ash 1 2)) cons-lap-sym)

(defprop output-selector-normal 
	(field alu-output-bus-selector-multiplier 1) cons-lap-sym)
(defprop output-selector-rightshift-1 
	(field alu-output-bus-selector-multiplier 2) cons-lap-sym)
(defprop output-selector-leftshift-1 
	(field alu-output-bus-selector-multiplier 3) cons-lap-sym)

(defprop shift-q-left (force-alu #.(ash 1 0)) cons-lap-sym)
(defprop shift-q-right (force-alu #.(ash 2 0)) cons-lap-sym)
(defprop load-q (force-alu #.(ash 3 0)) cons-lap-sym)

(defprop r-bit #.(ash 1 9) cons-lap-sym)
(defprop p-bit #.(ash 1 8) cons-lap-sym)
(defprop inhibit-xct-next-bit #.(ash 1 7) cons-lap-sym)

(defprop invert-jump-sense (force-jump #.(ash 1 6)) cons-lap-sym)

(defprop jump-less-than-condition 41 cons-lap-sym)
(defprop jump-less-or-equal-condition 42 cons-lap-sym)
(defprop jump-equal-condition 43 cons-lap-sym)
(defprop jump-greater-than-condition 
	    (plus invert-jump-sense jump-less-or-equal-condition) cons-lap-sym)
(defprop jump-greater-or-equal-condition 
	    (plus invert-jump-sense jump-less-than-condition) cons-lap-sym)

(defprop jump-on-page-fault-condition 44 cons-lap-sym)
(defprop jump-on-page-fault-or-interrupt-pending-condition 45 cons-lap-sym)
(defprop jump-on-page-fault-or-interrupt-pending-or-sequence-break-condition 46 cons-lap-sym)
(defprop jump-always 47 cons-lap-sym)

(defprop jump-on-bit-condition 0 cons-lap-sym)

;misc function codes

(defprop instruction-stream #.(ash 3 10) cons-lap-sym)
(defprop write-dispatch-ram (force-dispatch #.(ash 2 10)) cons-lap-sym)
(defprop halt-cons #.(ash 1 10) cons-lap-sym)

(defprop i-long #o1000000000000000 cons-lap-sym)

; "AUGMENTED" INSTRUCTIONS
(defprop jump-op-xct-next jump-inst cons-lap-sym)
(defprop jump-op (plus jump-inst inhibit-xct-next-bit) cons-lap-sym)

(defprop jump-xct-next (plus jump-op-xct-next jump-always) cons-lap-sym)
(defprop jump (plus jump-op jump-always) cons-lap-sym)

(defprop call-xct-next (plus jump-xct-next p-bit) cons-lap-sym) 
(defprop call (plus jump p-bit) cons-lap-sym)

(defprop popj-xct-next (plus jump-xct-next r-bit) cons-lap-sym)
(defprop popj (plus jump r-bit) cons-lap-sym)

(defprop jump-if-bit-set (plus jump-op jump-on-bit-condition) cons-lap-sym)
(defprop jump-if-bit-clear (plus jump-if-bit-set invert-jump-sense) cons-lap-sym)
(defprop jump-if-bit-set-xct-next (plus jump-op-xct-next jump-on-bit-condition)
	 cons-lap-sym)
(defprop jump-if-bit-clear-xct-next (plus jump-if-bit-set-xct-next invert-jump-sense)
	 cons-lap-sym)

(defprop call-if-bit-set (plus jump-if-bit-set p-bit) cons-lap-sym)
(defprop call-if-bit-clear (plus jump-if-bit-clear p-bit) cons-lap-sym)
(defprop call-if-bit-set-xct-next (plus jump-if-bit-set-xct-next p-bit) cons-lap-sym)
(defprop call-if-bit-clear-xct-next (plus jump-if-bit-clear-xct-next p-bit)
	 cons-lap-sym)

(defprop popj-if-bit-set (plus jump-if-bit-set r-bit) cons-lap-sym)
(defprop popj-if-bit-clear (plus jump-if-bit-clear r-bit) cons-lap-sym)
(defprop popj-if-bit-set-xct-next (plus jump-if-bit-set-xct-next r-bit) cons-lap-sym)
(defprop popj-if-bit-clear-xct-next (plus jump-if-bit-clear-xct-next r-bit)
	 cons-lap-sym)

(defprop jump-equal (plus jump-op jump-equal-condition) cons-lap-sym)
(defprop jump-not-equal (plus jump-op 
			   (plus jump-equal-condition invert-jump-sense)) cons-lap-sym)
(defprop jump-equal-xct-next (plus jump-op-xct-next jump-equal-condition) cons-lap-sym)
(defprop jump-not-equal-xct-next 
			(plus jump-op-xct-next  
			   (plus jump-equal-condition invert-jump-sense)) cons-lap-sym)

(defprop call-equal (plus jump-equal p-bit) cons-lap-sym)
(defprop call-not-equal (plus jump-not-equal p-bit) cons-lap-sym)
(defprop call-equal-xct-next (plus jump-equal-xct-next p-bit) cons-lap-sym)
(defprop call-not-equal-xct-next (plus jump-not-equal-xct-next p-bit) cons-lap-sym)

(defprop popj-equal (plus jump-equal r-bit) cons-lap-sym)
(defprop popj-not-equal (plus jump-not-equal r-bit) cons-lap-sym)
(defprop popj-equal-xct-next (plus jump-equal-xct-next r-bit) cons-lap-sym)
(defprop popj-not-equal-xct-next (plus jump-not-equal-xct-next r-bit) cons-lap-sym)

(defprop jump-less-than (plus jump-op jump-less-than-condition) cons-lap-sym)
(defprop jump-less-than-xct-next (plus jump-op-xct-next jump-less-than-condition)
	 cons-lap-sym)

(defprop call-less-than (plus jump-less-than p-bit) cons-lap-sym)
(defprop call-less-than-xct-next (plus jump-less-than-xct-next p-bit) cons-lap-sym)

(defprop popj-less-than (plus jump-less-than r-bit) cons-lap-sym)
(defprop popj-less-than-xct-next (plus jump-less-than-xct-next r-bit) cons-lap-sym)

(defprop jump-greater-than (plus jump-op jump-greater-than-condition) cons-lap-sym)
(defprop jump-greater-than-xct-next
	 (plus jump-op-xct-next jump-greater-than-condition) cons-lap-sym)

(defprop call-greater-than (plus jump-greater-than p-bit) cons-lap-sym)
(defprop call-greater-than-xct-next
	 (plus jump-greater-than-xct-next p-bit) cons-lap-sym)

(defprop popj-greater-than (plus jump-greater-than r-bit) cons-lap-sym)
(defprop popj-greater-than-xct-next
	 (plus jump-greater-than-xct-next r-bit) cons-lap-sym)

(defprop jump-greater-or-equal
	 (plus jump-op jump-greater-or-equal-condition) cons-lap-sym)
(defprop jump-greater-or-equal-xct-next
	 (plus jump-op-xct-next jump-greater-or-equal-condition) cons-lap-sym)

(defprop call-greater-or-equal (plus jump-greater-or-equal p-bit) cons-lap-sym)
(defprop call-greater-or-equal-xct-next
	 (plus jump-greater-or-equal-xct-next p-bit) cons-lap-sym)

(defprop popj-greater-or-equal (plus jump-greater-or-equal r-bit) cons-lap-sym)
(defprop popj-greater-or-equal-xct-next
	 (plus jump-greater-or-equal-xct-next r-bit) cons-lap-sym)

(defprop jump-less-or-equal (plus jump-op jump-less-or-equal-condition) cons-lap-sym)
(defprop jump-less-or-equal-xct-next
	 (plus jump-op-xct-next jump-less-or-equal-condition) cons-lap-sym)

(defprop call-less-or-equal (plus jump-less-or-equal p-bit) cons-lap-sym)
(defprop call-less-or-equal-xct-next
	 (plus jump-less-or-equal-xct-next p-bit) cons-lap-sym)

(defprop popj-less-or-equal (plus jump-less-or-equal r-bit) cons-lap-sym)
(defprop popj-less-or-equal-xct-next
	 (plus jump-less-or-equal-xct-next r-bit) cons-lap-sym)


(defprop jump-if-page-fault (plus jump-op jump-on-page-fault-condition) cons-lap-sym)
(defprop jump-if-page-fault-xct-next
	 (plus jump-op-xct-next jump-on-page-fault-condition) cons-lap-sym)

(defprop call-if-page-fault (plus jump-if-page-fault p-bit) cons-lap-sym)
(defprop call-if-page-fault-xct-next
	 (plus jump-if-page-fault-xct-next p-bit) cons-lap-sym)

(defprop popj-if-page-fault (plus jump-if-page-fault r-bit) cons-lap-sym)
(defprop popj-if-page-fault-xct-next
	 (plus jump-if-page-fault-xct-next r-bit) cons-lap-sym)

(defprop jump-if-page-fault-or-interrupt (plus jump-op jump-on-page-fault-or-interrupt-pending-condition) cons-lap-sym)
(defprop jump-if-page-fault-or-interrupt-xct-next
	 (plus jump-op-xct-next jump-on-page-fault-or-interrupt-pending-condition) cons-lap-sym)

(defprop call-if-page-fault-or-interrupt (plus jump-if-page-fault-or-interrupt p-bit) cons-lap-sym)
(defprop call-if-page-fault-or-interrupt-xct-next
	 (plus jump-if-page-fault-or-interrupt-xct-next p-bit) cons-lap-sym)

(defprop popj-if-page-fault-or-interrupt (plus jump-if-page-fault-or-interrupt r-bit) cons-lap-sym)
(defprop popj-if-page-fault-or-interrupt-xct-next
	 (plus jump-if-page-fault-or-interrupt-xct-next r-bit) cons-lap-sym)

;these really check for page fault, interrupt or sequence break,
;but that makes a hell of a name!
(defprop jump-if-sequence-break (plus jump-op jump-on-page-fault-or-interrupt-pending-or-sequence-break-condition) cons-lap-sym)
(defprop jump-if-sequence-break-xct-next
	 (plus jump-op-xct-next jump-on-page-fault-or-interrupt-pending-or-sequence-break-condition) cons-lap-sym)

(defprop call-if-sequence-break (plus jump-if-sequence-break p-bit) cons-lap-sym)
(defprop call-if-sequence-break-xct-next
	 (plus jump-if-sequence-break-xct-next p-bit) cons-lap-sym)

(defprop popj-if-sequence-break (plus jump-if-sequence-break r-bit) cons-lap-sym)
(defprop popj-if-sequence-break-xct-next
	 (plus jump-if-sequence-break-xct-next r-bit) cons-lap-sym)

(defprop write-i-mem (plus jump-op p-bit r-bit jump-always) cons-lap-sym)

(defprop jump-conditional jump-op cons-lap-sym)	;defined for convenience
(defprop jump-conditional-xct-next jump-op-xct-next cons-lap-sym) ;likewise

(defprop call-conditional (plus jump-op p-bit) cons-lap-sym)	;defined for convienence
(defprop call-conditional-xct-next (plus jump-op-xct-next p-bit) cons-lap-sym)

(defprop popj-conditional (plus jump-conditional r-bit) cons-lap-sym)
(defprop popj-conditional-xct-next (plus jump-conditional-xct-next r-bit) cons-lap-sym)


(defprop dispatch-call-xct-next (plus dispatch 0) cons-lap-sym)
				;this sym defined for convience, dispatch table must
				;have the right thing to make this happen.

(defprop dispatch-xct-next (plus dispatch 0) cons-lap-sym)  ;likewise
(defprop dispatch-call (plus dispatch 0) cons-lap-sym)	;likewise
(defprop dispatch-popj-xct-next (plus dispatch 0) cons-lap-sym) ;ditto

(defprop popj-after-next #o100000000000000 cons-lap-sym)

(defprop ldb byte-inst cons-lap-sym)
(defprop dpb (plus byte-inst mr-bit) cons-lap-sym)	;(like pdp10 dpb)sr-bit reversed
(defprop selective-deposit (plus (plus byte-inst	;(like pdp1 dip, dap)
			  mr-bit) sr-bit) cons-lap-sym)	;sr-bit reversed

(defprop no-op alu-inst cons-lap-sym)

(defprop setz  (field alu-op #o00) cons-lap-sym)
(defprop and   (field alu-op #o01) cons-lap-sym)
(defprop andca (field alu-op #o02) cons-lap-sym)
(defprop setm  (field alu-op #o03) cons-lap-sym)
(defprop andcm (field alu-op #o04) cons-lap-sym)
(defprop seta  (field alu-op #o05) cons-lap-sym)
(defprop xor   (field alu-op #o06) cons-lap-sym)
(defprop ior   (field alu-op #o07) cons-lap-sym)
(defprop andcb (field alu-op #o10) cons-lap-sym)
(defprop eqv   (field alu-op #o11) cons-lap-sym)
(defprop setca (field alu-op #o12) cons-lap-sym)
(defprop orca  (field alu-op #o13) cons-lap-sym)
(defprop setcm (field alu-op #o14) cons-lap-sym)
(defprop orcm  (field alu-op #o15) cons-lap-sym)
(defprop orcb  (field alu-op #o16) cons-lap-sym)
(defprop seto  (field alu-op #o17) cons-lap-sym)

(defprop add   (field alu-op #o31) cons-lap-sym)
(defprop sub   (plus (field alu-op #o26) alu-carry-in-one) cons-lap-sym)
(defprop sub-m+1 (field alu-op #o26) cons-lap-sym)
(defprop m+m   (field alu-op #o37) cons-lap-sym)
(defprop m+1   (plus (field alu-op #o34) alu-carry-in-one) cons-lap-sym)
(defprop m-a-1 (field alu-op #o26) cons-lap-sym)
(defprop m+a+1 (plus (field alu-op #o31) alu-carry-in-one) cons-lap-sym)
(defprop m+m+1 (plus (field alu-op #o37) alu-carry-in-one) cons-lap-sym)


(defprop multiply-step (plus (plus (field alu-op #o40) shift-q-right)
			 output-selector-rightshift-1) cons-lap-sym)
(defprop divide-first-step (plus (plus (field alu-op #o51) shift-q-left)
			 output-selector-leftshift-1) cons-lap-sym)
(defprop divide-step (plus (plus (field alu-op #o41) shift-q-left)
			 output-selector-leftshift-1) cons-lap-sym)
(defprop divide-last-step (plus (field alu-op #o41) shift-q-left) cons-lap-sym)
(defprop divide-remainder-correction-step (field alu-op #o45) cons-lap-sym)

; Function sources
(defprop read-i-arg 
		(or (source-p (field function-source 0))
		    (error)) cons-lap-sym)

(defprop micro-stack-pntr-and-data
		(or (source-p (field function-source 1))
		    (error)) cons-lap-sym)

(defprop pdl-buffer-pointer (or (source-p (field function-source 2))
			        (field function-destination #o14))
  cons-lap-sym)

(defprop pdl-buffer-index (or (source-p (field function-source 3))
		 	      (field function-destination #o13)) cons-lap-sym)

(defprop c-pdl-buffer-index 
		(or (source-p (field function-source 5))
		    (field function-destination #o12)) cons-lap-sym)

(defprop c-opc-buffer (or (source-p (field function-source 6))
			  (error)) cons-lap-sym)

(defprop q-r (or (source-p (field function-source 7))
	         (force-alu 3)) cons-lap-sym)

(defprop memory-map-data (or (source-p (field function-source #o11))
			      (error)) cons-lap-sym)

(defprop read-memory-data (or (source-p (field function-source #o12))
			      (error)) cons-lap-sym)

(defprop location-counter (or (source-p (field function-source #o13))
			    (field function-destination 1)) cons-lap-sym)

(defprop micro-stack-pntr-and-data-pop
		(or (source-p (field function-source #o14))
		    (error)) cons-lap-sym)

(defprop micro-stack-pointer (or (source-p (plus (field function-source 1)
						 (byte-field 5 24.)))
				 (error)) cons-lap-sym)

(defprop micro-stack-pointer-pop (or (source-p (plus (field function-source #o14)
						     (byte-field 5 24.)))
				     (error)) cons-lap-sym)

(defprop micro-stack-data (or (source-p (plus (field function-source 1)
					      (byte-field 19. 0)))
			      (error)) cons-lap-sym)

(defprop micro-stack-data-pop
		(or (source-p (plus (field function-source #o14)
				    (byte-field 19. 0)))
		    (error)) cons-lap-sym)

(defprop c-pdl-buffer-pointer 
		(or (source-p (field function-source #o25))
		    (field function-destination 10)) cons-lap-sym)

(defprop c-pdl-buffer-pointer-pop
		(or (source-p (field function-source #o24))
		    (error)) cons-lap-sym)

;Function-destinations

(defprop interrupt-control (or (source-p (error))
			       (field function-destination 2)) cons-lap-sym)

;WRITE-MEMORY-DATA AND MD ARE SYNONYMS.  WRITE-MEMORY-DATA WILL GO
; AWAY EVENTUALLY
(defprop write-memory-data 
		(or (source-p (field function-source #o12))
		    (field function-destination #o30)) cons-lap-sym)

(defprop md  
		(or (source-p (field function-source #o12))
		    (field function-destination #o30)) cons-lap-sym)

(defprop write-memory-data-start-write 
		(or (source-p (error))
		    (field function-destination #o32)) cons-lap-sym)

(defprop md-start-write 
		(or (source-p (error))
		    (field function-destination #o32)) cons-lap-sym)

(defprop write-memory-data-write-map
		(or (source-p (error))
		    (field function-destination #o33)) cons-lap-sym)

(defprop md-write-map
		(or (source-p (error))
		    (field function-destination #o33)) cons-lap-sym)

(defprop vma (or (source-p (field function-source #o10))
		 (field function-destination #o20)) cons-lap-sym)

(defprop vma-start-read (or (source-p (error))
			    (field function-destination #o21)) cons-lap-sym)

(defprop vma-start-write (or (source-p (error))
			     (field function-destination #o22)) cons-lap-sym)

(defprop vma-write-map  (or (source-p (error))
			    (field function-destination #o23)) cons-lap-sym)

;10 C-PDL-BUFFER-POINTER

(defprop c-pdl-buffer-pointer-push 
		(or (source-p (error))
		    (field function-destination #o11)) cons-lap-sym)

(defprop micro-stack-data-push 
		(or (source-p (error))
		    (field function-destination #o15)) cons-lap-sym)

(defprop oa-reg-low (or (source-p (error))
			(field function-destination #o16)) cons-lap-sym)

(defprop oa-reg-high (or (source-p (error))
			 (field function-destination #o17)) cons-lap-sym)
(defprop oa-reg-hi (or (source-p (error))
		       (field function-destination #o17)) cons-lap-sym)

;New names for some things:  pdl-buffer

(defprop pdl-push
	 (or (source-p (error))
	     (field function-destination #o11)) cons-lap-sym)

(defprop pdl-pop
	 (or (source-p (field function-source #o24))
	     (error)) cons-lap-sym)

(defprop pdl-pointer
	 (or (source-p (field function-source 2))
	     (field function-destination #o14)) cons-lap-sym)

(defprop pdl-aux-pointer
	 (or (source-p (field function-source 3))
	     (field function-destination #o13)) cons-lap-sym)

(defprop pdl-pointer-indirect
	 (or (source-p (field function-source #o25))
	     (field function-destination #o10)) cons-lap-sym)

(defprop pdl-aux-pointer-indirect
	 (or (source-p (field function-source 5))
	     (field function-destination #o12)) cons-lap-sym)

;Other things

(defprop i-arg 
	 (or (source-p (field function-source 0))
	     (error)) cons-lap-sym)

(defprop map-data (or (source-p (field function-source #o11))
		      (error)) cons-lap-sym)

(defprop instruction-fetch (force-dispatch #.(ash 1 24)) cons-lap-sym)
(defprop push-own-address (force-dispatch #.(ash 1 25)) cons-lap-sym)

(defprop dispatch-on-map-18 (force-dispatch #.(ash 1 8)) cons-lap-sym)
(defprop dispatch-on-map-19 (force-dispatch #.(ash 1 9)) cons-lap-sym)

(defprop shift-alu-right
	(field alu-output-bus-selector-multiplier 2) cons-lap-sym)
(defprop shift-alu-left
	(field alu-output-bus-selector-multiplier 3) cons-lap-sym)
