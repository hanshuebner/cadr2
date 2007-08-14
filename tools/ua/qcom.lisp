;;;;-*-MODE:LISP; BASE:8-*-

;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(in-package "MICRO-ASSEMBLER")

(defvar area-list '(
		  resident-symbol-area			;t and nil
	system-communication-area		;used by paging, console, pdp10 i/o, etc.
	scratch-pad-init-area			;load micro code variables upon startup
	micro-code-symbol-area			;600 qs misc dispatch, ucode entry dispatch
	page-table-area				;page hash table
	physical-page-data			;region number,,pht index
						;-1 if out of service
	region-origin				;fixnum base address indexed by region #
	region-length				;fixnum length indexed by region #
	region-bits				;fixnum, see %%region- syms for fields
	region-sorted-by-origin			;region#'s in increasing order of origin
						; last entry replicated through to the end
						; must have data-type dtp-fix and no cdr-code
						; does not include free regions & free region#s
		;end wired areas
	region-free-pointer			;fixnum, relative allocation point.
	region-gc-pointer			;gc use, mainly relative dirty/clean boundary
	region-list-thread			;next region# in area, or 1_23.+area#
						; threads free regions (in free-area), free #s
	area-name				;atomic name indexed by area #
	area-region-list			;first region# in area
	area-region-size			;recommended size for new regions
	area-maximum-size			;approximate maximum #wds allowed in this area
	free-area				;owns free regions of virtual address space
	support-entry-vector			;constants needed by basic microcode
	constants-area				;common constants used by macrocode
	extra-pdl-area				;separately gc-able area, mainly extended nums
						; must be right before micro-code-entry-area
	micro-code-entry-area			;micro entry address
						;or locative indirect micro-code-symbol-area
	micro-code-entry-name-area		;micro entry name
	micro-code-entry-args-info-area		;micro entry %args-info
	micro-code-entry-max-pdl-usage		;micro entry pdl depth incl micro-micro calls
	micro-code-exit-area	;-obsolete- flush when convenient.  replaced by
				       ; mc-code-exit-vector. constants used by user microcode

		;areas after here are not "initial", not known specially by microcode 
	micro-code-entry-arglist-area		;value for arglist function to return
	micro-code-symbol-name-area		;names of micro-code-symbol-area entries
	linear-pdl-area				;main pdl
	linear-bind-pdl-area			;corresponding bind pdl
	init-list-area				;list constants created by cold load
		;end fixed areas, which must have only one region
	working-storage-area			;ordinary consing happens here
	permanent-storage-area			;put "permanent" data structures here
	property-list-area			;exists for paging reasons
	p-n-string				;print names and strings
	control-tables				;obarray, readtable (semi-obsolete)
	obt-tails				;obarray bucket conses (semi-obsolete)
	nr-sym					;symbols not in resident-symbol-area
	rubout-processor-area			;rubout processor buffer (obsolete)
	macro-compiled-program			;macro code loaded here
	pdl-area				;put stack-group regular-pdls here
	fasl-table-area				;fasload's table is here
	fasl-temp-area				;fasload temporary consing
	fasl-constants-area			;fasload loads constants here
  ))

 ;THESE AREAS ARE ENCACHED IN THE PDL BUFFER.
(defvar pdl-buffer-area-list '(
	linear-pdl-area				;main pdl
	pdl-area				;pdls for misc stack groups
))

 ;Note that at present all areas up to REGION-SORTED-BY-ORIGIN must be wired.
 ;The reason is that when the microcode starts up it STRAIGHT-MAPS that
 ;amount of virtual memory, without checking separately for each page.
 ;it would lose big if one of those STRAIGHT-MAPPED pages got swapped out.
(defvar wired-area-list '(			;areas that may not be moved nor swapped out
	resident-symbol-area			;no good reason
	system-communication-area		;for console, pdp10, etc.
	scratch-pad-init-area			;load micro code variables upon startup
	micro-code-symbol-area			;no good reason, actually
	page-table-area				;used by page fault handler
	physical-page-data			;used by page fault handler
	region-origin				;used by page fault handler
	region-length				;used by page fault handler
	region-bits				;used by page fault handler
	region-sorted-by-origin			;used by page fault handler
))

;This list isn't necessarily up to date.  Feature isn't really used yet.
(defvar read-only-area-list '(			;areas to be set up read only by cold load
	scratch-pad-init-area
	micro-code-symbol-area
	support-entry-vector
	constants-area
	init-list-area
	micro-code-symbol-name-area
))

;; Default area size is one page

(defvar rm-area-sizes '(p-n-string #o600 obt-tails #o100 nr-sym #o500 macro-compiled-program #o1000
	page-table-area #o20	;must be a power of two.  must agree with size-of-page-table
				; and page-table-area in cload.  in addition, the microcode
				; must be assembled with the correct value of this in effect.
	physical-page-data #o4	;enough for 256k machine
	rubout-processor-area #o4 
	linear-pdl-area #o100 linear-bind-pdl-area #o10 pdl-area #o300 
	working-storage-area #o400 permanent-storage-area #o200 property-list-area #o100
	control-tables #o13 init-list-area #o40
	micro-code-entry-area 2 micro-code-entry-name-area 2
	micro-code-entry-args-info-area 2 micro-code-entry-arglist-area 2
	micro-code-entry-max-pdl-usage 2
	micro-code-symbol-name-area 2 micro-code-symbol-area 2 
	fasl-table-area #o201  ;3 times length-of-fasl-table plus 1 page
	micro-code-exit-area 0 fasl-constants-area #o600 extra-pdl-area #o10
	fasl-temp-area 40))

(defprop nr-sym #o20000 cold-load-array-size) ;don't make it 200000 in maclisp at cold load time!
(defprop fasl-temp-area #o1000 cold-load-array-size) ;used for once-only-init-list
(defprop property-list-area #o1000 cold-load-array-size)

;In the cold-load, areas have only one region, so you can only use one
;representation type per area.  These are the list areas, the rest are structure areas.
(defvar list-structured-areas '(
	system-communication-area scratch-pad-init-area micro-code-symbol-area
	page-table-area physical-page-data region-origin region-length
	region-bits region-sorted-by-origin region-free-pointer region-gc-pointer
	region-list-thread area-name area-region-list area-region-size
	area-maximum-size support-entry-vector constants-area
	micro-code-entry-area micro-code-entry-name-area
	micro-code-entry-args-info-area micro-code-entry-max-pdl-usage
	micro-code-entry-arglist-area
	micro-code-symbol-name-area init-list-area property-list-area
	obt-tails fasl-constants-area
))

(defvar static-areas '(	;not including Fixed areas
	init-list-area permanent-storage-area p-n-string control-tables
	nr-sym rubout-processor-area macro-compiled-program 
	fasl-table-area fasl-temp-area fasl-constants-area
))

; Numeric values of data types, shifted over into the data type field,
; suitable for being added to the pointer to produce the contents of a Q.
; These do NOT go into the cold load.
(defvar data-types '(qztrap qznull qzfree			;errors
 qzsym qzsymh qzfix qzxnum				;ordinary atoms
 qzhdr
 qzgcf qzevcp qz1qf qzhf qxbf				;forwards
 qzloc							;locatives
 qzlist							;lists
 qzuent							;functions, etc...
 qzfefp qzaryp qzaryh					;...
 qzstkg qzclos
 qzsflo qzsmth qzinst qzinsh qzenty 
 ))

;DTP-ENTITY  THIS IS "TEMPORARY"

; Numeric values of data types, suitable for being DPB'd into the
; data type field, or returned by (%DATA-TYPE ...).
(defvar q-data-types '(dtp-trap dtp-null dtp-free 
 dtp-symbol dtp-symbol-header dtp-fix dtp-extended-number dtp-header 
 dtp-gc-forward dtp-external-value-cell-pointer dtp-one-q-forward
 dtp-header-forward dtp-body-forward
 dtp-locative
 dtp-list 
 dtp-u-entry 
 dtp-fef-pointer dtp-array-pointer dtp-array-header 
 dtp-stack-group dtp-closure dtp-small-flonum dtp-select-method 
 dtp-instance dtp-instance-header dtp-entity 
 ))

; Numeric values of CDR codes, right-justified in word for %P-CDR-CODE, etc.
(defvar q-cdr-codes '(cdr-normal cdr-error cdr-nil cdr-next))

; Byte pointers at the parts of a Q or other thing, and their values.
; Q-FIELD-VALUES does NOT itself go into the cold load.
(defvar %%q-cdr-code (byte 2 30))
(defvar %%q-flag-bit (byte 1 29))
(defvar %%q-data-type (byte 5 24))
(defvar %%q-pointer (byte 24 0))
(defvar %%q-pointer-within-page (byte 7 0))
(defvar %%q-typed-pointer (byte 29 0))
(defvar %%q-all-but-typed-pointer (byte 3 29))
(defvar %%q-all-but-pointer (byte 8 29))
(defvar %%q-all-but-cdr-code (byte 30 0))
;; use these for referencing macro instructions
(defvar %%q-high-half (byte 16 16))
(defvar %%q-low-half (byte 16 0))
;; fields in a 16-bit character
(defvar %%ch-font (byte 8 8))
(defvar %%ch-char (byte 8 0))
(defvar %%kbd-char (byte 8 0))
(defvar %%kbd-control-meta (byte 4 8))
(defvar %%kbd-control (byte 1 8))
(defvar %%kbd-meta (byte 1 9))
(defvar %%kbd-super (byte 1 10))
(defvar %%kbd-hyper (byte 1 11))
(defvar %%kbd-mouse (byte 1 15))
(defvar %%kbd-mouse-button (byte 3 0))
(defvar %%kbd-mouse-n-clicks (byte 3 3))

(defvar q-fields '(%%q-cdr-code %%q-flag-bit %%q-data-type
				%%q-pointer %%q-pointer-within-page
				%%q-typed-pointer %%q-all-but-typed-pointer
				%%q-all-but-pointer %%q-all-but-cdr-code
				%%q-high-half %%q-low-half
				%%ch-font %%ch-char %%kbd-char
				%%kbd-control-meta %%kbd-control
				%%kbd-meta %%kbd-super %%kbd-hyper
				%%kbd-mouse %%kbd-mouse-button
				%%kbd-mouse-n-clicks))

;(defvar %q-flag-bit (logdpb -1 %%q-flag-bit 0))  ;used by qlf in cold mode

;;; Stuff in the REGION-BITS array, some of these bits also appear in the
;;; map in the same orientation.  This is for CADR, CONS has its map aligned
;;; differently, and is just too much trouble to hack.

(defvar %%region-map-bits (byte 10 14))		;10 bits to go into the map (access/status/meta)
;;#o2404				;access and status bits
(defvar %%region-oldspace-meta-bit (byte 1 19)) ;0=old or free, 1=new or static or fixed.
					 ;0 causes transport-trap for read of ptr to here
(defvar %%region-extra-pdl-meta-bit (byte 1 18)) ;0=extra-pdl, 1=normal.
					  ;0 traps writing of ptr to here into "random" mem
(defvar %%region-representation-type (byte 2 16))  ;data representation type code:
(defconstant %region-representation-type-list 0)
(defconstant %region-representation-type-structure 1)   ;2 and 3 reserved for future
;; #o1602 spare meta bits
;; #o1501 spare (formerly unimplemented compact-cons flag)
(defvar %%region-space-type (byte 4 9))	;code for type of space:

(defconstant %region-space-free 0)	;0 free region (in free-area) or free region#
(defconstant %region-space-old 1)	;1 oldspace region of dynamic area
(defconstant %region-space-new 2)	;2 newspace region of dynamic area
(defconstant %region-space-static 3)	;3 static area
(defconstant %region-space-fixed 4)	;4 fixed, static+not growable+no consing allowed
(defconstant %region-space-exited 5)	;5 like static but has exit-vector
(defconstant %region-space-exit 6)	;6 an exit-vector
(defconstant %region-space-extra-pdl 7)	;7 an extra-pdl for some stack-group
(defconstant %region-space-wired 8)	;10 like static, gc-pointer is physical address
(defconstant %region-space-user-paged 9) ;11 gc doesn't touch, trap to user on page fault
(defconstant %region-space-copy 10)	;12 like newspace, stuff copied from oldspace goes
					;   here while newly-consed stuff goes to newspace

(defvar %%region-scavenge-enable (byte 1 8))	;if 1, scavenger touches this region
;;#o0010 spare bits.  will include paging-algorithm.

(defvar q-region-bits '(%%region-map-bits %%region-oldspace-meta-bit
					  %%region-extra-pdl-meta-bit
					  %%region-representation-type
					  %region-representation-type-list
					  %region-representation-type-structure
					  %%region-space-type
					  %region-space-free
					  %region-space-old
					  %region-space-new
					  %region-space-static
					  %region-space-fixed
					  %region-space-exited
					  %region-space-exit
					  %region-space-extra-pdl
					  %region-space-wired
					  %region-space-user-paged
					  %region-space-copy
					  %%region-scavenge-enable))

(defvar system-communication-area-qs '(	;locations relative to #o400 in cadr
	  ;locations #o400-#o437 are miscellaneous Qs declared below
	  ;locations 440-477 are the reverse first level map
	  ;locations 500-577 are the keyboard buffer
	  ;locations 600-637 are the disk-error log
	  ;locations 700-777 are reserved for disk CCW's (only 777 used now)
	  ;In CADR, location 777 is used (for now) by the disk code for the CCW.
	%sys-com-area-origin-pntr		;address of area-origin area
	%sys-com-valid-size			;in a saved band, number of words used
		;*** Next two are no longer used I believe ***
	%sys-com-page-table-pntr		;address of page-table-area
	%sys-com-page-table-size		;number of qs
	%sys-com-obarray-pntr			;current obarray, could be an array-pointer
						;but now is usually a symbol whose value
						;is the currently-selected obarray (package)
		;*** Next five are no longer used I believe ***
	%sys-com-remote-keyboard		;0 or char input from pdp10
						; this is in lisp machine 2+8 form

						;the following four have no data type
	%sys-com-micro-load-m-data		;m source data
	%sys-com-micro-load-a-data		;a source data
	%sys-com-micro-load-address		;register address (a, m, d, or i)
	%sys-com-micro-load-flag		;changing this causes micro load bootstrap
						;to look at and clear preceding three.

	%sys-com-unibus-interrupt-list		;see lmio;unibus (list of unibus channels)

	%sys-com-temporary			;microcode bashes this at extra-pdl-purge

	%sys-com-free-area/#-list		;threaded through area-region-list, end=0
	%sys-com-free-region/#-list		;threaded through region-list-thread, end=0
	%sys-com-memory-size			;number of words of main memory
	%sys-com-wired-size			;# words of low memory wired down

		;Chaos net interrupt-handler variables
	%sys-com-chaos-free-list
	%sys-com-chaos-transmit-list
	%sys-com-chaos-receive-list

		;Debugger locations
	%sys-com-debugger-requests		;request to power control/debugger
	%sys-com-debugger-keep-alive		;keep alive flag word
	%sys-com-debugger-data-1		;for intercommunication
	%sys-com-debugger-data-2

        %sys-com-major-version		;major cold load version as fixnum.  available to
					; microcode for downward compatibility.
		;to be added:
		;swap out scheduler and disk stuff
		;eventually this may replace scratch-pad-init-area
		;those of these that don't need to survive warm boot could be in a-memory
))

(and (> (length system-communication-area-qs) #o40)
     (error '|system communication area overflow|))

(defconstant a-memory-virtual-address   ;VIRTUAL ADDRESS OF 0@A.  MUST AGREE WITH VALUE IN
      #o76776000) ;UCADR. (unfortunately called LOWEST-A-MEM-VIRTUAL-ADDRESS).
(defconstant io-space-virtual-address	 ;Virtual address of X-BUS IO space.
      #o77000000) ;Must agree with LOWEST-IO-SPACE-VIRTUAL-ADDRESS in UCADR.
(defconstant unibus-virtual-address	 ;Virtual address of UNIBUS IO space.
      #o77400000) ;Must agree with LOWEST-UNIBUS-VIRTUAL-ADDRESS in UCADR.

(defvar %initially-disable-trapping nil)    ;this non-nil inhibits lisp-reinitialize from
					  ; doing an (enable-trapping)
(defvar inhibit-scheduling-flag nil)	  ;this non-nil inhibits clock & scheduling


(defvar %%header-type-field (byte 5 19))
(defvar %%header-rest-field (byte 19 0))
(defvar header-fields  '(%%header-type-field %%header-rest-field))

; These are the values that go in the %%HEADER-TYPE-FIELD of a Q of
; data type DTP-HEADER.
(defvar q-header-types '(%header-type-error %header-type-fef
			%header-type-array-leader
		       %header-type-unused %header-type-flonum %header-type-complex
		       %header-type-bignum %header-type-rational-bignum))

; These are the header types, shifted so they can be added directly into a Q.
; These do NOT go in the cold load.
(defvar header-types '(header-type-error header-type-fef
			header-type-array-leader
		     header-type-unused header-type-flonum header-type-complex
		     header-type-bignum header-type-rational-bignum))

; These three lists describing the possible types of "argument descriptor info"
(defvar adi-kinds '(adi-err adi-return-info adi-restart-pc adi-fexpr-call 
			adi-lexpr-call adi-bind-stack-level adi-unused-6
			adi-used-up-return-info))

(defvar adi-storing-options '(adi-st-err adi-st-block adi-st-list 
	adi-st-make-list adi-st-indirect))

(defvar %%adi-type (byte 3 20))
(defvar %%adi-ret-storing-option (byte 3 17))
(defvar %%adi-ret-swap-sv (byte 1 16))
(defvar %%adi-ret-num-vals-expecting (byte 6 0))
(defvar %%adi-rpc-micro-stack-level (byte 6 0))

(defvar adi-fields '(%%adi-type %%adi-ret-storing-option
				%%adi-ret-swap-sv %%adi-ret-num-vals-expecting
				%%adi-rpc-micro-stack-level))

; LINEAR-PDL-QS and LINEAR-PDL-FIELDS, and their elements, go in the real machine.
(defvar linear-pdl-qs '(%lp-fef %lp-entry-state %lp-exit-state %lp-call-state))
		;these are assigned values starting with 0 and incrementing by -1
  (assign-values-init-delta linear-pdl-qs 0 0 -1)

(defvar %lp-call-block-length (length linear-pdl-qs))
(defvar llpfrm 4)			;# fixed alloc qs in linar pdl block (obsolete, use above)

(defvar %lp-initial-local-block-offset 1)

(defvar linear-pdl-fields-values '(
     ;lpcls (%lp-call-state).  Stored when this call frame is created.
     ;bits 26', 27' not used in LPCLS
     %%lp-cls-downward-closure-pushed #o2501	;not used
     %%lp-cls-adi-present #o2401 			;ADI words precede this call-block
     %%lp-cls-destination #o2004			;Where in the caller to put this frame's value
     %%lp-cls-delta-to-open-block #o1010		;Offset back to previous open or active block
     %%lp-cls-delta-to-active-block #o0010	;Offset back to previous active block
						;An active block is one that is executing
						;An open block is one whose args are being made
     ;LPEXS (%LP-EXIT-STATE).  Stored when this frame calls out.
     ;bits 22'-27' not used in LPEXS
     %%lp-exs-micro-stack-saved #o2101		;A microstack frame exists on special pdl
     %%lp-exs-pc-status #o2001			;Same as below
     %%lp-exs-binding-block-pushed #o2001 	;M-QBBFL STORED HERE IN MACRO EXIT OPERATION 
     %%lp-exs-exit-pc #o0017			;LC as offset in halfwords from FEF
						;Meaningless if %LP-FEF not a fef.
	;; Don't change %%LP-EXS-EXIT-PC, the numerical value is known by UCADR
     ;LPENS (%LP-ENTRY-STATE).  Stored when this frame entered.
     ;bits 16'-27' not used in LPENS
;    %%lp-ens-specials #o2601 %%lp-ens-binding-arrow-direction #o2501 
;    %%lp-ens-environment-pointer-points-here #o2401 
     %%lp-ens-num-args-supplied #o1006
     %%lp-ens-macro-local-block-origin #o0010))

(assign-alternate linear-pdl-fields-values)
(defvar linear-pdl-fields (get-alternate linear-pdl-fields-values))

;; MICRO-STACK-FIELDS and its elements go in the real machine.
(defvar %%us-rpc (byte 16 0))		;return pc
(defvar %%us-macro-instruction-return (byte 1 14)) ;triggers instruction-stream stuff
(defvar %%us-ppbmia (byte 1 15))	;adi on micro-to-micro-call
(defvar %%us-ppbspc (byte 1 17))	;binding block pushed

(defvar micro-stack-fields '(%%us-rpc %%us-macro-instruction-return
				      %%us-ppbmia %%us-ppbspc))


; M-FLAGS-FIELDS and M-ERROR-SUBSTATUS-FIELDS and their elements go in the real machine.
(defvar m-flags-fields-values '(		;must agree with defs in ucons
	%%m-flags-qbbfl #o0001 		;bind block open flag
	%%m-flags-car-sym-mode #o0102	;car of symbol gives: error, error except 
					; (car nil) -> nil, nil, p-name pointer
	%%m-flags-car-num-mode #o0302	;car of number gives: error, nil, "whatever it is"
	%%m-flags-cdr-sym-mode #o0502	;cdr of symbol gives: error, error except
					; (cdr nil) -> nil, nil, property-list
	%%m-flags-cdr-num-mode #o0702	;cdr of number gives: error, nil, "whatever it is"
	%%m-flags-dont-swap-in #o1101	;magic flag for creating fresh pages
	%%m-flags-trap-enable #o1201	;1 enable error trapping
	%%m-flags-mar-mode #o1302		;1-bit = read-trap, 2-bit = write-trap
	%%m-flags-pgf-write #o1501	;flag used by page fault routine
	%%m-flags-interrupt #o1601	;in microcode interrupt
	%%m-flags-scavenge #o1701		;in scavenger
	%%m-flags-transport #o2001	;in transporter
	%%m-flags-stack-group-switch #o2101 ;switching stack groups
	%%m-flags-deferred-sequence-break #o2201 ;sequence break pending but inhibited
))
(assign-alternate m-flags-fields-values)
(defvar m-flags-fields (get-alternate m-flags-fields-values))

(defvar m-error-substatus-fields-values '(	;must agree with defs in ucons
	%%m-esubs-too-few-args #o0001
	%%m-esubs-too-many-args #o0101
	%%m-esubs-bad-quoted-arg #o0201 
	%%m-esubs-bad-evaled-arg #o0301
	%%m-esubs-bad-dt #o0401
	%%m-esubs-bad-quote-status #o0501
))
(assign-alternate m-error-substatus-fields-values)
(defvar m-error-substatus-fields (get-alternate m-error-substatus-fields-values))

;A "Numeric Argument Description" is what %ARGS-INFO and ARGS-INFO return.
;Such descriptors can also be hung on symbols' Q-ARGS-PROP properties.
;The "fast option Q" of a FEF is stored in this format.
;These symbols go in the real machine.
(defconstant %arg-desc-quoted-rest #o10000000) ;has quoted rest argument
(defvar %%arg-desc-quoted-rest (byte 1 21))
(defconstant %arg-desc-evaled-rest #o4000000) ;has evaluated rest argument
(defvar %%arg-desc-evaled-rest (byte 1 20))
(defvar %%arg-desc-any-rest (byte 2 20)) ;non-zero if has either kind of rest arg
(defconstant %arg-desc-fef-quote-hair #o2000000) ;macro compiled fcn with hairy quoting,
(defvar %%arg-desc-fef-quote-hair (byte 1 19)) ; caller must check a-d-l for full info
(defconstant %arg-desc-interpreted #o1000000) ;this is interpreted function, 
(defvar %%arg-desc-interpreted (byte 1 18)) ; no information available (val=1000077)
(defconstant %arg-desc-fef-bind-hair #o400000) ;macro compiled fcn with hairy binding,
(defvar %%arg-desc-fef-bind-hair (byte 1 17)) ; linear enter must check a-d-l
(defvar %%arg-desc-min-args (byte 6 6))	;minimum number of required args
(defvar %%arg-desc-max-args (byte 6 0))	;maximum number of required+optional
					; args.  rest args not counted.

(defvar numeric-arg-desc-fields '(%arg-desc-quoted-rest %%arg-desc-quoted-rest
							%arg-desc-evaled-rest
							%%arg-desc-evaled-rest
							%%arg-desc-any-rest
							%arg-desc-fef-quote-hair
							%%arg-desc-fef-quote-hair
							%arg-desc-interpreted
							%%arg-desc-interpreted
							%arg-desc-fef-bind-hair
							%%arg-desc-fef-bind-hair
							%%arg-desc-min-args
							%%arg-desc-max-args))

(defvar arg-desc-field-values '(%fef-arg-syntax #o160 %fef-quote-status #o600
       %fef-des-dt #o17000 
       %fef-init-option #o17 %fef-special-bit #x10000
       %fef-name-present #x100000
;***unfortunately, assign-comp-values knows about these too****
       %%fef-name-present #o2001
       %%fef-special-bit #o1601 %%fef-specialness #o1602
       %%fef-functional #o1501 %%fef-des-dt #o1104
       %%fef-quote-status #o0702 %%fef-arg-syntax #o0403
       %%fef-init-option #o0004
))

(assign-alternate arg-desc-field-values)
(defvar arg-desc-fields (get-alternate arg-desc-field-values))
	;arg-desc-fields gets set to a list consisting of the alternating members of 
	;arg-desc-field-values

(defvar fef-name-present '(fef-nm-no fef-nm-yes))
(defvar fef-specialness '(fef-local fef-special fef-specialness-unused fef-remote))
(defvar fef-functional '(fef-functional-dontknow fef-functional-arg))
(defvar fef-des-dt '(fef-dt-dontcare fef-dt-number fef-dt-fixnum fef-dt-sym
			 fef-dt-atom fef-dt-list fef-dt-frame))
(defvar fef-quote-status '(fef-qt-dontcare fef-qt-eval fef-qt-qt))
(defvar fef-arg-syntax '(fef-arg-req fef-arg-opt fef-arg-rest fef-arg-aux
			 fef-arg-free fef-arg-internal fef-arg-internal-aux))
(defvar fef-init-option '(fef-ini-none fef-ini-nil fef-ini-pntr fef-ini-c-pntr
			 fef-ini-opt-sa fef-ini-comp-c fef-ini-eff-adr
			 fef-ini-self))


(defvar array-field-values '(
  	%%array-type-field #o2305 %%array-leader-bit #o2101
	%%array-displaced-bit #o2001 %%array-flag-bit #o1701
	%%array-number-dimensions #o1403 %%array-long-length-flag #o1301
	%%array-named-structure-flag #o1201 
        %%array-index-length-if-short #o0012
	%array-max-short-index-length #o1777))

(defvar array-leader-field-values '(%array-leader-length #o777777 
       %%array-leader-length #o0022))

(defvar array-misc-values '(array-dim-mult #x4000 array-dimension-shift #o-14 
   array-type-shift #o-23 array-leader-bit #x200000
   array-displaced-bit #x100000 array-long-length-flag #x2000
   array-named-structure-flag #x1000))

(defvar array-fields (get-alternate array-field-values))

(defvar array-leader-fields (get-alternate array-leader-field-values))

(defvar array-miscs (get-alternate array-misc-values))

(defvar array-types '(art-error art-1b art-2b art-4b art-8b art-16b art-32b 
	 art-q art-q-list art-string art-stack-group-head art-special-pdl art-tvb 
	 art-reg-pdl art-float))

(defvar array-elements-per-q '((art-q . 1) (art-string . 4) (art-1b . #o40) (art-2b . #o20)
     (art-4b . #o10) (art-8b . 4) (art-16b . 2) (art-32b . 1) (art-q-list . 1) 
     (art-stack-group-head . 1) (art-special-pdl . 1) (art-tvb . #o20)
     (art-reg-pdl . 1) (art-float . -2)))

;NIL for Q-type arrays
(defvar array-bits-per-element '((art-q . nil) (art-string . 8) (art-1b . 1) (art-2b . 2)
     (art-4b . 4) (art-8b . 8) (art-16b . 16.) (art-32b . 24.) (art-q-list . nil) 
     (art-stack-group-head . nil) (art-special-pdl . nil) (art-tvb . 1) 
     (art-reg-pdl . nil) (art-float . 32.)))

;FEF HEADER FIELDS
(defvar fefh-constant-values '(%fefh-pc #o177777	;There are 19 available bits in this word!
      %fefh-no-adl #x40000
      %fefh-fast-arg #x20000 %fefh-sv-bind #x10000
      %%fefh-pc #o0020 %%fefh-pc-in-words #o0117 %%fefh-no-adl #o2201
      %%fefh-fast-arg #o2101 %%fefh-sv-bind #o2001))

(assign-alternate fefh-constant-values)

(defvar fefh-constants (get-alternate fefh-constant-values))

;FEF HEADER Q INDEXES

(defvar fefhi-indexes '(%fefhi-ipc %fefhi-storage-length %fefhi-fctn-name %fefhi-fast-arg-opt
		      %fefhi-sv-bitmap %fefhi-misc %fefhi-special-value-cell-pntrs))

(defvar ifefoff (1- (length fefhi-indexes)))	;q's in fixed alloc part of fef
(defvar %fef-header-length ifefoff)		;better name for above

(defvar fefhi-values '(%%fefhi-fso-min-args #o0606 %%fefhi-fso-max-args #o0006
      %%fefhi-ms-local-block-length #o0007 %%fefhi-ms-arg-desc-org #o0710
      %%fefhi-ms-bind-desc-length #o1710
      %%fefhi-ms-debug-info-present #o2701
      %%fefhi-svm-active #o2601
      %fefhi-svm-active #x4000000
      %%fefhi-svm-bits #o0026
      %%fefhi-svm-high-bit #o2501))

(defvar fefhi-fields (get-alternate fefhi-values))

;Page table stuff etc.

(defvar page-values '(

      ; definitions of fields in page hash table

      ;word 1 
      %%pht1-virtual-page-number #o1020	;aligned same as vma
	%pht-dummy-virtual-address #o177777 ;all ones means this is dummy entry
					;which just remembers a free core page
      %%pht1-swap-status-code #o0003
	%pht-swap-status-normal 1	;ordinary page
	%pht-swap-status-flushable 2	;safely reusable to swap pages into
					;may need to be written to disk first
	%pht-swap-status-prepage 3	;same as flushable, but came in via prepage
	%pht-swap-status-age-trap 4	;like normal but trying to make flushable
	%pht-swap-status-wired 5	;not swappable

      %%pht1-age #o0302			;number of times aged

      %%pht1-modified-bit #o0501		;1 if page modified, but the fact not recorded
					; in the map-status, because it is nominally read-only
					; or nominally read-write-first.

      %%pht1-valid-bit #o0601		;1 if this hash table slot is occupied.

      ;pht word 2.  this is identical to the level-2 map
      %%pht2-meta-bits #o1606				;see %%region-map-bits

      %%pht2-map-status-code #o2403
	%pht-map-status-map-not-valid 0		;level 1 or 2 map not set up
	%pht-map-status-meta-bits-only 1	;has meta bits but no physical address
	%pht-map-status-read-only 2		;garbage collector can still write in it
	%pht-map-status-read-write-first 3	;read/write but not modified
	%pht-map-status-read-write 4		;read/write and modified
	%pht-map-status-pdl-buffer 5		;may reside in pdl buffer
	%pht-map-status-mar 6			;mar set somewhere on this page

      %%pht2-map-access-code #o2602
      %%pht2-access-status-and-meta-bits #o1612
      %%pht2-access-and-status-bits #o2404 
      %%pht2-physical-page-number #o0016
))

(assign-alternate page-values)
(defvar page-hash-table-fields (get-alternate page-values))

;;; See LISPM2;SGDEFS
(defvar stack-group-head-leader-qs '(sg-name 
      sg-regular-pdl sg-regular-pdl-limit sg-special-pdl sg-special-pdl-limit
      sg-initial-function-index 
      sg-ucode 
;end static section, begin debugging section
      sg-trap-tag   ;symbolic tag corresponding to sg-trap-micro-pc.  gotten via
                    ; microcode-error-table, etc.  properties off this symbol
                    ; drive various stages in error recovery, etc.
      sg-recovery-history  ;available for hairy sg munging routines to leave tracks in
                        ; for debugging purposes.
      sg-foothold-data  ;structure which saves dynamic section of "real" sg when
                        ; executing in the foothold. 
; locations below here are actually loaded/stored on sg-enter/sg-leave
;end debugging section, begin "high level" section
      sg-state sg-previous-stack-group sg-calling-args-pointer 
      sg-calling-args-number ;sg-following-stack-group 
      sg-trap-ap-level
;end high-level section, begin "dynamic" section --below here is saved in 
; sg-foothold-data when %%sg-st-foothold-executing is set.
      sg-regular-pdl-pointer sg-special-pdl-pointer
      sg-ap sg-ipmark 
      sg-trap-micro-pc  ;pc saved from opcs at micro-location trap
;     sg-error-handling-sg sg-interrupt-handling-sg 
;       having these be part of the sg is basically a good idea, but it
;       doesnt buy anything for the time being and costs a couple of microinstructions
      sg-saved-qlaryh sg-saved-qlaryl sg-saved-m-flags  
      sg-ac-k sg-ac-s sg-ac-j 
      sg-ac-i sg-ac-q sg-ac-r sg-ac-t sg-ac-e sg-ac-d sg-ac-c 
      sg-ac-b sg-ac-a sg-ac-zr sg-ac-2 sg-ac-1 sg-vma-m1-m2-tags sg-saved-vma sg-pdl-phase))

;Fields in SG-STATE Q
(defvar sg-state-field-values '(%%sg-st-current-state #o0006
      %%sg-st-foothold-executing #o0601
      %%sg-st-processing-error #o0701
      %%sg-st-processing-interrrupt-request #o1001
      %%sg-st-safe #o1101
      %%sg-st-inst-disp #o1202
      %%sg-st-in-swapped-state #o2601
      %%sg-st-swap-sv-on-call-out #o2501
      %%sg-st-swap-sv-of-sg-that-calls-me #o2401))
(defvar sg-state-fields (get-alternate sg-state-field-values))

(defvar sg-inst-dispatches '(
      sg-main-dispatch			;main instruction dispatch
      sg-debug-dispatch			;debugging dispatch
      sg-single-step-dispatch		;dispatch once, and then break
      sg-single-step-trap		;for sequence breaks out of trapping instructions
      ))

(defvar sg-states '(
      sg-state-error         ;0 should never get this
      sg-state-active        ;actually executing on machine.
      sg-state-resumable     ;reached by interrupt or  error recovery completed
                             ; just restore state and do a ucode popj to resume.
      sg-state-awaiting-return    ;after doing a "legitimate" sg-call.  to resume this
                                  ; reload sg then return a value by transferring to
                                  ; qmex1.
      sg-state-invoke-call-on-return  ;to resume this, reload sg, then simulate
                                      ; a store in destination-last.  the error
                                      ; system can produce this state when it wants
                                      ; to activate the foothold or perform a retry.
      sg-state-interrupted-dirty  ;get this if forced to take an interrupt at an
                                  ; inopportune time.
      sg-state-awaiting-error-recovery   ;immediatedly after error, before recovery
      sg-state-awaiting-call 
      sg-state-awaiting-initial-call
      sg-state-exhausted))

(defvar special-pdl-leader-qs '(special-pdl-sg-head-pointer))
(defvar reg-pdl-leader-qs '(reg-pdl-sg-head-pointer))

;;mesa stuff
;(defvar mesa-instruction-field-values '(%%mesa-op-code 1305 %%mesa-address 1300 
;      mesa-address-exit-vector #o2000 mesa-address-indirect #o1000 
;      mesa-address-const-page #x100))
;
;(assign-alternate mesa-instruction-field-values)
;
;(defvar mesa-instruction-fields (get-alternate mesa-instruction-field-values))
;
;(defvar mesa-fef-field-values '(%%mesa-fef-max-ip-usage 1710 
;	%%mesa-fef-total-storage-used 1700))
;
;(assign-alternate mesa-fef-field-values)
;
;(defvar mesa-fef-fields (get-alternate mesa-fef-field-values))
;
;(defvar mesa-parameter-values '(mesa-max-exit-vector-length 500. 
;	mesa-fef-length 4))
;
;(assign-alternate mesa-parameter-values)
;
;(defvar mesa-parameters (get-alternate mesa-parameter-values))

(defconstant page-size #o400)

(defconstant length-of-fasl-table #o40000)

(defconstant length-of-atom-head 5)

(defconstant size-of-ob-tbl #o177)	 ;used by pre-package intern kludge

(defconstant size-of-area-arrays #o377)

(defconstant size-of-page-table #o10000)  	;length in qs of page-table-area
					;used by ucadr

;Size of various hardware memories in "addressible locations"
(defconstant size-of-hardware-control-memory   #o40000)
(defconstant size-of-hardware-dispatch-memory  #o4000)
(defconstant size-of-hardware-a-memory         #o2000)
(defconstant size-of-hardware-m-memory           #o40)
(defconstant size-of-hardware-pdl-buffer       #o2000)
(defconstant size-of-hardware-micro-stack        #o40)
(defconstant size-of-hardware-level-1-map      #o4000)
(defconstant size-of-hardware-level-2-map      #o2000)
(defconstant size-of-hardware-unibus-map         #o20)

(defvar a-memory-location-names '(	;list in order of contents of a-memory starting at 40
  %microcode-version-number		;second file name of microcode source file as a number
  %number-of-micro-entries		;number of slots used in micro-code-entry-area
  default-cons-area			;default area for cons, list, etc.
  number-cons-area			;for bignums, big-floats, etc.  can be 
					; extra-pdl-area or just regular area.
  %initial-fef				;pointer to fef of function machine starts up in
  %error-handler-stack-group		;sg to switch to on traps
  %current-stack-group			;current stack-group
  %initial-stack-group			;stack-group machine starts up in
  %current-stack-group-state		;sg-state q of current stack group
  %current-stack-group-previous-stack-group	;
  %current-stack-group-calling-args-pointer	;
  %current-stack-group-calling-args-number	;
; %current-stack-group-following-stack-group	;
  %trap-micro-pc                        ;pc gotten out of opcs by trap
  %counter-block-a-mem-address		;loc of beginning of counter block relative to
					; a memory as a fixnum.
  %chaos-csr-address			;xbus address
  %mar-low				;fixnum mar lower bound (inclusive)
  %mar-high				;fixnum mar upper bound (inclusive)
					;%%m-flags-mar-mode controls the above
  self					;self pointer for dtp-instance, etc
  %method-search-pointer		;method list element were last method found.
  inhibit-scheduling-flag		;non-nil suppresses sequence breaks
  inhibit-scavenging-flag		;non-nil turns off the scavenger
  %disk-run-light			;address of disk run light, that+2 is proc run light
  %loaded-band				;low 24 bits (fixnum) of booted band name (e.g. "od3")
  %disk-blocks-per-track		;(from label) blocks per track, usually 17.
  %disk-blocks-per-cylinder		;(from label) 85. on t-80, 323. on t-300
		;the garbage-collector process hangs on these variables
  %region-cons-alarm			;counts new regions created
  %page-cons-alarm			;counts pages allocated to regions
  %gc-flip-ready			;if non-nil, there are no pointers to oldspace
  %inhibit-read-only			;if non-nil, you can write in read-only
  %scavenger-ws-enable			;if non-nil, scavenger working set hack enabled
  %method-subroutine-pointer		;continuation point for select-method subroutine
					; or nil.
  %qlaryh				;header of last array ref'ed as function
  %qlaryl				;element # of last array ref'ed as function
  %scheduler-stack-group		;force call to this on sequence-break.  this
					;stack group must bind on inhibit-scheduling-flag as
					;part of the stack-group switch for proper operation.
  %current-sheet			;sheet or screen currently selected by microcode
  %read-compare-enables			;fixnum: 1 r/c after read, 2 r/c after write
  %mc-code-exit-vector			;exit vector used by microcompiled code to ref q
  					; quantities.  replaces micro-code-exit-area.
  alphabetic-case-affects-string-comparison ;if t, upper and lower case are not equal
  zunderflow				;if non-nil, floating pointer underflow yields zero
  %gc-generation-number			;increments whenever any new oldspace is created.
					; thus if this has changed, objects' addresses
					; may have changed.
))

(defvar a-memory-counter-block-names '(
  %count-first-level-map-reloads	;# first level map reloads
  %count-second-level-map-reloads	;# second level map reloads
  %count-pdl-buffer-read-faults		;# took pgf and did read from pdl-buffer
  %count-pdl-buffer-write-faults	;# took pgf and did write to pdl-buffer
  %count-pdl-buffer-memory-faults	;# took pgf for pdl-buf, but data in main mem.
  %count-disk-page-reads		;count of pages read from disk
  %count-disk-page-writes		;count of pages written to disk
  %count-disk-errors			;count of recoverable errs
  %count-fresh-pages			;count of fresh pages 
					; generated in core instead of read from disk
  %count-aged-pages			;number of times ager set age trap
  %count-age-flushed-pages		;number of times age trap -> flushable
  %count-disk-read-compare-rewrites	;count of writes redone due to failure to read-compare
  %count-disk-recalibrates		;due to seek errors
  %count-meta-bits-map-reloads		;# second level map reloads to meta-bits-only
  %count-chaos-transmit-aborts		;number of transmit aborts in microcode
  %count-disk-read-compare-differences	;number of read-compare differences without
					; accompanying disk read error
  %count-cons-work			;gc parameter
  %count-scavenger-work			;..
  %tv-clock-rate			;tv frame rate divided by this is seq brk clock
  %aging-depth				;number of laps to age a page.  don't make > 3!!
  %count-disk-ecc-corrected-errors	;number of soft ecc errors
  %count-findcore-steps			;number of iterations finding mem to swap out
  %count-findcore-emergencies		;number of times findcore had to age all pages
  %count-disk-read-compare-rereads	;reads done over due to r/c diff or error
  %count-disk-page-read-operations	;read operations (count once even if multipage)
  %count-disk-page-write-operations	;write operations (count once even if multipage)
  %count-disk-page-write-waits		;waiting for a page to get written, to reclaim core
  %count-disk-page-write-busys		;waiting for a page to get written, to use disk
  %count-disk-prepages-used		;counts prepaged pages that were wanted
  %count-disk-prepages-not-used		;counts prepaged pages that were reclaimed
  %disk-error-log-pointer		;address of next 4-word block in 600-637
))

(defvar m-memory-location-names 		   ;m-mem locns are assigned piecemeal..
      '(%mode-flags %sequence-break-source-enable))
  (setf (get '%mode-flags 'forwarding-virtual-address)
	(+ a-memory-virtual-address #o26))

  (setf (get '%sequence-break-source-enable 'forwarding-virtual-address)
	(+ a-memory-virtual-address #o34))


(defvar disk-rq-leader-qs '(%disk-rq-leader-n-hwds	;number halfwords really used
			  %disk-rq-leader-n-pages	;number of buffer pages allocated
			  %disk-rq-leader-buffer	;displaced art-16b array to buffer pgs
			  %disk-rq-leader-thread))	;link to next free rqb
(defvar disk-rq-hwds '(%disk-rq-done-flag			;0 rq entered, -1 completed
		     %disk-rq-done-flag-high
		     ;; these are set up by the requester
		     %disk-rq-command			;disk command register
		     %disk-rq-command-high
		     %disk-rq-ccw-list-pointer-low	;clp low 16
		     %disk-rq-ccw-list-pointer-high	;clp high 6
		     %disk-rq-surface-sector		;disk address reg low
		     %disk-rq-unit-cylinder		;disk address reg high
		     ;; these are stored when the operation completes
		     %disk-rq-status-low		;disk status reg low 16
		     %disk-rq-status-high		;disk status reg high 16
		     %disk-rq-mem-address-low		;last mem ref addr low 16
		     %disk-rq-mem-address-high		;last mem ref addr high 6
		     %disk-rq-final-surface-sector	;disk address reg low
		     %disk-rq-final-unit-cylinder	;disk address reg high
		     %disk-rq-ecc-position
		     %disk-rq-ecc-pattern
		     %disk-rq-ccw-list))			;ccw list customarily starts here


(defvar disk-hardware-values '(%%disk-status-high-block-counter #o1010
			       %%disk-status-high-internal-parity #o0701
			       %%disk-status-high-read-compare-difference #o0601 %%disk-status-high-ccw-cycle #o0501
	%%disk-status-high-nxm #o0401 %%disk-status-high-mem-parity #o0301
	%%disk-status-high-header-compare #o0201 %%disk-status-high-header-ecc #o0101
	%%disk-status-high-ecc-hard #o0001
	%disk-status-high-error #o237 ;mask for bits which are errors normally
	%%disk-status-low-ecc-soft #o1701 %%disk-status-low-overrun #o1601
	%%disk-status-low-transfer-aborted #o1501 %%disk-status-low-start-block-error #o1401
	%%disk-status-low-timeout #o1301 %%disk-status-low-seek-error #o1201
	%%disk-status-low-off-line #o1101 %%disk-status-low-off-cylinder #o1001
	%%disk-status-low-read-only #o0701 %%disk-status-low-fault #o0601
	%%disk-status-low-no-select #o0501 %%disk-status-low-multiple-select #o0401
	%%disk-status-low-interrupt #o0301 %%disk-status-low-sel-unit-attention #o0201
	%%disk-status-low-attention (byte 1 1)
	%%disk-status-low-ready (byte 1 0)

	%disk-status-low-error #o177560  ;mask for bits which are errors normally
	%disk-command-done-interrupt-enable #x800
	%disk-command-attention-interrupt-enable #x400 ;trident only
	%disk-command-recalibrate #o10001005
	%disk-command-fault-clear #o10000405	;recalibrate on marksman
	%disk-command-data-strobe-late #o200	;these are all different on marksman
	%disk-command-data-strobe-early #o100	;..
	%disk-command-servo-offset #o40		;..
	%disk-command-servo-offset-forward #o20	;..
	%disk-command-read 0
	%disk-command-read-compare #o10
	%disk-command-write #o11
	%disk-command-read-all 2
	%disk-command-write-all #o13
	%disk-command-seek #o20000004
	%%disk-command-seek-cylinder #o3010	;only used by marksman
	%disk-command-at-ease 5			;get status on marksman
	%disk-command-offset-clear 6		;nop on marksman
	%disk-command-reset-controller 16))
		;Marksman also has get-status commands, not listed here.

(assign-values disk-rq-leader-qs 0)
(assign-values disk-rq-hwds 0)
(assign-alternate disk-hardware-values)
(defvar disk-hardware-symbols (get-alternate disk-hardware-values))

;;; Definitions for interrupt-driven Unibus input channels
;;; Note that these start at 1 rather than at 0, to leave room for an array header

(defvar unibus-channel-qs '(
	%unibus-channel-link			;Address of next or 0 to end list
	%unibus-channel-vector-address		;Interrupt vector address of device
	%unibus-channel-csr-address		;Virtual address of status register
	%unibus-channel-csr-bits		;Bits which must be on in CSR
	%unibus-channel-data-address		;Virtual address of data register(s)
						;The %%Q-FLAG bit means there are 2 data regs
	%unibus-channel-buffer-start		;Start address of buffer
	%unibus-channel-buffer-end		;End address+1 of buffer
	%unibus-channel-buffer-in-ptr		;Address of next word to store
	%unibus-channel-buffer-out-ptr))	;Address of next word to extract
(assign-values-init-delta unibus-channel-qs 0 1 1)

;;; Definitions for Chaos net hardware and microcode

;;;  Command/Status register fields

(defvar chaos-hardware-values
      '(%%chaos-csr-timer-interrupt-enable (byte 1 0)
        %%chaos-csr-loop-back (byte 1 1)
	%%chaos-csr-receive-all (byte 1 2)
	%%chaos-csr-receiver-clear (byte 1 3)
	%%chaos-csr-receive-enable (byte 1 4)
	%%chaos-csr-transmit-enable (byte 1 5)
	%%chaos-csr-interrupt-enables (byte 2 4)
	%%chaos-csr-transmit-abort (byte 1 6)
	%%chaos-csr-transmit-done (byte 1 7)
	%%chaos-csr-transmitter-clear (byte 1 8)
	%%chaos-csr-lost-count (byte 4 9)
	%%chaos-csr-reset (byte 1 13)
	%%chaos-csr-crc-error (byte 1 14)
	%%chaos-csr-receive-done (byte 1 15)

;;; Offsets of other registers from CSR
;;; These are in words, not bytes

	%chaos-my-number-offset 1
	%chaos-write-buffer-offset 1
	%chaos-read-buffer-offset 2
	%chaos-bit-count-offset 3
	%chaos-start-transmit-offset 5))

;;; Leader of a wired Chaos buffer

(defvar chaos-buffer-leader-qs '(
	%chaos-leader-word-count		;Fill pointer for ART-16B array
	%chaos-leader-thread			;Next buffer in wired list (free, rcv, xmt)
						;NIL for end of list
	%chaos-leader-csr-1			;Receive stores CSR before reading out here
	%chaos-leader-csr-2			;Receive stores CSR after reading out here
						;Get lost-count from here
	%chaos-leader-bit-count))		;Receive stores bit-count before reading out

(assign-values chaos-buffer-leader-qs 0)
(assign-alternate chaos-hardware-values)
(defvar chaos-hardware-symbols (get-alternate chaos-hardware-values))

;Use of DTP-INSTANCE.  Points to a structure whose header is of
;type DTP-INSTANCE-HEADER; the pointer field of that header points
;to a structure (generally an array) which contains the fields described
;below.  This structure is called an instance-descriptor and contains
;the constant or shared part of the instance.  The instance structure,
;after its DTP-INSTANCE-HEADER, contains several words used as value
;cells of instance variables, which are the variable or unshared
;part of the instance.
;Note that these are offsets, not indices into the array.  They
;are defined here this way because microcode uses them.  This could
;be a cdr-coded list or an instance rather than an array.
(defvar instance-descriptor-offsets '(
	%instance-descriptor-header		;The array header.
	%instance-descriptor-reserved		;e.g. for named-structure symbol
	%instance-descriptor-size		;The size of the instance; this is one more
						;than the number of instance-variable slots.
						;This is looked at by the garbage collector.
	%instance-descriptor-bindings
		;Describes bindings to perform when the instance
		;is called.  If this is a list, then SELF is bound
		;to the instance and the elements of the list are
		;locatives to cells which are bound to EVCP's
		;to successive instance-variable slots of the
		;instance.  If this is not a list, it is something
		;reserved for future facilities based on the same
		;primitives.  NIL is a list.
		;Note that if this is a list, it must be CDR-CODED!
		;The microcode depends on this for a little extra speed.
	%instance-descriptor-function		;Function to be called when the instance
						; is called.  Typically a DTP-SELECT-METHOD
	%instance-descriptor-typename		;A symbol which is returned by TYPEP
))	;Additional slots may exist, defined by the particular class system employed.
	;If the instance-descriptor is an array, it must not be so long as to
	;contain a long-length Q.
(assign-values instance-descriptor-offsets 0)

(defun assign-qcom-values nil 
	(assign-values adi-kinds 0)
	(assign-values adi-storing-options 0)
	(assign-alternate arg-desc-field-values)
	(assign-alternate array-field-values)
	(assign-alternate array-leader-field-values)
	(assign-alternate array-misc-values)
	(assign-values array-types 19.)
	(assign-values data-types 24.)
	(assign-values fef-arg-syntax 4)
	(assign-values fef-des-dt #o11)
	(assign-values fef-functional #o15)
	(assign-values fef-init-option 0)
	(assign-values fef-name-present #o20)
	(assign-values fef-quote-status #o7)
	(assign-values fef-specialness #o16)
	(assign-values fefhi-indexes 0)
	(assign-alternate fefhi-values)
        ;;(assign-alternate header-field-values)
        (assign-values header-types #o23)
	(assign-values q-cdr-codes 0)
	(assign-values q-data-types 0)
        (assign-values q-header-types 0)
	(assign-alternate sg-state-field-values)
	(assign-values sg-states 0)
	(assign-values sg-inst-dispatches 0)
	(assign-values special-pdl-leader-qs 0)
	(assign-values stack-group-head-leader-qs 0)
	(assign-values system-communication-area-qs 0)
	(assign-values reg-pdl-leader-qs 0)
)

(assign-qcom-values)  ;foo.   assign-values, etc had better be defined.


