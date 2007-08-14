;;; -*- Mode: LISP; Package: UA -*-
;;;     MICRO ASSEMBLER  FOR CADR

;;;TO COMPILE OR RUN ON THE LISP MACHINE, USE THE PACKAGE DEFINITION IN LCADR;UA PKG

(in-package "UA")

(defmacro logldb (ptr val) `(ldb ,ptr ,val))
(defmacro logdpb (newval ptr val) `(dpb ,newval ,ptr ,val))

;SYMBOLS IN CONS-LAP:
; A SYMBOL IN CONS-LAP HAS AS ITS VALUE A PROGRAM!
;  THE PROGRAM IS EVALUATED BY RECURSIVE CALLS TO CONS-LAP-EVAL.
;  IF THE ARGUMENT TO CONS-LAP-EVAL IS NUMERIC, IT IS RETURNED AS THE VALUE.
;  IF NIL, THIS SPECIFIES THE NULL VALUE.
;  IF A SYMBOL, ITS VALUE IS RUN AS A PROGRAM AND RETURNED.
;  IF A LIST, CAR OF THE LISP IS THE FUNCTION AND THE REST OF THE LIST
;   ARGUMENTS, LISP STYLE.  UNLESS OTHERWISE NOTED BELOW, ALL FUNCTIONS
;   EVALUATE THEIR ARGS (LISP STYLE) AND ACTUALLY DO SOMETHING ONLY
;   AFTER THE EVALUATION OF THEIR ARGUMENTS HAS FINISHED.

;AVAILABLE FUNCTIONS:
; FUNCTIONS OF ONE ARGUMENT
;  SPECIFERS OF LOCALITY: A-MEM, M-MEM, I-MEM, D-MEM.
;	RETURN VALUE INDICATING THAT THEIR ARGUMENT CORRESPONDS TO AN
;	ADDRESS IN THE SPECIFIED MEMORY.
;  CONDITIONALS:  DESTINATION-P, SOURCE-P, DISPATCH-INSTRUCTION-P, JUMP-INSTRUCTION-P 
;       ALU-INSTRUCTION-P, BYTE-INSTRUCTION-P. EVALUATE AND RETURN ARGUMENT
;       ONLY IF SPECIFIED CONDITION TRUE (NAMELY: ASSEMBLING A DESTINATION FIELD,
;	A SOURCE FIELD, OR THE TYPE OF INSTRUCTION INDICATED). RETURN NIL
;	IF CONDITION FALSE. 
;  NEGATION: NOT. MUST BE NESTED WITH ONE OF THE CONDITIONALS ABOVE AS IS
;	(NOT (DESTINATION (...))).
;  OR. RETURNS FIRST NON-NIL VALUE LIKE LISP OR.
;  PLUS. COMBINES THE VALUES / PROPERTIES REPRESENTED BY ALL ITS ARGUMENTS.
;       USED TO BE TWO ARGS ONLY, NOW TAKES ANY NUMBER OF ARGS.
;  DIFFERENCE.  LIKEWISE.
;  INSTRUCTION-TYPE FORCE: FORCE-DISPATCH, FORCE-JUMP, FORCE-ALU, FORCE-BYTE.
;       FORCE-DISPATCH-OR-BYTE, FORCE-ALU-OR-BYTE.
;  DEFAULT-CONDITION.  DEFAULT-BTYE. IF DISPATCH IS FORCED, RETURN NIL.
;	OTHERWISE FORCE BYTE.
;  BYTE-FIELD <BITS BITS-OVER>. DEFAULTS BYTE-INSTRUCTION.  ERROR IF OTHER THAN
;	BYTE INSTRUCTION OR DISPATCH INSTRUCTION (OR IF A ONE BIT FIELD,
;	JUMP INSTRUCTION).  ASSEMBLES THE RIGHT THING
;	TO REFERENCE BYTE, AS PER WHAT INSTRUCTION TYPE IS.
;  LISP-BYTE <%% FORM BYTE SPECIFIER>.  SIMILIAR TO BYTE-FIELD, BUT BYTE DESCRIPTION IS 
;	OBTAINED BY EVAL ING ARGUMENT AND INTERPRETING IT AS A BYTE SPECIFIER.
;       I.E. PPSS WHERE PP GIVES POSITION AND SS GIVES SIZE A LA PDP-10 
;	BYTE INSTRUCTION.
;  ALL-BUT-LISP-BYTE <%% FORM BYTE SPECIFIER>.  SIMILAR, BUT ADDRESSES BITS NOT IN
;	<BYTE>.  <BYTE> MUST BE EITHER LEFT OR RIGHT ADJUSTED IN 32. BITS.
;  BYTE-MASK <SYMBOLIC BYTE SPECIFIER>.  ARG CAN BE SYMBOL OR COMPOSITION OF
;	OPS AND SYMBOLS SPECIFYING A BYTE (IE CONTAINING SOMEWHERE IN THERE
;	A BYTE-FIELD OR LISP-BYTE OPERATION).  THIS IS DUG OUT BY BYTE-MASK
;	AND IS RETURNS THE VALUE OF ALL 1'S IN THE SPECIFIED BYTE.
;  BYTE-VALUE <SYMBOLIC BYTE SPECIFIER> <VALUE TO STORE IN BYTE>
;	RETURNS A VALUE OF THE SPECIFIED NUMBER IN THE SPECIFIED BYTE.
;	FOR CONVENIENCE, THE VALUE MAY BE EITHER A CONS-LAP SYMBOL OR A LISP SYMBOL.
;  FIELDS: (FIELD <FIELD NAME> <VALUE>).  NOTATION IS MADE THAT <FIELD NAME>
;	HAS BEEN SPECIFIED.  THE VALUE IS OBTAINED AS FOLLOWS:  THE PROGRAM
;	ASSOCIATED WITH <FIELD NAME> AS A SYMBOL IS RUN AND ITS VALUE MULTIPLIED
;	BY <VALUE> (THIS IS DONE RATHER THAN SHIFTING SO BIGNUMS WORK CONVIENTLY).
;	ADDITIONALLY, IF A CONS-LAP-ADDITIVE-CONSTANT
;	PROPERTY IS PRESENT ON <FIELD NAME> IT WILL BE ADDED IN AFTER MULTIPLING.
;	ANY PROPERTIES SPECIFIED IN THE	RUNNING OF <FIELD NAME> STICK.
;  I-ARG.  ASSEMBLES ITS ARGUMENT INTO THE IMMEDIATE ARGUMENT FIELD OF A DISPATCH
;	INSTRUCTION.
;  ((ARG-CALL ADR) .. ) OR ((ARG-JUMP ADR) .. ).  ASSEMBLES A DISPATCH INSTRUCTION
;       WHICH DISPATCHES ON ZERO BITS TO A D-MEM ENTRY WHICH DOES A CALL (OR JUMP)
;	TO ADR.  USE IF IT IS DESIRED TO SUPPLY AN I-ARG ON AN UNCONDITIONAL
;	CALL (OR JUMP).  ((ARG-CALL-XCT-NEXT ADR) .. ) AND ((ARG-JUMP-XCT-NEXT ADR) ..)
;	ARE ALSO AVAILABLE.
;  EVAL <ARG>.  CALLS LISP EVAL ON ARG AND RETURNS (NUMERIC HOPEFULLY) VALUE.
;  LOC <ARG> SETS LOCATION COUNTER TO <ARG>.
;  MODULO <ARG> SETS LOCATION COUNTER TO BE ON A MOD <ARG> BOUNDARY.
; The following group provide communication between an assembly and microcompiled
;    code or other assemblies which may be added to it.
;  MC-LINKAGE <list of symbols>.  The values of these symbols are made available
;       to the micro-compiled-code loader and to the incremental mode of the assembler.
;	A and M memory symbols with values less than 40 are automatically 
;       MC-LINKAGEifyed.
;  MC-LINKAGE sym.  Useful primarily in incrmental assemblies.  Expands to value
;	given sym in either current or previous
;      assembly.  Includes appropriate memory.
;  MC-ENTRY-ADR <microcoded-function>  allowable only in incremental assembly.
;	evaluates to I-MEM address of entry to <function> in JUMP-ADDRESS field.
;  MISC-ENTRY-ADR <misc-instruction>   allowable only in incremental assembly.
;       evaluates to I-MEM address of entry to <misc-instruction> in JUMP-ADDRESS field.
;  MC-LINKAGE-VALUE <memory> <symbol>  useful primarily in incremental assemblies.
;       <memory> must be one of NUMBER, I-MEM, D-MEM, A-MEM, M-MEM.  <symbol> must
;       have been assigned a value with the MC-LINKAGE operation (either in the
;       current assembly, or a previous one to which this assembly is being added).
;       Evaluates to the value in the appropriate memory.

;  INSTRUCTIONS FOR ASSEMBLING VALUES FOR USE WITH OA REGISTER.  (RECALL? THAT
;	THE OA "REGISTER" IS THE HACK WHEREBY THE NEXT MICRO-INSTRUCTION GETS
;	IOR-ED WITH DATA PRODUCED BY THIS ONE).
;    OA-LOW-CONTEXT OA-HIGH-CONTEXT <I-MEM STORAGE-WORD>.  ASSEMBLES <I-MEM STORAGE
;	WORD> AND RETURN EITHER HI OR LOW PART AS NUMBER FOR USE WITH DESTINATIONS
;	OA-REG-HI OR OA-REG-LOW.
; SYMBOLS MAY BE EITHER ON THE SYMTAB OR ON THE PROPERTY LIST UNDER THE INDICATOR
;  CONS-LAP-SYM.

;THE TYPE OF INSTRUCTION THAT GETS ASSEMBLED IN A GIVEN STORAGE WORD IS DETERMINED
;AS FOLLOWS:
;  FIRST THERE IS A DEFAULT, ALU-INSTRUCTION.  IT IS OVERRIDDEN BY ANY OTHER SPECIFIER.
;	THIS IS THE ONLY SPECIFIER THAT
;	CAN BE "OUT-OF-HARMONY" WITH ANY OTHER PRESENT SPECIFIER WITHOUT CAUSING AN 
;	ERROR. 
;  IF A DESTINATION IS PRESENT, INSTRUCTION MUST BE ALU-INSTRUCTION OR BYTE-INSTRUCTION.
;  IF AN I-MEM CONTEXT SYMBOL IS PRESENT, INSTRUCTION MUST BE JUMP-INSTRUCTION.
;  IF A D-MEM CONTEXT SYMBOL IS PRESENT, INSTRUCTION MUST BE DISPATCH-INSTRUCTION.
;  IF BOTH A M-MEM AND A A-MEM SYMBOL ARE PRESENT, INSTRUCTION MUST BE ALU-INSTRUCTION
;	OR BYTE-INSTRUCTION.
;  INSTRUCTION CAN BE FORCED BY A FORCE-INSTRUCTION PROPERTY ON ANY SYMBOL IN THE
;	WORD.
;  TWO A-MEM OR TWO M-MEM SYMBOLS IN ONE INSTRUCTION IS AN ERROR.

;ONCE INSTRUCTION TYPE IS DETERMINED, A CHECK IS MADE TO SEE THAT ALL NECESSARY
; FIELDS IN IT HAVE BEEN SPECIFIED, AND DEFAULTS SUPPLIED FOR VARIOUS OPTIONAL
; FIELDS AND MODES IF THEY WERE NOT SPECIFIED.

;RANDOM CONVENTIONS --
; LOCATION TAGS ARE DEFINED AS FIELDS. IE (FIELD JUMP-ADDRESS-MULTIPLIER NNN)
; FOR SYMBOLS IN I-MEM. (A-SOURCE-MULTIPLIER, M-SOURCE-MULTIPLIER, AND 
; DISPATCH-ADDRESS-MULTIPLIER ARE THE CORRESPONDING FIELDS FOR A-MEM, M-MEM,
; AND D-MEM RESPECTIVELY).  THUS, WHEN NORMALLY EVALUATED, THEY HAVE
; THEIR VALUES IN THESE "PLACES".  THIS IS THE RIGHT THING EXCEPT FOR THESE
; CASES: 1)  DESTINATIONS.  CONVERT-VALUE-TO-DESTINATION COMPUTES AN APPROPRIATE
;		"SHIFT"
;	 2)  LOCALITY D-MEM.  CONS-LAP-PASS2 DOES THE RIGHT THING.  THIS INVOLVES
;		SHIFTING THE I-MEM ADR BACK TO THE LOW PART AND MOVING THE RPN
;		BITS UP (FROM THEIR NORMAL POSITION IN A JUMP INSTRUCTION).
; OTHER FEATURES/CROCKS
;   WHEN A BYTE-FIELD OPERATION IS ENCOUNTERED BY CONS-LAP-EVAL,
;	THE INSTRUCTION CONTEXT IS FORCED TO BYTE IF IT HAS NOT ALREADY
;	BEEN COMPLETELY SPECIFIED.  THEN THE BYTE REFERENCE IS ASSEMBLED
;	IN THE MANNER APPROPRIATE TO THE INSTRUCTION CONTEXT.
;   THE SR-BIT IS STORED INVERTED (SO THAT IT WILL OFF FOR NORMAL LDB).
;	CONS-LAP-DEFAULT-AND-BUGGER REVERSES SR-BIT IF IT'S A BYTE INSTRUCTION
;   THE HARDWARE IMPLEMENTS A LEFT ROTATE FOR THE M-ROTATE FIELD.  The is the
;       "right thing" for DPB and SELECTIVE-DEPOSIT, but LDB, DISPATCH, and 
;       JUMP-IF-BIT-SET need to be 32-reflected (IE ( 32. - M-ROTATE) MOD 32.) 
;       This is done by CONS-LAP-DEFAULT-AND-BUGGER.
;	CODE USING THE OA-REGISTER FEATURE TO MODIFY BYTE TYPE INSTRUCTIONS
;	MUST BE AWARE OF THIS.
;  TO PUT THE ADDRESS OF A MICRO CODE LOCATION INTO A CONSTANT IN A OR M
;	MEMORY, USE THE KLUDGEY CONSTRUCTION (I-MEM-LOC <TAG>).
;  SIMILARLY, A-MEM-LOC, M-MEM-LOC, D-MEM-LOC PSEUDO-OPS EXIST.

;   OPERATION OF THE ARG-CALL, ETC, FEATURE IN DISPATCH INSTRUCTIONS.
;	SOMETIMES IT IS DESIRABLE TO USE A DISPATCH INSTRUCTION WHEN
;	REALLY ONLY AN UNCONDITIONAL TRANSFER (CALL, ETC) IS DESIRED
;	IN ORDER TO BE ABLE TO LOAD THE DISPATCH-CONSTANT REGISTER IN THE
;	SAME INSTRUCTION.  IT WOULD BE A PAIN TO HAVE TO DEFINE A ONE REGISTER
;	DISPATCH TABLE, ETC IN THIS CASE.  SO THE ASSEMBLER PROVIDES A FEATURE
;	WHEREBY ARG-CALL, ARG-JUMP, ARG-CALL-XCT-NEXT, AND ARG-JUMP-XCT-NEXT
;	ARE SPECIALLY RECOGNIZED.  USING THESE PSEUDO-OPS, THE INSTRUCTION
;	MAY BE WRITTEN AS "NORMAL" AND THE ASSEMBLER WILL TAKE CARE OF
;	ALLOCATING A D-MEM LOCATION AND MOVING THE RPN BITS AND I-MEM JUMP ADDRESS
;	BITS THERE.  THIS D-MEM LOCATION IS AUTOMATICALLY PLUGGED INTO THE
;	DISPATCH OFFSET.
;   ON A NORMAL PDP-10 STYLE LOAD BYTE, THE A-MEM ADDRESS MUST CONTAIN 0
;	FOR CORRECT OPERATION.  A-MEM
;	LOCATION 2 IS CHOSEN TO CONTAIN ZERO, AND LOCATION 3 TO CONTAIN -1,
;	MAKING A CONVENIENT PAIR FOR DOING SIGN-EXTENSION.  THE A-MEM ADDRESS
;	OF A LOAD-BYTE INSTRUCTION WILL BE DEFAULTED TO 2 IF NOT SPECIFIED.

;ENTRY POINTS INTO MICRO-CODE FROM MACRO-CODE, ETC:
;   THE MICRO-CODE-SYMBOL AREA CONTAINS ALL (INITIAL) ENTRY POINTS INTO
;  MICRO-CODE.  THE FIRST 600 Q'S OF MICRO-CODE-SYMBOL AREA GIVE THE CONTROL-MEMORY
;  TRANSFER ADDRESSES FOR MACRO-CODE MISC-INSTRUCTIONS 200-777.  FOLLOWING THAT
;  ARE OTHER ENTRY POINTS, MOSTLY FOR MICRO-COMPILED RUNTIME ROUTINES, ETC.
;  THESE LAST ARE NOT REFERENCED DYNAMICALLY, BUT JUST BY LOADERS, ETC.
;   THE MICRO-CODE-SYMBOL AREA IS COMPLETELY DETERMINED BY CONSLP UNDER CONTROL
;  OF THE (MISC-INST-ENTRY <NAME>) PSEUDO-OPERATION.
;     (MISC-INST-ENTRY <NAME>) DECLARES THAT THE CURRENT LOCATION IS THE ENTRY POINT 
;	WHEN <NAME> IS EXECUTED AS A MACRO-INSTRUCTION. CONSLP LOOKS ON THE PROPERTY
;	LIST OF <NAME> TO FIND THE QLVAL PROPERTY (WHICH HAD BETTER BE THERE OR ERROR).
;	THESE QLVAL COME FROM LISPM;DEFMIC. CONSLP THEN ARRANGES FOR . TO APPEAR
;	IN THE APPROPRIATE LOCATION OF MICRO-CODE-SYMBOL AREA.
; IN ADDITION, (MICRO-CODE-ILLEGAL-ENTRY-HERE), ENCOUNTERED AT ANY TIME, FILLS
;	ALL UNUSED ENTRIES OF MICRO-CODE-SYMBOL AREA WITH THE CURRENT LOCATION.
;	(IT IS OK IF SOME OF THEM LATER GET STORED OVER WITH OTHER STUFF...)
;THE MC-LINKAGE PSEUDO-OP IS THE OTHER MECHANISM (BESIDE MISC-INST-ENTRY)
;  BY WHICH LINKAGE INFO CAN BE "COUPLED OUT" AND USED BY MICROCOMPILED ROUTINES.
;  USAGE IS (MC-LINKAGE <SYM> ..)  THE LOCATION WITHIN MEMORY OF SYM IS ADDED TO
;  MC-LINKAGE-ALIST, AND THAT IS WRITTEN AS PART OF THE ASSEMBLER STATE.  IF
;  SYM IS A LIST, CAR IS THE MICROCOMPILED NAME, CADR THE CONSLP NAME.

;THE ERROR TABLE:
; THE PSEUDO-OP (ERROR-TABLE FOO BAR BAZ...)
; WILL ADD THE LINE (LOC FOO BAR BAZ...) TO THE ERROR TABLE, WHERE LOC IS
; THE ADDRESS OF THE PRECEEDING I-MEM INSTRUCTION.  THE ERROR TABLE IS
; AN OUTPUT FILE, UCONS TABLE, WHICH CAN BE READ IN TO LISP.  IT CONTAINS
; A SETQ OF MICROCODE-ERROR-TABLE TO A LIST OF ERROR TABLE ENTRIES,
; AND A SETQ OF MICROCODE-ERROR-TABLE-VERSION TO THE SOURCE FILE VERSION
; NUMBER, WHICH CAN BE COMPARED AGAINST %MICROCODE-VERSION-NUMBER.

(declaim (special destination-context locality i-mem-loc d-mem-loc
	   a-mem-crevice-list a-constant-loc m-constant-loc
	   conslp-input conslp-output
	   version-number      ;Numeric value of FN2 for this file
	   base-version-number ;NIL or, if incremental assembly, version this to augment.
           a-mem-loc m-mem-loc d-mem-free-blocks field-indicators combined-value 
	   combined-indicators instruction-context in-dispatch-block 
	   dispatch-block-limit dispatch-arm dispatch-constant m-constant-list 
	   a-constant-list a-constant-base m-constant-base cons-lap-last-sym 
	   a-memory-range-list m-memory-range-list
	   i-memory-range-list d-memory-range-list 
	   cons-lap-wds-since-last-sym cons-lap-saved-symtab sr-bit 
	   arg-call-list current-word  
	   mc-linkage-alist
	   rm-area-sizes cons-lap-pass2 micro-code-symbol-table-fill-value
	   cons-lap-init-state   ;If this non-null, current assembly is incremental
			         ; from this saved state.
	   current-assembly-micro-entries   ;List, ea element, (<type> <name> <adr>),
	 				    ; in incremental assembly
	   current-assembly-table	    ;Error table
	   current-assembly-highest-misc-entry
	   current-assembly-defmics

	   ;; Was in sys
	   micro-code-symbol-area
))

;THE ARG CALL LIST IS AN ASSOCIATION LIST WHERE THE KEY IS THE I-MEM LOCATION
;AT WHICH AN ((ARG-CALL) ..) TYPE INSTRUCTION HAS APPEARED, AND THE VALUE
;IS THE D-MEM LOCATION THAT HAS BEEN ALLOCATED TO IT.

(defvar i-mem)
(defvar a-mem)
(defvar d-mem)
(defvar micro-code-symbol-image)

;ARRAYS WHICH RECEIVE THE OUTPUT OF THE ASSEMBLY
(declaim (type (simple-array nil (*)) i-mem) 		;12k control mem enuf for now
	 (type (simple-array (unsigned-byte 32) (*)) a-mem)
	 (type (simple-array (unsigned-byte 32) (*)) d-mem)
	 (type (simple-array nil (*)) micro-code-symbol-image))


(defun cons-lap-barf (a b c)
  (format t "~%~S ~S~%" cons-lap-last-sym cons-lap-wds-since-last-sym)
  (format t "~S ~S ~S~%" a b c)
  (cond ((not (eq c 'warn))(break "CONS-LAP-BARF"))))

(defun cons-lap-initialize (init-state) 
  (prog (tem) 
	(cons-lap-init-locs-from-state init-state)
	(setq base-version-number (get-from-alternating-list init-state 'version-number))
	(setq a-mem-crevice-list nil)
	(setq d-mem-free-blocks 
	      (subst nil nil (cond ((get-from-alternating-list init-state 'd-mem-free-blocks))
				   (t '(nil (#o4000 . 0)))))) ;A BLOCK OF 4000 STARTING AT 0
	(allremprop 'cons-lap-user-symbol)
	(setq m-constant-list			;DUMMY UP SLOTS FOR USAGE COUNT AND LAST
	      (cond ((setq tem (get-from-alternating-list init-state 'm-constant-list))  ;USE
		     (mapcar (function (lambda (x)
					 (append x '(#o100000 nil) nil)))
			     tem))
		    (t nil)))
	(setq a-constant-list
	      (cond ((setq tem (get-from-alternating-list init-state 'a-constant-list))
		     (mapcar (function (lambda (x)
					 (append x '(#o100000 nil) nil)))
			     tem))
		    (t nil)))
	(setq a-constant-base nil)		;SEE CONS-LAP-LOC-MODULO
	(setq m-constant-base nil)
	(setq a-memory-range-list nil)
	(setq m-memory-range-list nil)
	(setq i-memory-range-list nil)
	(setq d-memory-range-list nil)
	(setq current-assembly-micro-entries nil)
	(setq current-assembly-table nil)
  ;do not initialize current-assembly-defmics here computed during readin phase
	(setq current-assembly-highest-misc-entry
	      (cond ((get-from-alternating-list init-state 'highest-misc-entry))
		    (t 0)))
	(setq mc-linkage-alist (get-from-alternating-list init-state 'mc-linkage-alist))
	(dolist (e mc-linkage-alist)
	  (cond ((and (member (cadr e) '(a m) :test #'eq)
		      (< (caddr e) 40))
		 (cons-lap-define-linkage-symbol (car e)))))
	(cons-lap-allocate-arrays)
	(allremprop 'cons-lap-b-ptr)))

(defun make-assembler-state-list nil 
    (list 'i-mem-loc i-mem-loc 'd-mem-loc d-mem-loc 'a-mem-loc a-mem-loc
	  'm-mem-loc m-mem-loc 
	  'a-constant-loc a-constant-loc 'a-constant-base a-constant-base
	  'm-constant-loc m-constant-loc 'm-constant-base m-constant-base 
	  'd-mem-free-blocks d-mem-free-blocks 
	  'm-constant-list (make-constant-list m-constant-list)
	  'a-constant-list (make-constant-list a-constant-list)
	  'micro-code-symbol-table-fill-value
	   (cond ((boundp 'micro-code-symbol-table-fill-value)
		  micro-code-symbol-table-fill-value)
		 (t nil))
	  'a-memory-range-list a-memory-range-list
	  'm-memory-range-list m-memory-range-list
	  'i-memory-range-list i-memory-range-list
	  'd-memory-range-list d-memory-range-list
	  'mc-linkage-alist mc-linkage-alist
	  'micro-entries current-assembly-micro-entries
	  'highest-misc-entry current-assembly-highest-misc-entry))


(defun cons-lap-allocate-arrays nil 
  (setq i-mem (make-array #o30000))
  (setq a-mem (make-array #o2000 :element-type '(unsigned-byte 32)
			  :initial-element 0))
  (setq d-mem (make-array #o4000 :element-type '(unsigned-byte 32)
			  :initial-element 0))
  (let ((micro-code-symbol-image-size))
    (setq micro-code-symbol-image-size
	  (* page-size
	     (list-assq 'micro-code-symbol-area rm-area-sizes)))
    (setq micro-code-symbol-image
	  (make-array micro-code-symbol-image-size))))

(defun cons-lap-init-locs-from-state (init-state) 
  (prog (tem) 
	(setq i-mem-loc (cond ((get-from-alternating-list init-state 'i-mem-loc)) (t 0)))
	(setq d-mem-loc (cond ((get-from-alternating-list init-state 'd-mem-loc)) (t 0)))
	(setq a-mem-loc (cond ((setq tem (get-from-alternating-list init-state 'a-mem-loc))
			       (max tem (cond ((get-from-alternating-list
						init-state 'a-constant-loc))
					      (t 0))))
			      (t 0)))
	(setq m-mem-loc (cond ((setq tem (get-from-alternating-list init-state 'm-mem-loc))
			       (max tem (cond ((get-from-alternating-list
						init-state 'm-constant-loc))
					      (t 0))))
			      (t 0)))
	))

;IF INIT-STATE NON-NIL, ITS REPRESENTS A PREVIOUS ASSEMBLY
; IS TO BE AUGMENTED BY THE CURRENT ASSEMBLY.
(defun assemble (&optional fn init-state dont-re-read &aux input-file)
  (cond ((null fn)
	 (format t "~&Enter input file name: ")
	 (setq fn (read-line))))
  (setq input-file (parse-namestring fn))
  (setq conslp-input (setq conslp-output
			   (find-symbol (string-upcase (pathname-name input-file)))))
  ;; *** Next line will be fixed to the right thing later ***
  (cond ((and dont-re-read (boundp conslp-input))
	 (format t "~&Ucode already read in.~%"))
	(t
	 (format t "Reading ~A~%" conslp-input)
	 (setq current-assembly-defmics nil)
	 (load input-file)))
  (dolist (x current-assembly-defmics) ;process UA-DEFMICs read
    (apply #'ua-do-defmic x))
  (format t "Begin Assembly ~A~%" conslp-input)
  (cons-lap (symbol-value conslp-input) init-state)
  (cond ((null init-state)   ;dont write on incremental assembly
	 (write-various-outputs))))

(defun write-various-outputs ()
  (cond	((y-or-n-p "WRITE-MCR? ")
	 (write-mcr base-version-number)))
  (write-tbl-file conslp-output)
  (write-error-table conslp-output))

;; Taken from LISPM;UTIL.  This is used in reading in the DEFMIC file.
;; Only sets up the QLVAL property, not the QINTCMP property and not the
;; function lists. 

;(defun defmic fexpr (x)
;  (prog (name opcode arglist lisp-function-p no-qintcmp
;	 function-name instruction-name)
;    (setq name (car x)
;	  opcode (cadr x)
;	  arglist (caddr x)
;	  lisp-function-p (cadddr x))
;    (and (cddddr x) (setq no-qintcmp (car (cddddr x))))
;    (cond ((atom name)
;	   (setq function-name name instruction-name name))
;	  ((setq function-name (car name) instruction-name (cdr name))))
;    (putprop instruction-name opcode 'qlval)))

(defmacro defmic (name opcode arglist lisp-function-p &optional no-qintcmp)
  (cond ((atom name)
	 `(progn 
	    (defvar ,name)
	    (setf (get ',name 'qlval) ,opcode)))
	(t `(progn
	      ;(defvar ,(cdr name))
	      (setf (get ',(cdr name) 'qlval) ,opcode)))))

(defun cons-lap (u-prog &optional cons-lap-init-state)
  (prog (;i-mem-loc d-mem-loc a-mem-loc m-mem-loc m-constant-loc a-constant-loc ;use top level
	 ;m-constant-list a-constant-list m-constant-base a-constant-base  ;bindings for these
	 ;d-mem-free-blocks micro-code-symbol-table-fill-value
         ;a-memory-range-list m-memory-range-list i-memory-range-list d-memory-range-list
         ;current-assembly-micro-entries current-assembly-table current-assembly-defmics 
	 ;current-assembly-highest-misc-entry
	 ;mc-linkage-alist
         initial-a-mem-loc initial-m-mem-loc initial-i-mem-loc initial-d-mem-free-blocks
	 locality 
         in-dispatch-block cons-lap-last-sym cons-lap-wds-since-last-sym 
	 dispatch-block-limit t1 dispatch-arm cons-lap-pass2
         dispatch-constant arg-call-list)
	(setq cons-lap-wds-since-last-sym 0)
	(cons-lap-initialize cons-lap-init-state)
	(setq initial-a-mem-loc a-mem-loc initial-m-mem-loc m-mem-loc
	      initial-i-mem-loc i-mem-loc)
	(setq initial-d-mem-free-blocks (subst nil nil d-mem-free-blocks))
 	(setq t1 u-prog)
l1	(cond ((null t1) (go l2)))
	(cons-lap-pass1 (car t1))
	(setq t1 (cdr t1))
	(go l1)
l2	(setq m-constant-loc (setq m-constant-base m-mem-loc))
	(setq a-constant-loc (setq a-constant-base a-mem-loc))
	(setq cons-lap-last-sym nil)
	(setq cons-lap-wds-since-last-sym 0)
	(setq cons-lap-pass2 t)
	(cons-lap-init-locs-from-state cons-lap-init-state)
	(setq t1 u-prog)
l3	(cond ((null t1) (go l4)))
	(cons-lap-pass2 (car t1))
	(setq t1 (cdr t1))
	(go l3)
l4	(cond ((not (eql m-mem-loc m-constant-base))
	       (cons-lap-barf (list m-mem-loc m-constant-base) 'cld-m-mem 'barf)))
	(cond ((not (eql a-mem-loc a-constant-base))
	       (cons-lap-barf (list a-mem-loc a-constant-base) 'cld-a-mem 'barf)))
	(setq locality 'm-mem)
	(cons-lap-store-constant-list a-mem
				      m-constant-list)  ;this stores
			;the complete list (including those from previous assembly)
			;but i guess thats ok.
	(setq locality 'a-mem)
	(cons-lap-store-constant-list a-mem a-constant-list)
	(setq a-memory-range-list (cons (list initial-a-mem-loc
					      (- (max a-mem-loc a-constant-loc)
						 initial-a-mem-loc))
					a-memory-range-list))
	(setq m-memory-range-list (cons (list initial-m-mem-loc
					      (- (max m-mem-loc m-constant-loc)
						 initial-m-mem-loc))
					m-memory-range-list))
	(setq i-memory-range-list (cons (list initial-i-mem-loc
					      (- i-mem-loc initial-i-mem-loc))
					i-memory-range-list))
	(let ((tem (find-d-mem-ranges-used
		      (cdr initial-d-mem-free-blocks)
		      (cdr d-mem-free-blocks))))
	  (cond (tem (setq d-memory-range-list (append tem d-memory-range-list)))))
	(return "Now do (WRITE-VARIOUS-OUTPUTS) and/or (CONS-DUMP-MEMORIES)")))

(defun write-error-table (fn)
  (with-open-file (output-file (make-pathname :name (string-downcase (symbol-name fn)) :type "err")
			       :direction :output
			       :if-exists :supersede)
		  (format output-file "(defvar microcode-error-table '("
		  (dolist (i current-assembly-table)
		    (print i output-file))
		  (format output-file "))~%")))

(defun write-tbl-file (fn)
  (with-open-file (output-file (make-pathname :name (string-downcase (symbol-name fn)) :type "tbl")
			       :direction :output
			       :if-exists :supersede)
		  (print 'locations-used output-file)
		  (print (list 'a-mem (max a-mem-loc a-constant-loc)) output-file)
		  (print (list 'm-mem (max m-mem-loc m-constant-loc)) output-file)
		  (print (list 'i-mem i-mem-loc) output-file)
		  (print (list 'd-mem (- #o4000 (get-d-mem-free-locs (cdr d-mem-free-blocks))))
			 output-file)
		  (terpri output-file)
		  fn))

;;;For each old free block, determine what part of it has been used and
;;; make a list of those ranges.
(defun find-d-mem-ranges-used (old-free-blocks new-free-blocks)
  (prog (ans sa len new-sa new-len)
     l  (cond ((null old-free-blocks) (return ans)))
        (setq sa (cdar old-free-blocks) len (caar old-free-blocks))
     l1 (multiple-value-setq (new-sa new-len)
	  (find-next-free-block-higher-or-equal sa new-free-blocks))
	(cond ((null new-sa)
	       (setq ans (cons (list sa len) ans))    ;evidently, block must be used now
	       (go x1))
	      ((not (= sa new-sa))
	       (setq ans (cons (list sa (min len (- new-sa sa)))  ;part (or all) block used
			       ans))))
	(setq len (- len (- (+ new-sa new-len) sa)))    ;advance to above that one
	(cond ((<= len 0) (go x1))
	      (t (setq sa (+ new-sa new-len))
		 (go l1)))
     x1 (setq old-free-blocks (cdr old-free-blocks))
	(go l)))

(defun find-next-free-block-higher-or-equal (sa free-blocks)
  (prog (ans)
     l  (cond ((null free-blocks)
	       (cond ((null ans) (return (values nil 0)))
		     (t (return (values (cdr ans) (car ans))))))
	      ((and (>= (cdar free-blocks) sa)
		    (or (null ans)
			(< (cdar free-blocks) (cdr ans))))
	       (setq ans (car free-blocks))))
        (setq free-blocks (cdr free-blocks))
	(go l)))

(defun get-d-mem-free-locs (x)
  (cond ((null x) 0)
        (t (+ (caar x) (get-d-mem-free-locs (cdr x))))))

(defun cons-lap-store-constant-list (mem l)
  (prog nil 
	l	(cond ((null l) (return nil)))
	(setf (aref mem (cadar l)) (caar l))
	(setq l (cdr l))
	(go l)))

;;CONSTANT LISTS.
;;A LIST OF LISTS.  CAR IS VALUE OF CONSTANT, CADR IS ADDRESS, CADDR IS
;; #USERS, CADDDR IS LAST PC TO USE IT.
;;
;; ARG IS A-CONSTANT-LIST OR M-CONSTANT-LIST

(defun cons-lap-report-constants-usage (l)
  (setq l (sort (append l nil) (function (lambda (x y) (< (caddr x) (caddr y))))))
  (format t "~%#USES	VALUE	USEPC")
  (do ((l l (cdr l)))
      ((null l))
      (print (caddr (car l)))
      (print #\Tab)
      (prin1 (caar l))
      (print #\Tab)
      (prin1 (cadddr (car l))))
  (format t "~%"))

(defun cons-lap-pass1 (wd) 
  (prog (current-word)
	(setq current-word wd)			;for debugging
	(cond ((atom wd)
	       (setq cons-lap-last-sym wd)
	       (setq cons-lap-wds-since-last-sym 0)
	       (cons-lap-defsym 
		 wd 
		 (list locality 
		       (cons 'field 
			     (cond ((eq locality 'i-mem)
				    (list 'jump-address-multiplier i-mem-loc))
				   ((eq locality 'a-mem) 
				    (list 'a-source-multiplier a-mem-loc))
				   ((eq locality 'm-mem) 
				    (list 'm-source-multiplier m-mem-loc))
				   ((eq locality 'd-mem) 
				    (list 'dispatch-address-multiplier d-mem-loc))
				   (t (cons-lap-barf locality 
						     'bad-locality 
						     'barf))) )) )
	       (cond ((or (eq locality 'm-mem)		;automatically mc-linkageify
			  (and (eq locality 'a-mem)	; accumulator type frobs.
			       (< a-mem-loc #o40)))
		      (cons-lap-mc-linkage-store wd))))
	      ((eq (car wd) 'def-data-field)
		(def-data-field (cadr wd) 
				(cons-lap-arg-eval (caddr wd))
				(cons-lap-arg-eval (cadddr wd))))
	      ((eq (car wd) 'def-bit-field-in-reg)
		(def-bit-field-in-reg (cadr wd)
				      (cons-lap-arg-eval (caddr wd))
				      (cons-lap-arg-eval (cadddr wd))
				      (car (cddddr wd))))
	      ((eq (car wd) 'assign)
		(cons-lap-defsym (cadr wd)
				 (caddr wd)))
	      ((eq (car wd) 'assign-eval)
		(cons-lap-defsym (cadr wd)
				 (cons-lap-arg-eval (caddr wd))))
	      ((eq (car wd) 'def-next-bit)
		(def-next-field (cadr wd) 1 (caddr wd)))
	      ((eq (car wd) 'reset-bit-pointer)
		(reset-bit-pointer (cadr wd)))
	      ((eq (car wd) 'def-next-field)
		(def-next-field (cadr wd) 
				(cons-lap-arg-eval (caddr wd))
				(cadddr wd)))
	      ((eq (car wd) 'locality)
		(setq locality (cadr wd))
		(cond ((not (member locality '(m-mem a-mem d-mem i-mem)
				  :test #'eq))
			(cons-lap-barf locality 'bad-locality 'barf))))
	      ((eq (car wd) 'start-dispatch)
		(cond ((not (eq locality 'd-mem))
			(cons-lap-barf locality 'bad-start-dispatch 'barf)))
		(cond (in-dispatch-block 
			(cons-lap-barf wd 'already-in-dispatch 'data)))
		(setq d-mem-loc (find-d-mem-space (expt 2 (cadr wd))))
		(setq in-dispatch-block t))
	      ((eq (car wd) 'end-dispatch)
		(cond ((null in-dispatch-block)
			(cons-lap-barf wd 'not-in-dispatch-block 'data)))
		(cond ((> d-mem-loc dispatch-block-limit)
			(cons-lap-barf d-mem-loc  
				       'dispatch-block-overflow 
				       'data))
		      ((not (= d-mem-loc dispatch-block-limit))
			(cons-lap-barf (list d-mem-loc dispatch-block-limit)
			      'dispatch-block-underflow 
			      'warn)))
		(setq in-dispatch-block nil))
	      ((member (car wd) '(loc modulo) :test #'eq)
		(cons-lap-loc-modulo wd))
	      ((eq (car wd) 'repeat)
		(cons-lap-repeat-1 (cons-lap-arg-eval (cadr wd))
				   (cddr wd)))
	      ((member (car wd) '(misc-inst-entry mc-linkage mc-linkage-value
				micro-code-illegal-entry-here error-table
				mc-entry-adr misc-entry-adr) :test #'eq)
		(go x))
	      ((eq (car wd) 'comment))
	      ((eq (car wd) 'if)
	       (cond ((eval (cadr wd))
		      (cons-lap-pass1 (caddr wd)))
		     (t (mapc (function cons-lap-pass1) (cdddr wd)))))
	      (t (cons-lap-pass1-wd wd)
		 (go w1)))
x	(return nil)
w1	(setq cons-lap-wds-since-last-sym (1+ cons-lap-wds-since-last-sym))
	(cond ((eq locality 'a-mem)
		(setq a-mem-loc (1+ a-mem-loc)))
	      ((eq locality 'm-mem)
		(setq m-mem-loc (1+ m-mem-loc)))
	      ((eq locality 'd-mem)
		(cond ((not in-dispatch-block)
			(cons-lap-barf wd 'storage-wd-not-in-dispatch-block 'data)))
		(setq d-mem-loc (1+ d-mem-loc)))
	      ((eq locality 'i-mem)
		(setq i-mem-loc (1+ i-mem-loc)))
	      (t (cons-lap-barf wd 'storage-wd-in-bad-locality 'data)))
	(return nil)))

(defun cons-lap-loc-modulo (wd)
   ((lambda (point item)
	(and (eq (car wd) 'modulo)
	     (setq item (* item (/ (+ (symbol-value point) item -1) item))))
	(and (< item (symbol-value point))
	     (cons-lap-barf wd 'backwards 'data))
	(and (eq locality 'd-mem)
	     (cons-lap-d-mem-loc item))
	(and (null a-constant-base)	;on pass 1
	     (eq locality 'a-mem)	;kludge to use skipped area for constants
	     (do ((i a-mem-loc (1+ i)))
		 ((= i item))
		 (or (< i #o40)
		    (setq a-mem-crevice-list (cons i a-mem-crevice-list)))))
	(set point item))
     (cdr (assoc locality '((a-mem . a-mem-loc)
			   (m-mem . m-mem-loc)
			   (d-mem . d-mem-loc)
			   (i-mem . i-mem-loc)) :test #'eq))
     (cadr wd)))

;; Allocate one D-MEM word at a specific address
(defun cons-lap-d-mem-loc (l)
  (or cons-lap-pass2
      (do ((bl d-mem-free-blocks (cdr bl))
	   (tem))
	  ((null (cdr bl)) t)
	  (setq tem (cadr bl))		;a block
	  (cond ((and (not (< l (cdr tem))) ;if loc is in this block
		      (< l (+ (cdr tem) (car tem))))
		 (rplacd bl (cddr bl))	;patch out this block
		 (cons-lap-d-mem-loc-splitup bl (cdr tem) l) ;install blocks before loc
		 (cons-lap-d-mem-loc-splitup bl (1+ l) ;install blocks after loc
					     (+ (car tem) (cdr tem)))
		 (return nil)))))
  (setq d-mem-loc l
	in-dispatch-block t
        dispatch-constant 0	       ;dont add anything to this one.
	dispatch-block-limit (1+ l)))

;SPLIT UP INTO POWER OF 2 BLOCKS
;******* KNOWS THAT D MEM IS 4000 LOCATIONS *******
(defun cons-lap-d-mem-loc-splitup (bl low high)
  (declare (fixnum low high))
  (prog (blocksize)
    (declare (fixnum blocksize))
rcr (cond ((= low high) (return nil)))
		 ;compute largest power of 2 block starting at low
    (setq blocksize (boole 1 (+ #o4000 low) (- #o4000 low)))
a   (cond ((> (+ low blocksize) high)
	   (setq blocksize (/ blocksize 2))
	   (go a)))
    (rplacd bl (cons (cons blocksize low) (cdr bl)))	;put in this block
    (setq bl (cdr bl)	;do the remainder
	  low (+ low blocksize))
    (go rcr)))

(defun cons-lap-repeat-1 (count lst)
 (prog (orpcnt rpcnt)
        (setq orpcnt (cons-lap-symeval 'repeat-count))
	(setq rpcnt 0)
l	(cond ((zerop count)
	       (cons-lap-set 'repeat-count orpcnt)
	       (return nil)))
	(cons-lap-set 'repeat-count rpcnt)
	(mapc (function (lambda (x) (cons-lap-pass1 (cond ((atom x) (list x))
							  (t x)))))
	      lst)
	(setq count (1- count))
	(setq rpcnt (1+ rpcnt))
	(go l)))

(defun cons-lap-pass1-wd (wd)
  (prog () 
 l 	(cond ((atom wd) (return nil))
	      ((atom (car wd)))			;flush
	      ((member (caar wd)
		     '(arg-call arg-jump arg-call-xct-next arg-jump-xct-next)
		     :test #'eq)
		(setq arg-call-list 
			(cons (cons i-mem-loc (find-d-mem-space 1))
			      arg-call-list)))
	      ((member (caar wd) '(oa-low-context oa-hi-context) :test #'eq)
		(cons-lap-pass1-wd (cdar wd))))
	(setq wd (cdr wd))
	(go l)))

(defun find-d-mem-space (l)
  (prog (b p s)
  l0	(setq s #o20000)	;size of best block to split so far
	(setq p d-mem-free-blocks)
  l	(cond ((null (cdr p)) (go s))
	      ((= l (caadr p))
		(go x))
	      ((and (> (caadr p) l)
		    (< (caadr p) s))
		(setq b p)
		(setq s (caadr p))))
	(setq p (cdr p))
	(go l)
  x	(setq b (cadr p))
	(rplacd p (cddr p))
	(setq dispatch-block-limit (+ (car b) (cdr b)))
	(return (cdr b))
  s	(cond ((null b)
		(cons-lap-barf l 'out-of-d-mem 'barf)))
	(rplaca (cadr b) (ash s -1))
	(rplacd d-mem-free-blocks 
		(cons (cons (ash s -1)
			    (+ (ash s -1) (cdadr b)))
		      (cdr d-mem-free-blocks)))
	(setq b nil)
	(go l0) ))

(defun cons-lap-defsym (sym val)
  (prog (tm) 
	(cond ((setq tm (cons-lap-symeval sym))
		(cond ((not (equal val tm))
			(cons-lap-barf (list val tm) 'mult-def-sym 'data))))
	      (t (setf (get sym 'cons-lap-user-symbol) val)))
	(return nil)))

(defun cons-lap-set (sym val)
  (setf (get sym 'cons-lap-user-symbol) val))

(defun cons-lap-symeval (sym)
  (or (get sym 'cons-lap-sym) (get sym 'cons-lap-user-symbol)))

(defun cons-lap-lisp-symeval (sym)
  (or (boundp sym) (error "unbound lisp variable ~S" sym))
  (symbol-value sym))

(defun def-data-field (sym bits bits-over)
  (prog ()
	(cons-lap-defsym sym 
	  (list 'm-mem (list 'byte-field bits bits-over))) 
	(return nil)))

(defun def-bit-field-in-reg (sym bits bits-over reg)
  (prog ()
	(cons-lap-defsym sym 
	  (list '+ 
		(list 'byte-field bits bits-over)
		reg))
	(return nil)))


(defun reset-bit-pointer (sym)
  (setf (get sym 'cons-lap-b-ptr) 0))

(defun def-next-field (sym bits in-sym)
  (prog (b-ptr in-sym-v n-b-ptr)
	(cond ((not (atom in-sym))
		(cons-lap-barf in-sym 'bad-next-field 'data)
		(return nil)))
	(setq b-ptr (cond ((get in-sym 'cons-lap-b-ptr))
			  (t '0)))
	(cond ((null (setq in-sym-v (cons-lap-symeval in-sym)))
		(cons-lap-barf in-sym 'undef-in-def-next-field 'data)
		(return nil)))
	(cond ((> (setq n-b-ptr (+ bits b-ptr)) 32.)
		(cons-lap-barf in-sym 'out-of-bits 'data)
		(return nil)))
	(cons-lap-defsym sym (list '+ (list 'byte-field bits b-ptr)
				   in-sym-v))
	(setf (get in-sym 'cons-lap-b-ptr) n-b-ptr)
))

(defun cons-lap-pass2 (wd)
  (prog (v)
	(cond ((atom wd)
	       (setq cons-lap-last-sym wd)
	       (setq cons-lap-wds-since-last-sym 0)
	       (cond ((and dispatch-arm 
			   (eq locality 'd-mem))
		      (setq d-mem-loc (ldb (byte 11 12) (cons-lap-arg-eval wd)))
		      (setq dispatch-arm nil))
		     ((not (equal 
			    (cons-lap-symeval wd)
			    (list locality 
				  (cons 'field 
					(cond ((eq locality 'i-mem)
					       (list 'jump-address-multiplier i-mem-loc))
					      ((eq locality 'a-mem) 
					       (list 'a-source-multiplier a-mem-loc))
					      ((eq locality 'm-mem) 
					       (list 'm-source-multiplier m-mem-loc))
					      ((eq locality 'd-mem) 
					       (list 'dispatch-address-multiplier d-mem-loc))
					      (t (cons-lap-barf locality 
								'bad-locality 
								'barf))) )) ))
		      (cons-lap-barf wd 'def-dfrs-on-pass2 'barf))))
	      ((member (car wd) '(def-data-field assign assign-eval
				   def-next-bit reset-bit-pointer
				   def-next-field end-dispatch 
				   def-bit-field-in-reg) :test #'eq))
	      ((eq (car wd) 'locality)
	       (setq locality (cadr wd)))
	      ((eq (car wd) 'start-dispatch)
	       (setq dispatch-constant (cond ((cons-lap-arg-eval (caddr wd)))
					     (t 0)))
	       (setq dispatch-arm t))	;set d-mem-loc to next d-mem symbol encountered
	      				;error if storage word before that.
	      ((member (car wd) '(loc modulo) :test #'eq)
	       (cons-lap-loc-modulo wd))
	      ((eq (car wd) 'repeat)
	       (cons-lap-repeat-2 (cons-lap-arg-eval (cadr wd))
				  (cddr wd)))
	      ((eq (car wd) 'misc-inst-entry)
	       (let ((opcode (get (cadr wd) 'qlval)))
		 (cond ((null opcode)
			(cons-lap-barf (cadr wd) 'no-ucode-entry-index 'warn))
		       (t
			 (setq current-assembly-highest-misc-entry
			       (max opcode current-assembly-highest-misc-entry))
			 (cond ((null cons-lap-init-state)
				(setf (aref micro-code-symbol-image (- opcode #o200)) i-mem-loc))
			       (t (setq current-assembly-micro-entries;in incremental assembly
					(cons (list 'misc-inst-entry (cadr wd) i-mem-loc)
					      current-assembly-micro-entries))))))))
	      ((eq (car wd) 'micro-code-illegal-entry-here)
	       (setq micro-code-symbol-table-fill-value i-mem-loc)
	       (cons-lap-wipe-symbol-vector i-mem-loc))
	      ((and (eq (car wd) 'mc-linkage)
		    (listp (cadr wd)))
	       (mapc (function cons-lap-mc-linkage-store) (cadr wd)))
	      ((eq (car wd) 'error-table)
	       (setq current-assembly-table
		     (nconc current-assembly-table
			    (list (cons (1- i-mem-loc) (cdr wd))))))
	      ((eq (car wd) 'comment))
	      ((eq (car wd) 'if)
	       (cond ((eval (cadr wd))
		      (cons-lap-pass2 (caddr wd)))
		     (t (mapc (function cons-lap-pass2) (cdddr wd)))))
	      (t (go w1)))
     x	(return nil)
     w1	(setq cons-lap-wds-since-last-sym (1+ cons-lap-wds-since-last-sym))
	(cond (dispatch-arm 
	       (cons-lap-barf wd 'storage-wd-in-unlocated-dispatch-block 'data)))
	(setq v (cons-word-eval wd))
	(cond ((eq locality 'a-mem)
	       (cond ((>= a-mem-loc #o40)
		      (setf (aref a-mem a-mem-loc) v)))	;this range is really m-mem
	       (setq a-mem-loc (1+ a-mem-loc)))
	      ((eq locality 'm-mem)
	       (cond ((< m-mem-loc #o40)
		      (setf (aref a-mem m-mem-loc) v))
		     (t (cons-lap-barf m-mem-loc 'm-mem-overflow 'data)))
	       (setq m-mem-loc (1+ m-mem-loc)))
	      ((eq locality 'd-mem)
	       (setq v (+ v dispatch-constant))	;constant for entire block
	       (setq v (+ (ash (ldb (byte 3 7) v) 14)	;rpn bits from jump
			  (ldb (byte 14 12) v)))		;pc from jump
	       (setf (aref d-mem d-mem-loc) v)
	       (setq d-mem-loc (1+ d-mem-loc)))
	      ((eq locality 'i-mem)
	       (setf (aref i-mem i-mem-loc) v)
	       (setq i-mem-loc (1+ i-mem-loc)))
	      (t (cons-lap-barf wd 'storage-wd-in-bad-locality 'data)))
	(return nil)
	))

;add symbol to MC-LINKAGE-ALIST
(defun cons-lap-mc-linkage-store (elem)
  (prog (mc-sym conslp-sym val tem type)
	(cond ((atom elem)
	       (setq mc-sym elem conslp-sym elem))
	      (t (setq mc-sym (car elem) conslp-sym (cadr elem))))
  	(setq val (get conslp-sym 'cons-lap-user-symbol))
    l	(cond ((null val) (return nil))
	      ((numberp val))
	      ((atom val)
		(setq val (cons-lap-symeval val))
		(setq type 'n)
		(go l))
             ((and (setq tem (assoc (car val) 
			'( (i-mem jump-address-multiplier i)
                           (d-mem dispatch-address-multiplier d)
                           (a-mem a-source-multiplier a)
                           (m-mem m-source-multiplier m)) :test #'eq))
                   (eq (caadr val) 'field)
                   (eq (cadadr val) (cadr tem)))
              (setq val (caddr (cadr val)))
	      (setq type (caddr tem)))
	     (t (return nil)))
        (setq mc-linkage-alist (cons (list mc-sym type val) mc-linkage-alist))
	))



;define MC-LINKAGE symbol as regular symbol
(defun cons-lap-define-linkage-symbol (symbol)
  (cons-lap-defsym symbol (cons-lap-mc-linkage symbol)))

;(MC-LINKAGE <SYMBOL>)
(defun cons-lap-mc-linkage (symbol)
  (prog (tem v mult mem)
	(cond ((null (setq tem (assoc symbol mc-linkage-alist :test #'string-equal)))
	       (error "~%Undefined MC-LINKAGE symbol ~S" symbol)))
	(setq mem (string (cadr tem)) v (caddr tem))
	(cond ((string-equal mem "N") (go x))
	      ((setq tem (assoc mem
			      '(("I" jump-address-multiplier i-mem)
				("D" dispatch-address-multiplier d-mem)
				("A" a-source-multiplier a-mem)
				("M" m-source-multiplier m-mem))
			      :test #'string-equal))
	       (setq mult (cadr tem) mem (caddr tem)))
	      (t (error "~%Unknown memory name ~S" mem)))
	(setq v `(,mem (field ,mult ,v)))
    x   (return v)
))

;(MC-LINKAGE-VALUE <MEMORY> <SYMBOL>)
(defun cons-lap-mc-linkage-value (memory symbol)
  (prog (v mult)
	(cond ((null (setq v (assoc symbol mc-linkage-alist :test #'string-equal)))
	       (error "~%Undefined MC-LINKAGE symbol ~S" symbol)))
	(setq v (caddr v))
	(cond ((string-equal memory "NUMBER") (go x))
	      ((setq mult (assoc memory
			       '( ("I-MEM" . jump-address-multiplier)
				  ("D-MEM" . dispatch-address-multiplier)
				  ("A-MEM" . a-source-multiplier)
				  ("M-MEM" . m-source-multiplier))
			       :test #'string-equal))
	       (setq mult (cdr mult)))
	      (t (error "~%Unknown memory name ~S" memory)))
	(setq v `(field ,mult ,v))
    x   (return v)
))

(defun cons-lap-wipe-symbol-vector (quan)
  (prog (idx end-test)
	(setq idx 0)
	(setq end-test (length micro-code-symbol-image))
     l	(cond ((not (< idx end-test))
	       (return t))
	      ((null (aref micro-code-symbol-image idx))
	       (setf (aref micro-code-symbol-image idx) quan)))
	(setq idx (1+ idx))
	(go l)))

(defun cons-lap-repeat-2 (count lst)
  (prog (orpcnt rpcnt)
	(setq orpcnt (cons-lap-symeval 'repeat-count))
	(setq rpcnt 0)
     l	(cond ((zerop count)
	       (cons-lap-set 'repeat-count orpcnt)
	       (return nil)))
	(cons-lap-set 'repeat-count rpcnt)
	(mapc (function (lambda (x) (cons-lap-pass2 (cond ((atom x) (list x))
							  (t x)))))
	      lst)
	(setq count (1- count))
	(setq rpcnt (1+ rpcnt))
	(go l)))

(defun cons-word-eval (wd)
  (prog (combined-value combined-indicators destination-context 
			instruction-context field-indicators field-value tem tem1 tem2 
			destination-indicators current-word)
	(setq combined-value 0)		;caution! combined-value can be a bignum
	(setq current-word wd)		;so can see it when stuff compiled
	(setq instruction-context 'instruction)
     l	(setq field-indicators nil)
	(cond ((null wd) (return 
			  (cons-lap-default-and-bugger 
			   instruction-context 
			   combined-value
			   combined-indicators 
			   destination-indicators)))
	      ((numberp (car wd))
	       (setq field-value (car wd)))
	      ((atom (car wd))
	       (setq field-value (cons-lap-sym-run (car wd))))
	      ((eq (caar wd) 'm-constant)
	       (setq field-value (cons-m-constant (cadar wd))))
	      ((eq (caar wd) 'a-constant)
	       (setq field-value (cons-a-constant (cadar wd))))
	      ((setq tem 
		     (assoc (caar wd) 
			   '((arg-call . #xc000) ;p-bit n-bit
			     (arg-jump . #x4000) ;n-bit
			     (arg-call-xct-next . #x2000) ;p-bit
			     (arg-jump-xct-next . #x0000)) ; none
			   :test #'eq))
	       (setq tem1 (cons-lap-arg-eval (cadar wd))) ;tag
	       (setq tem2 (assoc i-mem-loc arg-call-list))
	       (cond ((null tem2) 
		      (cons-lap-barf i-mem-loc 
				     'no-d-mem-reserved-for-arg-call 
				     'barf)))
	       (setf (aref d-mem (cdr tem2))
		      (+ (cdr tem) (ldb (byte 14 12) tem1)))
	       (cons-get-new-context 'force-dispatch)
	       (add-field-indicators 'd-mem)
	       (setq field-value (* (cdr tem2) (ash 1 12))))
	      ((member (caar wd) '(byte-field lisp-byte all-but-lisp-byte 
					    field byte-mask byte-value plus difference 
					    oa-high-context oa-low-context eval i-arg
					    i-mem-loc d-mem-loc a-mem-loc m-mem-loc
					    mc-linkage mc-linkage-value
					    mc-entry-adr misc-entry-adr)
		       :test #'eq)
	       (setq field-value (cons-lap-eval (car wd))))
	      (t
	       (cons-get-new-context 'force-alu-or-byte)
	       (setq field-value (cons-destination (car wd)))
	       
	       (setq field-value 
		     (convert-value-to-destination field-value field-indicators))
	       (setq destination-indicators field-indicators)
	       (setq field-indicators nil)) )
	(setq combined-value (+ combined-value field-value))
;	(print (list (car wd) field-value field-indicators))
	(setq combined-indicators (merge-indicators 
				   field-indicators combined-indicators))
	(setq wd (cdr wd))
	(go l)
	))

(defun cons-lap-default-and-bugger 
  (instruction-context combined-value combined-indicators destination-indicators)
  (prog (t1 t2 inst)
	;;	(print (list instruction-context 
	;;		     combined-value 
	;;		     combined-indicators 
	;;		     destination-indicators))
	(cond ((not (eq locality 'i-mem))
	       (go x))
	      ((member instruction-context '(force-alu force-alu-or-byte
						       instruction)
		       :test #'eq)
	       (go alu))
	      ((eq instruction-context 'force-dispatch)
	       (go dispatch))
	      ((eq instruction-context 'force-byte)
	       (go byte))
	      ((eq instruction-context 'force-jump)
	       (go jump))
	      (t (cons-lap-barf (list instruction-context 
				      combined-value combined-indicators
				      destination-indicators)
				'bad-instruction-type
				'warn)
		 (go x)))
	alu (cond ((null (member 'alu-output-bus-selector-multiplier ;default output bus
			       combined-indicators :test #'eq)) ;selector if not specd
		   (setq combined-value (+ combined-value (ash 1 12)))))
	(cond ((member 'alu-op combined-indicators :test #'eq)
	       (go alu-1)))
	(setq t1 (member 'a-mem combined-indicators :test #'eq)) ;default alu op if not
	(setq t2 (memql '(m-mem function-source) combined-indicators)) ;specd
	(cond ((and t1 t2)	   ;(alu must be acting as a selector)
	       (cons-lap-barf combined-indicators 
			      'alu-inst-adrs-a-and-m-without-alu-op 
			      'warn))
	      (t1 (setq combined-value 
			(+ combined-value (ash 5 3)))) ;seta
	      (t2 (setq combined-value 
			(+ combined-value (ash 3 3)))) ;setm
	      (t  (setq combined-value 
			(+ combined-value (ash 0 3))))) ;neither specd? setz i guess
	alu-1
	(go x)
	byte
	(cond ((null (member 'a-mem combined-indicators :test #'eq))	;default a-mem adr to
	       (setq combined-value	;a-zero if not supplied,
		     (+ combined-value (ash 2 32))))) ;this right for both ldb and dpb
	(setq inst #o600000000000000)	;byte inst
	(setq t1 (ldb (byte 1 12) combined-value)) ;get sr-bit
	(setq combined-value (dpb (- 1 t1) ;store it back complemented
				  (byte 12 1) combined-value))
	(cond ((> (ldb (byte 2 12) combined-value) 1)
	       (go x1)))		;dont bugger dpb or sel depos
	m-rotate-bugger			;32. reflect m-rotate field
	(setq t1 (logand #o6037 combined-value)) ;gobble misc fctn
					;and m-rotate
	m-rotate-bugger-1
	(setq t1 (logand #o37 t1))
	(setq t2 (logand #o37 (- #o40 t1)))
	(setq combined-value (+ combined-value (- t2 t1)))
	x1	(setq combined-value (+ combined-value inst))
	x	(return combined-value)
	dispatch 
	(setq inst #o1400000000000000) ;dispatch instruction plus i-long
	;;(setq inst 400000000000000)	;just dispatch instruction
	(go m-rotate-bugger)
	jump 
	(setq inst #o200000000000000)
	(setq t1 (logand #o6077 combined-value))
	(cond ((> (logand t1 #o77) #o37) (go x1))) ;test-condition, dont hack
	(go m-rotate-bugger-1)		;randomly save a bignum op
	))

;;CONSTANT LISTS.
;;A LIST OF LISTS.  CAR IS VALUE OF CONSTANT, CADR IS ADDRESS, CADDR IS
;; #USERS, CADDDR IS LAST PC TO USE IT.

(defun cons-m-constant (c)
  (prog (tem v)
	(setq v (cons-lap-arg-eval c))
	(cond ((= v 0) 
		(setq tem 2))	;m locn 2 always has 0
	      ((or (= v #o37777777777) (= v -1))
	       (setq tem 3))	;m locn 3 always has -1 (to 32 bits)
	      ((setq tem (assoc v m-constant-list))
	        (rplaca (cddr tem) (1+ (caddr tem)))
		(rplaca (cdddr tem) cons-lap-last-sym)
		(setq tem (cadr tem)))
	      (t
		(setq tem m-constant-loc m-constant-loc (1+ m-constant-loc))
		(setq m-constant-list (cons (list v tem 1 cons-lap-last-sym) m-constant-list))))
	(or (< tem #o40) (cons-lap-barf (list tem c) 'm-const-addr-oob 'barf))
	(add-field-indicators 'm-mem)
	(return (dpb tem (byte 5 26) 0)) ))

(defun cons-a-constant (c)
  (prog (tem v)
	(setq v (cons-lap-arg-eval c))
	(cond ((= v 0) 
		(setq tem 2))	;a locn 2 always has 0
	      ((or (= v #o37777777777) (= v -1))
	       (setq tem 3))	;a locn 3 always has -1 (to 32 bits)
	      ((setq tem (assoc v a-constant-list))
	        (rplaca (cddr tem) (1+ (caddr tem)))
		(rplaca (cdddr tem) cons-lap-last-sym)
		(setq tem (cadr tem)))
	      ((setq tem (assoc v m-constant-list))	;a=m!!
	        (rplaca (cddr tem) (1+ (caddr tem)))
		(rplaca (cdddr tem) cons-lap-last-sym)
		(setq tem (cadr tem)))
	      ((not (null a-mem-crevice-list))	;try to fill in crevices in memory
		(setq tem (car a-mem-crevice-list))
		(setq a-mem-crevice-list (cdr a-mem-crevice-list))
		(setq a-constant-list (cons (list v tem 1 cons-lap-last-sym) a-constant-list)))
	      (t
		(setq tem a-constant-loc a-constant-loc (1+ a-constant-loc))
		(setq a-constant-list (cons (list v tem 1 cons-lap-last-sym)
					    a-constant-list))))
	(or (< tem #o2000) (cons-lap-barf (list tem c) 'a-const-addr-oob 'barf))
	(add-field-indicators 'a-mem)
	(return (dpb tem (byte 10 32) 0)) ))

(defun convert-value-to-destination (value indicators)
  (prog (v)
	(setq v (ldb (byte 10 0) value))	;gobble byte info, if any (hope hope)
	(cond ((member 'a-mem indicators :test #'eq)
	       (cond ((memql '(m-mem function-destination) indicators)
		      (cons-lap-barf (list value indicators) 'bad-destination 'data)))
	       (setq v (+ v (dpb (ldb (byte 14 32) value) (byte 10 14) 0))))
	      ((member 'm-mem indicators :test #'eq)
	       (setq v (+ v (dpb (ldb (byte 6 26) value) (byte 6 14) 0)))))
	(cond ((member 'function-destination indicators :test #'eq)
	       (setq v (+ v (logand (ash 37 19) value)))))
	(cond ((memql '(a-mem) indicators)
	       (setq v (+ v (ash 1 25)))))
	(return v)
))

(defun merge-indicators (a b)
  (prog nil 
	(cond ((null b) (return a)))
  l	(cond ((null a) (return b))
	      ((not (member (car a) b :test #'eq))
		(setq b (cons (car a) b))))
	(setq a (cdr a))
	(go l)))

(defun cons-destination (x)
  (prog (destination-context v)
	(setq v 0)
	(setq destination-context 'destination)
	(cond ((null (cdr x))	;save a plus in common case..
		(return (cons-lap-sym-run (car x)))))
l	(cond ((null x) (return v)))
	(setq v (+ v (cons-lap-sym-run (car x))))
	(setq x (cdr x))
	(go l)
))

(defun cons-lap-sym-run (sym)
  (prog (tem)
	(cond ((null (setq tem (cons-lap-symeval sym)))
	       (cons-lap-barf sym 'undefined-sym 'warn)
	       (return 0))
	      (t (return (cons-lap-eval tem))))))

(defun cons-lap-arg-eval (arg)
  (prog (combined-value combined-indicators destination-context 
         instruction-context field-indicators)
	(setq instruction-context 'instruction)
	(return (cons-lap-eval arg))))

(defun cons-lap-eval (exp)      ;exp a symbol "program".
				;returns either a numberic value or nil, and
				;may have the side effect of modifing 
				;instruction-context and/or field-indicators


  (prog (val v v1 v2 tem)
l	(cond ((null exp) (go x))
	      ((numberp exp)
	       (setq v exp)
	       (go c-v))
	      ((atom exp) 
	       (setq v (cons-lap-sym-run exp))
	       (go c-v))
	      ((member (car exp) '(a-mem m-mem i-mem d-mem) :test #'eq)
	       (go l2))
	      ((eq (car exp) 'source-p) (go s-p))
	      ((eq (car exp) 'destination-p) (go d-p))
	      ((member (car exp) '(force-dispatch force-jump
						  force-alu force-byte
						  force-dispatch-or-byte
						  force-alu-or-byte)
		       :test #'eq)
	       (cons-get-new-context (car exp))
	       (go l2))
	      ((setq tem (assoc (car exp) '((dispatch-instruction-p . force-dispatch)
					    (byte-instruction-p . force-byte) (jump-instruction-p . force-jump)
					    (alu-instruction-p . force-alu)) :test #'eq))
	       (go i-p))
	      ((eq (car exp) 'not)
	       (go n1))
	      ((eq (car exp) 'or)
	       (go or-1))
	      ((setq v (assoc (car exp)
			      '((i-mem-loc . i-mem) (d-mem-loc . d-mem)
				(a-mem-loc . a-mem) (m-mem-loc . m-mem))
			      :test #'eq))
	       (setq tem (cons-lap-symeval (cadr exp)))
	       (or (eq (car tem) (cdr v))
		   (cons-lap-barf exp 'loses 'data))
	       (setq v (caddr (cadr tem)))
	       (go c-v))
	      ((eq (car exp) 'field)
		(setq tem (cons-lap-sym-run (cadr exp)))
		(setq v (* (cons-lap-eval (caddr exp)) tem))
		(cond ((setq tem (get (cadr exp) 'cons-lap-additive-constant))
			(setq v (+ v tem))))
		(add-field-indicators (cadr exp))
		(go c-v))
	      ((eq (car exp) 'plus)
	       (setq v (cons-lap-eval (cadr exp)))
	       (do ((l (cddr exp) (cdr l)))
		   ((null l))
		   (setq v (+ v (cons-lap-eval (car l)))))
	       (go c-v))
	      ((eq (car exp) 'difference)
		(setq v (- (cons-lap-eval (cadr exp))
				    (cons-lap-eval (caddr exp))))
		(go c-v))
	      ((eq (car exp) 'byte-field)
		(cond ((member instruction-context '(instruction force-dispatch-or-byte 
							force-alu-or-byte)
			       :test #'eq)
			(cons-get-new-context 'force-byte)))
		(setq v1 (cons-lap-eval (cadr exp)) v2 (cons-lap-eval (caddr exp)))
		(cond ((eq instruction-context 'force-byte)
		       (and (> v1 32.) (cons-lap-barf (cadr exp)
						      'byte-size-greater-than-32
						      'data))
		       (and (zerop v1) (setq v1 1))	;byte size 0, doing oa hackery, use 1-1
		       (setq v (+ (* #x20 (1- v1)) v2))) ;1- byte size, mrot not buggered yet
		      ((eq instruction-context 'force-dispatch)
			(and (> v1 7) (cons-lap-barf (cadr exp)
						     'dispatch-byte-size-greater-than-7
						     'data))
			(setq v (+ (* #x20 v1) v2)))
		      ((eq instruction-context 'force-jump)
			(cond ((not (= 1 v1))
				(cons-lap-barf (cadr exp) 
						'can-only-test-one-bit-field-with-jump 
						 'data)))
			(setq v v2))
		      (t (cons-lap-barf instruction-context 
					'byte-field-in-bad-context 
					'data)))
		(go c-v))
	      ((eq (car exp) 'lisp-byte)
		(setq v (cons-lap-eval (convert-lisp-byte (cadr exp))))
		(go c-v))
	      ((eq (car exp) 'all-but-lisp-byte)
		(setq v (cons-lap-eval (convert-all-but-lisp-byte (cadr exp))))
		(go c-v))
	      ((eq (car exp) 'byte-mask)
		(setq v (cons-lap-get-byte-value (cadr exp) -1))
		(go c-v))
	      ((eq (car exp) 'byte-value)
		(setq v (cons-lap-get-byte-value (cadr exp) (caddr exp)))
		(go c-v))
	      ((eq (car exp) 'eval)
		(setq v (eval (cadr exp)))
		(go c-v))
	      ((eq (car exp) 'i-arg)
		(setq v (dpb (cons-lap-eval (cadr exp))
				(byte 10 32) 0))
		(go c-v))
	      ((eq (car exp) 'oa-high-context)
		(setq v (ldb (byte 22 26) (cons-word-eval (cadr exp)))) ;all above 26. bits
		(go c-v))
	      ((eq (car exp) 'oa-low-context)
		;  (setq v (ldb (byye 26 0) (cons-word-eval (cadr exp)))) ;low 26. bits
		   (setq v (let ((tem-v (cons-word-eval (cadr exp))))  ;result of ldb cant be
			     (dpb (ldb (byte 3 23) tem-v) (byte 3 23)
				  (ldb (byte 23 0) tem-v)))) ;bignum for now.
		(go c-v))
	      ((and (eq (car exp) 'mc-linkage)
		    (symbolp (cadr exp)))
	       (setq v (cons-lap-eval (cons-lap-mc-linkage (cadr exp))))
	       (go c-v))
	      ((eq (car exp) 'mc-linkage-value)
	       (setq v (cons-lap-eval (cons-lap-mc-linkage-value (cadr exp) (caddr exp))))
	       (go c-v))
;	      ((and cons-lap-init-state		;incremental assembly
;		    (eq (car exp) 'mc-entry-adr))
;	       (cond ((not (= (%data-type
;				(setq tem (car (function-cell-location (cadr exp)))))
;			      dtp-u-entry))
;		(error "mc-entry-adr not DTP-U-ENTRY"))
;	       (setq v (cons-lap-eval
;			 `(i-mem (field jump-address-multiplier
;					,(aref micro-code-symbol-area
;					       (aref micro-code-entry-area
;						     tem))))))
;	       (go c-v))
	      ((and cons-lap-init-state		;incremental assembly
		    (eq (car exp) 'misc-entry-adr))
	       (setq v (cons-lap-eval
			 `(i-mem (field jump-address-multiplier
					,(aref micro-code-symbol-area
					       (- (get (cadr exp) 'qlval) #o200))))))
	       (go c-v))
	      (t (cons-lap-barf exp 'unrecgonized-op 'data)
		 (go x)))
or-2	(cond ((null (cdr (setq exp (cdr exp))))
		(go x)))				;all nil
or-1	(setq tem (cons-lap-eval (cadr exp)))
	(cond ((null tem) (go or-2)))	;that one evaluated to nil
merge-v	(cond ((null val) (setq val tem))
	      (t (setq val (+ val tem))))
	(go x)
n1	(setq tem (cons-lap-eval (list (caadr exp) 1)))
	(cond ((= tem 1) (go x))   ;that condition true, this false
	      (t (setq exp (cadr exp))	;that condition false, this true
		 (go l1)))
d-p	(cond (destination-context (go l1)))
	(go x)
s-p	(cond (destination-context (go x)))
	(go l1)

l2	(add-field-indicators (car exp))
l1	(setq exp (cadr exp))
	(go l)
i-p	(cond ((eq (cdr tem) instruction-context)
		(go l1))		;condition true
	      ((eq instruction-context 'instruction)
		(cons-lap-barf exp 'undetermined-condition 'warn)))
	(go x)		;condition false
c-v	(cond ((null val) (setq val 0)))
	(cond ((null v)
	       (cons-lap-barf exp 'evaluated-to-nil 'data))
	      (t (setq val (+ val v))))
x	(return val)))

(defun convert-lisp-byte (x)  ;convert lisp-byte to corresponding byte-field
  (prog (tem)
	(setq tem (eval x))
	(return (list 'byte-field (logand tem #o77)
				  (ldb (byte 6 6) tem)))))

(defun convert-all-but-lisp-byte (x)	;address all bits not in byte. byte must be
  (prog (tem bits over)			;left or right adjusted in 32. bits
	(setq tem (eval x))
	(setq bits (logand tem #o77) over (ldb (byte 6 6) tem))
	(cond ((= 0 over)
		(setq over bits)
		(setq bits (- 32. bits)))
	      ((= 32. (+ bits over))
		(setq bits (- 32. bits))
		(setq over 0))
	      (t (cons-lap-barf x 'all-but-byte-not-left-or-right-adjusted 'data)))
	(return (list 'byte-field bits over))))

(defun cons-lap-get-byte-value (exp val);"evaluate" exp similiar to cons-lap-eval
  (prog (tem)				;but return nil for anything but byte-field,
	(cond ((numberp val))		;for which return val in field of byte
	      ((not (atom val))
	       (setq val (cons-lap-arg-eval val)))
	      ((setq tem (cons-lap-symeval val))
	       (setq val tem))
	      ((setq val (cons-lap-lisp-symeval val))))
	(cond ((null exp) (return nil))
	      ((numberp exp)
		(return (cons-lap-get-byte-value (convert-lisp-byte exp) val)))
	      ((atom exp)
		(return (cons-lap-get-byte-value
			  (or (cons-lap-symeval exp) (cons-lap-lisp-symeval exp)) val)))
	      ((member (car exp) '(m-mem force-dispatch force-byte force-dispatch-or-byte 
				 force-alu-or-byte)
		       :test #'eq)
		(return (cons-lap-get-byte-value (cadr exp) val)))
	      ((member (car exp) '(a-mem i-mem d-mem source-p destination-p force-jump
				force-alu not or field eval)
		       :test #'eq)
		(return nil))
	      ((eq (car exp) 'plus)
		(return (do ((l (cdr exp) (cdr l)))
			    ((null l))
			  (and (setq tem (cons-lap-get-byte-value (car l) val))
			       (return tem)))))
	      ((eq (car exp) 'lisp-byte)
		(return (cons-lap-get-byte-value (convert-lisp-byte (cadr exp)) val)))
	      ((eq (car exp) 'byte-field)
		(return (dpb val (+ (ash (caddr exp) 6) (cadr exp)) 0)))
	      (t (cons-lap-barf exp 'cons-lap-get-byte-value 'warn)))
))

(defun add-field-indicators (x)
  (prog nil 
	(cond ((and destination-context   ;better not put in more than one of these
		    (member x '(a-mem m-mem i-mem d-mem) :test #'eq)  ;since going to divide it out.
		    (memql '(a-mem m-mem i-mem d-mem) field-indicators))
		(go e1)))
	(cond ((eq x 'a-mem)
		(go x))
	      ((eq x 'm-mem)
		(go x))
	      ((eq x 'i-mem)
		(go add-i))
	      ((eq x 'd-mem)
		(go add-d))
	      ((eq x 'force-dispatch)
		(go f-d))
	      ((eq x 'force-byte)
		(go f-b))
	      ((eq x 'force-alu)
		(go f-a))
	      ((eq x 'force-jump)
		(go f-j)))
   x	(cond ((not (member x field-indicators :test #'eq))
		(setq field-indicators (cons x field-indicators))))
	(return nil)
 f-b	(cond ((memql '(i-mem d-mem) combined-indicators)
		(go e1)))
	(go x)
 f-a	(cond ((or (member instruction-context '(force-dispatch force-jump)
			   :test #'eq)
		   (memql '(i-mem d-mem) combined-indicators))
		(go e1)))
	(go x)
 f-j 
 add-i	(cond ((member instruction-context '(force-dispatch force-byte
							    force-alu)
		       :test #'eq)
		(go e1)))
	(go x)
 f-d 
 add-d	(cond ((or (member instruction-context '(force-jump force-byte
							    force-alu)
			   :test #'eq)
		   (memql '(i-mem) combined-indicators))  ;a-mem ok now if writing dram
		(go e1)))
	(go x)
  e1	(cons-lap-barf (list x field-indicators combined-indicators)
	      'indicator-conflict 
	      'data)
	(return nil)
))

(defun memql (a b)
  (prog nil 
l	(cond ((null a) (return nil))
	      ((member (car a) b :test #'eq) (return a)))
	(setq a (cdr a))
	(go l)))

(defun cons-get-new-context (new-context)
  (prog nil
	(cond ((atom new-context) 
		(return (cons-get-new-context-1 new-context))))
l	(cond ((null new-context) (return t))
	      (t (cons-get-new-context-1 (car new-context))))
	(setq new-context (cdr new-context))
	(go l)))

(defun cons-get-new-context-1 (new)
  (prog nil 
	(cond ((or (eq instruction-context new)
		   (not (member new '(force-dispatch force-jump force-alu
						     force-byte 
						     force-dispatch-or-byte
						     force-alu-or-byte)
				:test #'eq)))
		(return nil))
	      ((eq instruction-context 'instruction)
		(go n1))
	      ((and (eq instruction-context 'force-byte)
		    (member new '(force-dispatch-or-byte force-alu-or-byte)
			    :test #'eq))
		(return nil))
	      ((and (eq instruction-context 'force-alu)
		    (eq new 'force-alu-or-byte))
		(return nil))
	      ((and (eq new 'force-byte)
		    (member instruction-context
			  '(force-dispatch-or-byte force-alu-or-byte)
			  :test #'eq))
		(go n1))
	      ((and (eq new 'force-alu)
		    (eq instruction-context 'force-alu-or-byte))
		(go n1))
	      ((or (and (eq instruction-context 'force-dispatch-or-byte)
			(eq new 'force-alu-or-byte))
		   (and (eq new 'force-alu-or-byte)
			(eq instruction-context 'force-dispatch-or-byte)))
		(setq new 'force-byte)
		(go n1)))
	(cons-lap-barf (list instruction-context new) 'conflicting-context 'data)
	(return nil)
  n1	(setq instruction-context new)
	(return t)
))

