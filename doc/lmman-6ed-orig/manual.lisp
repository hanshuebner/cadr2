; -*- Mode:LISP; Base:8 -*-
(caseq (status site)
  (OZ (defprop lmman (src l/.man) ppn)
      (defprop lmwind (src l/.wind) ppn))
  (AI (defprop lmman (dsk lmman) ppn)
      (defprop lmwind (dsk lmwind) ppn)))

(include ((#+TENEX dsk bolio) macros lisp))

(DECLARE (SPECIAL LEFT-INDENT ENVIRONMENT-TYPE
		  EXTRA-LEFT-INDENT-FIRST-LINE-OF-PARAGRAPH
		  METER-NAME JIN-CUR-FONT
		  ITEM-KINDEX-FLAG REQUEST-EOL-P
		  FUNCTION-INDEX VARIABLE-INDEX CONDITION-INDEX
		  MESSAGE-INDEX FLAVOR-INDEX))

(default-manual-style t '|Lisp Machine Manual| 'DOVER 10.)
(setq alarm-vpos 9900.)		;Temporary until Bolio 153

; This is the order I want the indices to come out in
(setq sorted-trailing-listings
      '(concept-index flavor-index message-index keyword-index
	initoption-index meter-index variable-index function-index))
(setq print-summary-table-of-contents t)
(setq index-page-number-alignment ':flush-left)

(deflisting concept-index
  (:type :index
     (:package-prefix-mode :normal)
     (:leading-between-initial-letters 150.))
  (:columns 2))

(deflisting function-index
  (:type :index
	 (:package-prefix-mode :twice)
	 (:leading-between-initial-letters 150.)))

(deflisting variable-index
  (:type :index (:package-prefix-mode :twice)
	 (:leading-between-initial-letters 150.))
  (:columns 2))

(deflisting keyword-index
  (:type :index (:package-prefix-mode :by-pname)
	 (:leading-between-initial-letters 150.))
  (:columns 2))

(deflisting message-index
  (:type :index (:package-prefix-mode :by-pname)
	 (:leading-between-initial-letters 150.))
  (:title |Operation Index|))

(deflisting initoption-index
  (:type :index (:leading-between-initial-letters 150.))
  (:title |Object Creation Options|))

(deflisting flavor-index
  (:type :index )
  (:title |Flavor Index|))

(deflisting condition-index
  (:title |Condition Name Index|)
  (:type :index)
  (:columns 2)
  (:request condition_index)
  (:request (condition_indexf nil t)))


;Hyphenated synonyms
(defprop defcondition-flavor defcondition-flavor-request request)
(defprop end_defcondition-flavor end-defcondition-flavor-request request)
(defprop defflavor-condition defcondition-flavor-request request)
(defprop end_defflavor-condition end-defcondition-flavor-request request)

(setq barf-stop-p nil)			;Don't enter breakpoints on "line too long", etc
(SETQ INHIBIT-AUTOMAGIC-SECOND-PASS 'ASK)

;Control the style of what .defspec and .defmac do
(SETQ SPECIAL-FORM-MARKER-EXDENTATION 1000.)	;1 inch in from right margin
(SETQ SPECIAL-FORM-MARKER-SPACE 750.)		;At least 3/4 inch between it and template
(SETQ SPECIAL-FORM-MARKER-FONT 2)		;Put it in italics I guess

;Meters are documented like Variables

(deflisting meter-index
  (:title |Meter Index|)
  (:columns 2)
  (:type :index))


(DEFPROP DEFMETER DEFMETER-REQUEST REQUEST)
(DEFPROP END_DEFMETER END-DEFMETER-REQUEST REQUEST)

(DEFUN DEFMETER-REQUEST ()
  (CHECK-ENV 'TEXT 'DEFMETER)
  (OR (NEED-SPACE-MILLS 1000.)	;1 INCH
      (OUTPUT-LEADING-MILLS DEFUN-PRE-LEADING))
  ((LAMBDA (LEFT-INDENT ENVIRONMENT-TYPE EXTRA-LEFT-INDENT-FIRST-LINE-OF-PARAGRAPH)
     (defmeter1-request)
     (DEFUN-HORRIBLE-TAB-CROCK)
     (*CATCH 'DEFMETER (MAIN-LOOP)))
   (CONVERT-MILLS 500.) ;1/2 INCH INDENT
   'DEFMETER
   0))

(defprop defmeter1 defmeter1-request request)

(defun defmeter1-request ()
  (CHECK-FONT-STATUS 1)
  (SETQ CUR-HPOS 0)
  ((LAMBDA (METER-NAME JIN-CUR-FONT)
      (OR METER-NAME (BARF '|Meter name missing in .defmeter|))
      (ADD-TO-INDEX METER-NAME 'METER-INDEX)
      (AUTO-SETQ METER-NAME '|METER|)
      (SET-HPOS LEFT-MARGIN)
      (PUT-STRING-FLUSH-LEFT METER-NAME)
      (JOUT-WHITE-SPACE (CONVERT-MILLS 200.))
      (SETQ JIN-CUR-FONT italic-font)
      (put-defspec-remark '|Meter|)
      ;(PUT-STRING-FLUSH-LEFT (STRING '|Meter|))
      (SETQ JIN-CUR-FONT 1)
      (LINE-ADVANCE)
      (SETQ BEGIN-NEW-PARAGRAPH NIL))
   (GET-WORD-STRING)
   8))

(DEFUN END-DEFMETER-REQUEST ()
  (CHECK-ENV 'DEFMETER 'END_DEFMETER)
  (CHECK-FONT-STATUS 1)
  (*THROW 'DEFMETER NIL))

(defvar newline-string (to-string (list #\cr #\lf)))

;.xitem item classify-under puts item in the index with classify-under as supertopic.
(defun (xitem request) ()
  (let ((item (get-word-string)) (item-kindex-flag (get-word-string)))
    (flush-request-line)			; Become consistent
    (let ((request-eol-p nil))
      (jin-push newline-string)			; push a new end-of-line
      (jin-push item)				; Put back the item
      (item-request))))

(defun (xitem1 request) ()
  (let ((item (get-word-string)) (item-kindex-flag (get-word-string)))
    (flush-request-line)
    (let ((request-eol-p nil))
      (jin-push newline-string)
      (jin-push item)
      (item1-request))))


;For examples

(DEFUN (DEFUN_NO_INDEX REQUEST) ()
  (LET ((FUNCTION-INDEX NIL))
    (DEFUN-REQUEST)))

(DEFUN (DEFSPEC_NO_INDEX REQUEST) ()
  (LET ((FUNCTION-INDEX NIL))
    (DEFSPEC-REQUEST)))

(DEFUN (DEFMAC_NO_INDEX REQUEST) ()
  (LET ((FUNCTION-INDEX NIL))
    (DEFMAC-REQUEST)))

(DEFUN (DEFVAR_NO_INDEX REQUEST) ()
  (LET ((VARIABLE-INDEX NIL))
    (DEFVAR-REQUEST)))

(DEFUN (DEFMETHOD_NO_INDEX REQUEST) ()
  (LET ((MESSAGE-INDEX NIL))
    (DEFMETHOD-REQUEST)))

(DEFUN (DEFCONDITION_NO_INDEX REQUEST) ()
  (LET ((CONDITION-INDEX NIL))
    (DEFCONDITION-REQUEST)))

(defun defcondition-flavor-no-index-request ()
  (let ((condition-index nil) (flavor-index nil))
    (defcondition-flavor-request)))

(defprop defcondition_flavor_no_index defcondition-flavor-no-index-request
	 request)
(defprop defcondition-flavor_no_index defcondition-flavor-no-index-request
	 request)

(defprop defconst defconst-request request)
(defprop defconst1 defconst1-request request)
(defprop end_defconst end-defconst-request request)

(DEFUN DEFCONST-REQUEST ()
  (CHECK-ENV 'TEXT 'DEFCONST)
  (OR (NEED-SPACE-MILLS 1000.) ;1 inch
      (OUTPUT-LEADING-MILLS DEFUN-PRE-LEADING))
  (CHECK-FONT-STATUS TEXT-FONT)
  ((LAMBDA (LEFT-INDENT ENVIRONMENT-TYPE EXTRA-LEFT-INDENT-FIRST-LINE-OF-PARAGRAPH)
      (DEFCONST1-REQUEST)  ;Gobble the arguments, put out line, index, etc.
      (FLUSH-REQUEST-LINE)
      (DEFUN-HORRIBLE-TAB-CROCK)
      (*CATCH 'DEFCONST (MAIN-LOOP)))
   (CONVERT-MILLS 500.) ;1/2 inch indent
   'DEFCONST
   0))

(DEFUN DEFCONST1-REQUEST ()
  (CHECK-ENV 'DEFCONST 'DEFCONST1)
  (SETQ CUR-HPOS 0)
  (let ((variable-name (get-word-string))
	(other-randomness (get-line-string))
	(jin-cur-font lisp-title-font))
    (or variable-name (barf '|Constant name missing in .DEFCONST or .DEFCONST1|))
    (ADD-TO-LISTING VARIABLE-NAME 'VARIABLE-INDEX)
    (AUTO-SETQ VARIABLE-NAME '|var|)
    (SET-HPOS LEFT-MARGIN)
    (PUT-STRING-FLUSH-LEFT VARIABLE-NAME)
    (cond ((plusp (string-length other-randomness))
	     (setq jin-cur-font text-font)
	     (output-nofill-string (just-string '|  |))
	     (output-nofill-string other-randomness)))
    (put-defspec-remark '|Constant|)
    (LINE-ADVANCE)
    (SETQ BEGIN-NEW-PARAGRAPH NIL)))

(DEFUN END-DEFCONST-REQUEST ()
  (end-documentation-block 'defconst 'end_defconst text-font))

;Characters that make it difficult to edit the file on the Lisp machine
(PUTPROP 'CTL-M (TO-STRING '(21 15)) 'JUST-VALUE)	;^Q so JIN-MORE doesn't discard
(PUTPROP 'CTL-H (TO-STRING '(21 10)) 'JUST-VALUE)
(PUTPROP 'CTL-QM (TO-STRING '(177)) 'JUST-VALUE)

(DEFUN (SAVE-CUMULATIVE-STATE REQUEST) ()
  (SAVE-CUMULATIVE-STATE T))

(DEFUN (SAVE-CUMULATIVE-STATE-NOVARS REQUEST) ()
  (SAVE-CUMULATIVE-STATE NIL))

(DEFUN SAVE-CUMULATIVE-STATE (WRITE-VARS-FLAG)
  ;; Record in CUMULATIVE.VARS all variables defined so far,
  ;; all data accumulated for indices and tables,
  ;; and the page number.
  (LET ((FILE (OPEN "CUMULATIVE.VARS" '(OUT ASCII BLOCK))))
    (COND (WRITE-VARS-FLAG
	   (PRINT `(SETQ ALL-THE-VARIABLES ',ALL-THE-VARIABLES) FILE)
	   (LOOP FOR X IN ALL-THE-VARIABLES
		 DO (PRINT (LIST 'DEFPROP X
				 (make-symbol (GET X 'JUST-VALUE))
				 'JUST-VALUE)
			   FILE)
		 (REMPROP X 'JUST-VALUE))))
    (LOOP FOR X IN (APPEND ALL-TRAILING-LISTINGS ALL-PREFATORY-LISTINGS)
	  WHEN X
	  DO
	  (LET ((TEM (SYMEVAL X)))
	    (SET X NIL)  ;Free up space
	    (LOOP FOR ELT IN TEM
		  WHEN (LISTP ELT)
		  DO (RPLACA ELT (STRING-INTERN-NESTED (CAR ELT))))
	    (PRINT `(SETQ ,X ',TEM) FILE)))
    (LOOP FOR FROB IN SECTIONIZATION-DEFINITIONS
	  FOR I FROM 0
	  FOR VAR = (SECTION-DEF-COUNTER-VARIABLE FROB)
	  DO (PRINT `(SETQ ,VAR ',(SYMEVAL VAR)) FILE))
    (PRINT '(NEXT-PAGE) FILE)
    (PRINT `(RESET-PAGE-NUMBER ,(1+ PAGE-NUMBER)) FILE)
    (FUNCALL (GET 'PAGE 'REQUEST))
    (TERPRI FILE)
    (CLOSE FILE))
  ;; Don't bother to output any indices or tables.
  (SETQ SORTED-TRAILING-LISTINGS NIL
	SORTED-PREFATORY-LISTINGS NIL
	ALL-TRAILING-LISTINGS NIL
	ALL-PREFATORY-LISTINGS NIL)
  ;; Prevent the VARS file from being written.
  (SETQ VARIABLES-FILENAME NIL
	ALL-THE-VARIABLES NIL))

(DEFUN STRING-INTERN-NESTED (X)
  (COND ((LISTP X)
	 (LOOP FOR Y ON X DO (RPLACA Y (STRING-INTERN-NESTED (CAR Y))))
	 X)
	(T (MAKE-SYMBOL X))))

(DEFUN FLUSHSYM (SYM)
  (OR (PLIST SYM) (BOUNDP SYM) (REMOB SYM)))

(DEFUN (RESTORE-CUMULATIVE-STATE REQUEST) ()
  (LOAD "CUMULATIVE.VARS")
  (LOOP FOR X IN (APPEND ALL-TRAILING-LISTINGS ALL-PREFATORY-LISTINGS)
	WHEN X
	DO
	(LOOP FOR ELT IN (SYMEVAL X)
	      WHEN (LISTP ELT)
	      DO (RPLACA ELT (STRINGIFY (CAR ELT))))))

(DEFUN (READ-OLD-VARS REQUEST) ()
  (LOAD "MANUAL.VARS"))

(DEFUN (SAVE-VARS-FILE REQUEST) ()
  (SAVE-VARIABLES "MANUAL.VARS"))

(DEFUN STRINGIFY (X)
  (COND ((LISTP X)
	 (LOOP FOR Y ON X DO (RPLACA Y (STRINGIFY (CAR Y))))
	 X)
	(T (FLUSHSYM X)
	   (STRING X))))

;For LOOP chapter
(LOAD "SRC:<L.MAN>LOOPTM.LISPM")

(SSTATUS UUOLINKS)