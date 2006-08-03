;; -*- Lisp -*-

;; Bolio to XML converter, shoe horned to convert the 6th release Lisp
;; Machine Manual to a modern format suitable to online browsing,
;; printing and eventually editing.

(in-package :convert-man)

(enable-interpol-syntax)

;; Debugging

(defvar *suppress-warnings* nil)
(defvar *debug* nil)

;; Configuration

(defvar *input-directory* #p"/home/hans/cadr2/doc/lmman-6ed-orig/")
(defvar *output-directory* #p"/home/hans/cadr2/doc/lmman-6ed-xml/")

;; Document constants

(defvar *manual-filenames* )

(defvar *manual-filenames*
  '(title intro fd-dtp fd-eva fd-flo fd-con resour fd-sym fd-num
    fd-arr generic fd-str fd-fun fd-clo fd-sg fd-loc fd-sub areas compil
    macros looptm defstr flavor ios rdprt pathnm files chaos packd maksys
    patch proces errors code query init time fd-hac))

(defvar *unicode-cp0-chars* #(#\DOT_OPERATOR
                              #\DOWNWARDS_ARROW
                              #\GREEK_SMALL_LETTER_ALPHA
                              #\GREEK_SMALL_LETTER_BETA
                              #\N-ARY_LOGICAL_AND
                              #\NOT_SIGN
                              #\GREEK_SMALL_LETTER_EPSILON
                              #\GREEK_SMALL_LETTER_PI
                              #\GREEK_SMALL_LETTER_LAMDA
                              #\GREEK_SMALL_LETTER_GAMMA
                              #\GREEK_SMALL_LETTER_DELTA
                              #\UPWARDS_ARROW
                              #\PLUS-MINUS_SIGN
                              #\CIRCLED_PLUS
                              #\INFINITY
                              #\PARTIAL_DIFFERENTIAL
                              #\SUBSET_OF
                              #\SUPERSET_OF
                              #\N-ARY_INTERSECTION
                              #\N-ARY_UNION
                              #\FOR_ALL
                              #\THERE_EXISTS
                              #\CIRCLED_TIMES
                              #\LEFT_RIGHT_ARROW
                              #\LEFTWARDS_ARROW
                              #\RIGHTWARDS_ARROW
                              #\NOT_EQUAL_TO
                              #\DIAMOND_OPERATOR
                              #\LESS-THAN_OR_EQUAL_TO
                              #\GREATER-THAN_OR_EQUAL_TO
                              #\IDENTICAL_TO
                              #\N-ARY_LOGICAL_OR))

;; Context

(defvar *current-file* nil)
(defvar *current-file-name* nil)
(defvar *current-line-number* nil)
(defvar *font-stack* nil)
(defvar *unresolved-references* nil)

(defun xml-newline ()
  (cxml::%write-rune #/U+000A cxml::*sink*))

(defun unquote-char (char)
  (cond
    ((> 32 (char-code char))
     (aref *unicode-cp0-chars* (char-code char)))
    ((< 31 (char-code char) 127)
     char)
    (t
     (warn "Unknown non-printable character with code ~A encountered" (char-code char)))))

(defun unquote-line (line)
  (let ((dc1-position (position #\dc1 line)))
    (if (null dc1-position)
        line
        (concatenate 'string
                     (subseq line 0 dc1-position)
                     (string (unquote-char (aref line (1+ dc1-position))))
                     (unquote-line (subseq line (+ 2 dc1-position)))))))

(defun file-warn (&rest args)
  (format *debug-io* "~&~A:~A: " *current-file* *current-line-number*)
  (apply #'format *debug-io* args))

(let (last-file seq)
  (defun generate-anchor-tag ()
    (unless (equal last-file *current-file-name*)
      (setf seq 0))
    (format nil "_~A" (incf seq))))

(defun make-anchor (&optional (name (generate-anchor-tag)))
  (with-element "a"
    (attribute "name" name))
  name)

(defvar *font-names* '((#\1 . "standard")
                       (#\2 . "arg")
                       (#\3 . "obj")
                       (#\5 . "sub-heading")
                       (#\9 . "heading")
                       (#\7 . "example")))

(defun font-name (char)
  (or (cdr (assoc char *font-names*))
      (prog1
          (string char)
        (warn "unknown font ~A" char))))

(defvar *global-directory* (make-hash-table :test #'equal))

(defun lookup-ref (key)
  (gethash key *global-directory*))

(defun enter-ref (key value)
  (when (and (lookup-ref key)
             (not (equal value (lookup-ref key))))
    (file-warn "symbol ~A defined to two values (~A and ~A)" key value (lookup-ref key)))
  (setf (gethash key *global-directory*) value))

(defun ref-expand (line)
  (aif (position #\Syn line)
       (let* ((open-paren-pos (position #\( line :start it))
              (close-paren-pos (position #\) line :start open-paren-pos))
              (key (subseq line (1+ open-paren-pos) close-paren-pos)))
         (text (subseq line 0 it))
         (with-element "ref"
           (aif (lookup-ref key)
                (attribute "file" it)
                (pushnew key *unresolved-references*))
           (attribute "key"  key))
         (ref-expand (subseq line (1+ close-paren-pos))))
       (text line)))

(defun font-expand (line)
  (let ((position 0))
    (labels
        ((do-font-expand ()
           (let* ((ack-pos (and (< position (length line))
                                (position #\Ack line :start position)))
                  (font-char (and ack-pos (aref line (1+ ack-pos)))))
             (if ack-pos
                 (progn
                   (ref-expand (subseq line position ack-pos))
                   (setf position (+ 2 ack-pos))
                   (if (eql #\* font-char)
                       (sax:end-element cxml::*sink* nil nil (or (pop *font-stack*)
                                                                 (file-warn "bad font nesting")))
                       (progn
                         (sax:start-element cxml::*sink* nil nil (font-name font-char) nil)
                         (push (font-name font-char) *font-stack*)))
                   (do-font-expand))
                 (ref-expand (subseq line position))))))
      (do-font-expand))))

(defvar *unparsed-handlers* (make-hash-table))
(defvar *parsed-handlers* (make-hash-table))
(defvar *bolio-variables* (make-hash-table))
(defvar *chapter-number* 0)
(defvar *section-number* 0)
(defvar *subsection-number* 0)
(defvar *in-paragraph*)

(defmacro define-unparsed-bolio-handler (name (arg) &body body)
  `(setf (gethash ',name *unparsed-handlers*)
    (lambda (,arg) ,@body)))

(defmacro define-bolio-handler (name (&rest args) &body body)
  `(setf (gethash ',name *parsed-handlers*)
    (lambda (,@args) ,@body)))

(defmacro with-paragraph (tag &body body)
  `(let ((*in-paragraph* ,tag))
    (with-element ,tag
      ,@body)))

(define-unparsed-bolio-handler chapter (title)
  (incf *chapter-number*)
  (setf *section-number* 0)
  (setf *subsection-number* 0)
  (with-element "chapter"
    (attribute "title" title)
    (continue-parsing)))

(define-unparsed-bolio-handler section (title)
  #+(or) (setf (context-section-title *current-context*) title)
  (with-element "section"
    (attribute "title" title)
    (continue-parsing :stop-before 'section)))

(define-unparsed-bolio-handler subsection (title)
  (with-element "subsection"
    (attribute "title" title)
    (continue-parsing :stop-before '(section subsection))))

(define-unparsed-bolio-handler loop_subsection (title)
  (with-element "subsection"
    (attribute "title" title)
    (continue-parsing :stop-before '(section subsection))))

(define-unparsed-bolio-handler c (comment)
  )

(define-bolio-handler nofill ()
  (with-element "pre"
    (continue-parsing :stop-after 'fill)))

(define-bolio-handler br ()
  (with-element "br"))

(define-bolio-handler sp ()
  (with-element "sp"))

(define-bolio-handler group ()
  (with-element "group"
    (continue-parsing :stop-after 'apart)))

(define-bolio-handler page ()
  (with-element "page"))

(define-bolio-handler need (amount)
  (with-element "need"
    (attribute "amount" amount)))

(define-bolio-handler nopara ()
  (with-element "nopara"))

(define-bolio-handler table (&rest args)
  (with-paragraph "table"
    (with-element "tbody"
      (continue-parsing :stop-after 'end_table))))

(defun handle-item (string)
  (with-element "tr"
    (with-element "td"
      (text string))
    (with-element "td"
      (continue-parsing :stop-before '(end_table item vitem)))))

(define-unparsed-bolio-handler item (string)
  (handle-item string))

(define-unparsed-bolio-handler vitem (string)
  (handle-item string))

(define-unparsed-bolio-handler kitem (string)
  (handle-item string))

(define-unparsed-bolio-handler xitem (string)
  (handle-item string))

(define-unparsed-bolio-handler item1 (string)
  (handle-item string))

(define-unparsed-bolio-handler xitem1 (string)
  (handle-item string))

(defun make-index-entry (index title)
  (xml-newline)
  (with-element "index-entry"
    (attribute "name" index)
    (attribute "title" title))
  (xml-newline))

(define-unparsed-bolio-handler cindex (title)
  (make-index-entry "concepts" title))

(define-unparsed-bolio-handler kindex (title)
  (make-index-entry "keywords" title))

(define-unparsed-bolio-handler vindex (title)
  (make-index-entry "variables" title))

(define-unparsed-bolio-handler findex (title)
  (make-index-entry "functions" title))

(define-unparsed-bolio-handler path_index (title)
  (make-index-entry "loop-path" title))

(define-unparsed-bolio-handler path_preposition_index (title)
  (make-index-entry "loop-path-preposition" title))

(define-unparsed-bolio-handler insert (file)
  (with-element "include"
    (attribute "file" file))
  (process-bolio-file file))

(define-bolio-handler eof ()
  (with-element "disabled"
    (continue-parsing)))

(define-bolio-handler lisp ()
  (with-paragraph "lisp"
    (continue-parsing :stop-after 'end_lisp)))

(defun dbg (format &rest args)
  (when *debug*
    (apply #'format *debug-io* format args)
    (terpri *debug-io*)))

(define-unparsed-bolio-handler cindex (name)
  #+(or) (setf (chapter-index *current-chapter*) name))

(define-bolio-handler setq (name value)
  (case (intern (string-upcase value))
    (chapter-number
     (make-anchor name)
     (enter-ref name *chapter-number*))
    (css-number
     (make-anchor name)
     (enter-ref name *section-number*))
    ((page section-page)
     (make-anchor name)
     (enter-ref name *current-file-name*))
    (t
     (file-warn "don't know how to enter ~A as reference" value))))

(define-bolio-handler exdent (amount &rest caption)
  (with-element "exdent"
    (attribute "amount" amount)
    (with-element "caption"
      (text (format nil "~{~A ~}" caption)))
    (continue-parsing :stop-any t)))

(defvar *bolio-input-stream*)

(defun parse-arguments (string)
  (cond
    ((zerop (length string))
     nil)
    ((not (position #\Space string))
     (list string))
    (t
     (cons (subseq string 0 (position #\Space string))
           (parse-arguments (subseq string (1+ (position #\Space string))))))))

(defun ensure-list (thing)
  (if (atom thing)
      (list thing)
      thing))

(defun handle-bolio-definition (line)
  "Defining commands in bolio begin with .def - The command may be
suffixed with _no_index if the definition should not be indexed, or
with 1 if the definition is one of multiple definitions which are
documented together in one text block.  These commands are handled
seperately since they require a lot of special processing.
The line given as argument is assumed to begin with .def"
  (register-groups-bind
   (type modifier name nil args) (#?r"^.def(\S+?)(|1|_no_index)\s+(\S+)(\s|$)(.*)$" line)
   (let ((no-index (equal modifier "_no_index"))
         (end-symbol (intern (format nil "~:@(end_def~A~)" type)))
         method-name)
     (setf type
           (cond
             ((equal type "un") "fun")
             ((equal type "metamethod") "method")
             ((equal type "const") "var")
             (t type)))
     (when (equal type "method")
       (setf method-name (subseq (first (split " " args)) 1))
       (setf args (regex-replace #?r"^\S+\s" args "")))
     (unless no-index
       (enter-ref (format nil "~A~@[-~(~A~)~]-~A"
                          name
                          method-name
                          (if (find type '("spec" "mac") :test #'equal)
                              "fun"
                              type))
                  *current-file-name*))
     (with-element "define"
       (attribute "type" type)
       (attribute "name" name)
       (when no-index
         (attribute "no-index" "1"))
       (unless (equal "" args)
         (with-element "args"
           (dolist (arg (split #?r"\s+" args))
             (with-element "arg"
               (text arg))))
         (xml-newline))
       (unless (equal "1" modifier)
         (xml-newline)
         (with-element "description"
           (continue-parsing :stop-after end-symbol)))))))

(defun continue-parsing (&key stop-before stop-after stop-any)
  (let (last-line-position)
    (labels
        ((read-input-line ()
           (incf *current-line-number*)
           (setf last-line-position (file-position *bolio-input-stream*))
           (read-line *bolio-input-stream* nil))
         (unread-input-line ()
           (decf *current-line-number*)
           (file-position *bolio-input-stream* last-line-position))
         (handle-bolio-command (line)
           (let ((command (intern (string-upcase (subseq line 1 (or (position #\Space line)
                                                                    (length line))))))
                 (arg-string (aif (position #\Space line)
                                  (subseq line (1+ it))
                                  "")))
             (when (and stop-after
                        (scan "^END" (symbol-name command))
                        (not (eq command stop-after)))
               (file-warn "expected ~A but saw ~A" stop-after command))
             (cond
               ((find command (ensure-list stop-before))
                (unread-input-line)
                (dbg "stopped before ~A" stop-before)
                (return-from continue-parsing))
               ((eq command stop-after)
                (dbg "stopped after ~A" stop-after)
                (return-from continue-parsing))
               ((equal "def" (subseq line 1 (min (length line) 4)))
                (handle-bolio-definition line))
               (t
                (aif (gethash command *unparsed-handlers*)
                     (funcall it arg-string)
                     (aif (gethash command *parsed-handlers*)
                          (apply it (parse-arguments arg-string))
                          (unless *suppress-warnings*
                            (file-warn "unknown bolio command ~A" command)))))))))
      (loop for line = (read-input-line)
            while line
            do (cond
                 ((and (not (zerop (length line))) (find (aref line 0) '(#\. #\')))
                  (when stop-any
                    (unread-input-line)
                    (return-from continue-parsing))
                  (handle-bolio-command line))
                 (t
                  (when (and (equal *in-paragraph* "p")
                             (or (zerop (length line))
                                 (eq #\Tab (aref line 0))))
                    (unless (zerop (length line))
                      (unread-input-line))
                    (return-from continue-parsing))
                  (cond
                    (*in-paragraph*
                     (font-expand (unquote-line line))
                     (xml-newline))
                    (t
                     (with-paragraph "p"
                       (unread-input-line)
                       (continue-parsing))))))))))

(defun process-bolio-file (name)
  (let ((input-pathname (merge-pathnames *input-directory*
                                         (make-pathname :name name :type "text")))
        (output-pathname (merge-pathnames *output-directory*
                                          (make-pathname :name name :type "xml"))))
    (ensure-directories-exist output-pathname)
    (format *debug-io* "~&Processing ~A => ~A" input-pathname output-pathname)
    (let ((*current-file* (namestring input-pathname))
          (*current-file-name* name)
          (*font-stack* nil)
          (*current-line-number* 0)
          (*in-paragraph* nil))
      (with-open-file (*bolio-input-stream* input-pathname)
        (with-open-file (output output-pathname
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede
                                :external-format charset:utf-8)
          (with-xml-output (make-character-stream-sink output)
            (continue-parsing))))
      (when *font-stack*
        (file-warn "imbalanced font specifications")))))

(defun process-bolio-files ()
  (setq *chapter-number* 0)
  (setq *section-number* 0)
  (setq *subsection-number* 0)
  (setq *unresolved-references* nil)
  (dolist (name (mapcar #'string-downcase (mapcar #'symbol-name *manual-filenames*)))
    (process-bolio-file name))
  (when *unresolved-references*
    (format *debug-io* "~&~A unresolved references" (length *unresolved-references*))))

(defun show-bolio-variables ()
  (loop for key being the hash-keys of *bolio-variables*
        do (format *debug-io* "~A => ~A~%" key (gethash key *bolio-variables*))))