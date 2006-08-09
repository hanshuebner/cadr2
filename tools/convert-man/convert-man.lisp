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

(defun unquote-string (line)
  (let ((dc1-position (position #\dc1 line)))
    (if (null dc1-position)
        line
        (concatenate 'string
                     (subseq line 0 dc1-position)
                     (string (unquote-char (aref line (1+ dc1-position))))
                     (unquote-string (subseq line (+ 2 dc1-position)))))))

(defun convert-charset (string)
  (regex-replace-all "" (unquote-string string) (string #\NO-BREAK_SPACE)))

(defun expand-tabs (line)
  (with-output-to-string (s)
    (do ((i 0 (1+ i))
         (col 0 (1+ col)))
        ((eql i (length line)))
      (let ((char (aref line i)))
        (case char
          (#\Tab (let ((spaces (- 8 (mod col 8))))
                   (princ (make-string spaces :initial-element #\Space) s)
                   (incf col (1- spaces))))
          (t (princ char s)))))))

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

(defun enter-ref (key &rest attributes)
  (let ((value (concatenate 'list (list :key key) attributes)))
    (when (and (lookup-ref key)
               (not (equal value (lookup-ref key))))
      (file-warn "symbol ~A defined to two values (~A and ~A)" key value (lookup-ref key)))
    (setf (gethash key *global-directory*) value)))

(defun ref-expand (line)
  (aif (position #\Syn line)
       (let* ((open-paren-pos (position #\( line :start it))
              (close-paren-pos (position #\) line :start open-paren-pos))
              (key (subseq line (1+ open-paren-pos) close-paren-pos)))
         (text (subseq line 0 it))
         (with-element "ref"
           (aif (lookup-ref key)
                (loop for (key value) on it by #'cddr
                      do (attribute (string-downcase (symbol-name key))
                                    (princ-to-string value)))
                (pushnew key *unresolved-references*)))
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
    (xml-newline)
    (with-element ,tag
      ,@body)))

(defvar *chapter-title->name* (make-hash-table :test #'equal))
(defvar *section-title->name* (make-hash-table :test #'equal))

(defvar *current-chapter-title* "")
(defvar *current-section-title* "")

(defun possibly-unquote (string)
  "remove quotes from string"
  (register-groups-bind
   (unquoted-string) (#?r"^\"(.*)\"$" string)
   (return-from possibly-unquote unquoted-string))
  string)

(define-unparsed-bolio-handler chapter (title)
  (setf title (possibly-unquote title))
  (let ((chapter-name (gethash title *chapter-title->name*)))
    (incf *chapter-number*)
    (setf *section-number* 0)
    (setf *subsection-number* 0)
    (setf *current-chapter-title* title)
    (when chapter-name
      (make-anchor chapter-name))
    (with-element "chapter"
      (attribute "name" chapter-name)
      (attribute "title" title)
      (attribute "number" (princ-to-string *chapter-number*))
      (continue-parsing))))

(define-unparsed-bolio-handler section (title)
  (setf title (possibly-unquote title))
  (let ((section-name (or (gethash title *section-title->name*) title)))
    (incf *section-number*)
    (setf *current-section-title* title)
    (when section-name
      (make-anchor section-name))
    (xml-newline)
    (with-element "section"
      (attribute "name" section-name)
      (attribute "title" title)
      (attribute "chapter-number" (princ-to-string *chapter-number*))
      (attribute "number" (princ-to-string *section-number*))
      (xml-newline)
      (continue-parsing :stop-before 'section))))

(define-unparsed-bolio-handler subsection (title)
  (setf title (possibly-unquote title))
  (let ((section-name (gethash title *section-title->name*)))
    (when section-name
      (make-anchor section-name))
    (xml-newline)
    (with-element "subsection"
      (attribute "name" section-name)
      (attribute "title" title)
      (xml-newline)
      (continue-parsing :stop-before '(section subsection)))))

(define-unparsed-bolio-handler loop_subsection (title)
  (setf title (possibly-unquote title))
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

(define-unparsed-bolio-handler center (text)
  (with-element "center"
    (font-expand (possibly-unquote text))))

(define-bolio-handler group ()
  (with-paragraph "group"
    (continue-parsing :stop-after 'apart)))

(define-bolio-handler page ()
  (with-element "page"))

(define-bolio-handler need (amount)
  (with-element "need"
    (attribute "amount" amount)))

(define-bolio-handler nopara ()
  (with-element "nopara"))

(defvar *table-font* (font-name #\1))

(define-bolio-handler table (&optional (font "1") &rest args)
  (declare (ignore args))
  (when (equal font "0")
    (setf font "1"))
  (let ((*table-font* (font-name (aref font 0))))
    (with-paragraph "table"
      (with-element "tbody"
        (continue-parsing :stop-after 'end_table)))))

(defun handle-item (string)
  (with-element "tr"
    (with-element "td"
      (with-element *table-font*
        (font-expand string)))
    (with-element "td"
      (continue-parsing :stop-before '(end_table
                                       item vitem kitem xitem item1 xitem1)))))

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
    (attribute "index" index)
    (attribute "title" (possibly-unquote title)))
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
  (setf file (string-downcase file))
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

(define-bolio-handler setq (name value)
  (case (intern (string-upcase value))
    (chapter-number
     (setf (gethash *current-chapter-title* *chapter-title->name*) name)
     (enter-ref name
                :type "chapter"
                :definition-in-file *current-file-name*
                :chapter *chapter-number*
                :title *current-chapter-title*))
    (section-page
     (setf (gethash *current-section-title* *section-title->name*) name)
     (enter-ref name
                :type "section"
                :definition-in-file *current-file-name*
                :chapter *chapter-number*
                :section *section-number*
                :title (if (zerop *section-number*)
                           *current-chapter-title*
                           *current-section-title*)))
    (css-number
     (make-anchor name)
     (enter-ref name
                :type "css"
                :definition-in-file *current-file-name*
                :section *section-number*
                :title (format nil "~A.~A" *chapter-number* *section-number*)))
    (page
     (enter-ref name
                :type "page"
                :definition-in-file *current-file-name*))
    (t
     (file-warn "don't know how to enter ~A as reference" value))))

(define-bolio-handler exdent (amount &rest caption)
  (with-element "exdent"
    (attribute "amount" amount)
    (with-element "caption"
      (font-expand (format nil "~{~A ~}" caption)))
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

(let ((last-line-position))
  (defun read-input-line ()
    (incf *current-line-number*)
    (setf last-line-position (file-position *bolio-input-stream*))
    (read-line *bolio-input-stream* nil))

  (defun unread-input-line ()
    (decf *current-line-number*)
    (file-position *bolio-input-stream* last-line-position)))

(defun type-title (type-symbol)
  (ecase type-symbol
    (message "Message")
    (fun "Function")
    (method "Method")
    (metamethod "Meta-Method")
    (const "Constant")
    (condition "Condition")
    (spec "Special Form")
    (mac "Macro")
    (flavor "Flavor")
    (flavor-condition "Flavor Condition")
    (condition-flavor "Condition Flavor")
    (var "Variable")
    (initoption "Initialization Option")
    (meter "Meter")))

(defun handle-bolio-definition (line)
  "Defining commands in bolio begin with .def - The command may be
suffixed with _no_index if the definition should not be indexed, or
with 1 if the definition is one of multiple definitions which are
documented together in one text block.  These commands are handled
seperately since they require a lot of special processing.
The line given as argument is assumed to begin with .def"
  (let (end-symbol)
    (labels
        ((handle-one-definition (line)
           (register-groups-bind
            (type modifier name nil args) (#?r"^.def(\S+?)(|1|_no_index)\s+(\S+)(\s|$)(.*)$" line)
            (when (equal type "un")
              (setf type "fun"))
            #+(or)
            (format t "~&type ~A modifier ~A name ~A args ~A" type modifier name args)
            (let* ((no-index (equal modifier "_no_index"))
                   method-name
                   (type-symbol (intern (string-upcase type)))
                   (type-name (string-downcase (symbol-name type-symbol)))
                   (type-title (type-title type-symbol))
                   (name-title name)
                   (key-suffix (case type-symbol
                                 ((fun spec mac) "fun")
                                 (metamethod "method")
                                 (const "var")
                                 (t type)))
                   key)
              (setf end-symbol (if (eq type-symbol 'fun)
                                   'end_defun
                                   (intern (format nil "~:@(end_def~A~)" type))))
              #+(or)
              (format t "~&name ~A type ~A type-symbol ~A type-name ~A type-title ~A key-suffix ~A"
                      name type type-symbol type-name type-title key-suffix)
              (when (find type-symbol '(method metamethod))
                (register-groups-bind
                 (method-name-buf args-buf) (#?r"^:(\S+)\s*(.*)$" args)
                 (setf name-title (format nil "~A :~A" name method-name-buf))
                 (setf method-name method-name-buf)
                 (setf args args-buf)))
              (setf key (format nil "~A~@[-~(~A~)~]-~A"
                                name
                                method-name
                                key-suffix))
              (unless no-index
                #+(or) (make-index-entry type-name (format nil "~A~@[ ~A~]" name method-name))
                (enter-ref key
                           :type type-name
                           :definition-in-file *current-file-name*
                           :title (format nil "~A ~A" type-title name-title)))
              (with-element "define"
                (attribute "type" type-name)
                (attribute "name" name)
                (attribute "key" key)
                (when no-index
                  (attribute "no-index" "1"))
                (unless (equal "" args)
                  (with-element "args"
                    (font-expand (regex-replace-all #?r"&(\S+)" args "1&\\1*"))))
                (xml-newline))))))
      (with-element "definition"
        (handle-one-definition line)
        (do ((alternate-definition-line (read-input-line) (read-input-line)))
            ((not (scan #?r"^\.def" alternate-definition-line)) (unread-input-line))
          (handle-one-definition (convert-charset alternate-definition-line)))
        (xml-newline)
        (with-paragraph "description"
          (continue-parsing :stop-after end-symbol))))))

(defun continue-parsing (&key stop-before stop-after stop-any)
  (labels
      ((handle-bolio-command (line)
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
          with *paragraph-has-lines* = nil
          while line
          do (cond
               ((and (not (zerop (length line))) (find (aref line 0) '(#\. #\')))
                (when stop-any
                  (unread-input-line)
                  (return-from continue-parsing))
                (handle-bolio-command (convert-charset line)))
               (t
                (when (and (equal *in-paragraph* "p")
                           *paragraph-has-lines*
                           (or (zerop (length line))
                               (eq #\Tab (aref line 0))))
                  (unless (zerop (length line))
                    (unread-input-line))
                  (return-from continue-parsing))
                (cond
                  (*in-paragraph*
                   (setf *paragraph-has-lines* t)
                   (font-expand (expand-tabs (convert-charset line)))
                   (xml-newline))
                  (t
                   (unless (equal "" line)
                     (with-paragraph "p"
                       (when (eq #\Tab (aref line 0))
                         (attribute "indent" "1"))
                       (unread-input-line)
                       (continue-parsing))
                     (xml-newline)))))))))

(defun process-bolio-file (name)
  (format *debug-io* "~&Processing ~A" name)
  (let ((input-pathname (merge-pathnames *input-directory*
                                         (make-pathname :name name :type "text")))
        (output-pathname (merge-pathnames *output-directory*
                                          (make-pathname :name name :type "xml")))
        (stylesheet-name (if (equal name "manual") "toc" "lmman")))
    (ensure-directories-exist output-pathname)
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
            (sax:processing-instruction cxml::*sink* "xml-stylesheet"
                                        (format nil "type=\"text/xsl\" href=\"~A.xsl\"" stylesheet-name))
            (with-element "document-part"
              (xml-newline)
              (handler-case
                  (continue-parsing)
                (error (e)
                  (file-warn "Error: ~A" e)))
              (xml-newline)))))
      (when *font-stack*
        (file-warn "imbalanced font specifications")))))

(define-bolio-handler save-cumulative-state ()
  (when *unresolved-references*
    (format *debug-io* "~&~A unresolved references" (length *unresolved-references*)))
  (setq *chapter-number* 0)
  (setq *section-number* 0)
  (setq *subsection-number* 0)
  (setq *unresolved-references* nil)
  (ext:gc))

(defun clear-cumulative-state ()
  (setf *global-directory* (make-hash-table :test #'equal)))

(defun produce-manual ()
  (let ((*package* (find-package :convert-man)))
    (clear-cumulative-state)
    (format *debug-io* "~&Reference collecting run")
    (process-bolio-file "manual")
    (format *debug-io* "~&File production run")
    (process-bolio-file "manual")
    (format *debug-io* "~&Generating table of contents")
    (unless (zerop (ext:run-program "xsltproc"
                                    :arguments (list "-o" (namestring (merge-pathnames *output-directory* #p"toc.html"))
                                                     (namestring (merge-pathnames *output-directory* #p"toc.xsl"))
                                                     (namestring (merge-pathnames *output-directory* #p"manual.xml")))))
      (warn "cannot create TOC"))
    (format *debug-io* "~&Generating index")
    (unless (zerop (ext:run-program "xsltproc"
                                    :arguments (list "-o" (namestring (merge-pathnames *output-directory* #p"index.xml"))
                                                     (namestring (merge-pathnames *output-directory* #p"indexer.xsl"))
                                                     (namestring (merge-pathnames *output-directory* #p"manual.xml")))))
      (warn "cannot create index"))))

#+(or)
(defun check-xml-file-for-bad-characters (name)
  (let ((pathname (merge-pathnames *output-directory*
                                   (make-pathname :name name :type "xml"))))
    (with-open-file (input pathname)
      (do ((line (read-line input nil) (read-line input nil))
           (line-number 1 (1+ line-number)))
          ((not line))
        (dotimes (i (length line))
          (let ((char (aref line i)))
            (unless (or (< 31 (char-code char))
                        (eq char #\Newline))
              (format t "~&~A:~A invalid character ~A" (namestring pathname) line-number char))))))))

#+(or)
(defun check-xml-files-for-bad-characters ()
  (dolist (name (mapcar #'string-downcase (mapcar #'symbol-name *manual-filenames*)))
    (format t "~&checking ~A" name)
    (check-xml-file-for-bad-characters name)))

#+(or)
(defun xmllint-file (name)
  (let ((pathname (merge-pathnames *output-directory*
                                   (make-pathname :name name :type "xml"))))
    (ext:run-program "xmllint" :arguments `("-noout" ,(namestring pathname)))
    #+(or)
    (with-open-stream (stream (ext:run-program "xmllint" :arguments `("-noout" ,(namestring pathname)) :output :stream))
      (do ((line (read-line stream nil) (read-line stream nil)))
          ((not line))
        (princ line)))))

#+(or)
(defun xmllint-files ()
  (dolist (name (mapcar #'string-downcase (mapcar #'symbol-name *manual-filenames*)))
    (format t "~&xmllinting ~A" name)
    (xmllint-file name)))

