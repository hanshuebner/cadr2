;; -*- Lisp -*-

;; Bolio to XML converter, shoe horned to convert the 6th release Lisp
;; Machine Manual to a modern format suitable to online browsing,
;; printing and eventually editing.

(in-package :convert-man)

(enable-interpol-syntax)

(defvar *suppress-warnings* nil)
(defvar *debug* nil)

(defvar *manual-filenames* '(title intro fd-dtp fd-eva fd-con resour
                             fd-sym fd-num fd-arr generic fd-str fd-fun
                             fd-clo fd-sg fd-loc fd-sub areas compil
                             macros looptm defstr flavor ios rdprt pathnm
                             files chaos packd maksys patch proces errors
                             code query init time fd-hac))

(defvar *input-directory* #p"/home/hans/cadr2/lmman/orig6ed/")
(defvar *output-directory* #p"/home/hans/cadr2/lmman/6ed-xml/")

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

(defvar *current-file* nil)
(defvar *current-line-number* nil)
(defvar *font-stack* nil)

(defun file-warn (&rest args)
  (format *debug-io* "~&~A:~A: " *current-file* *current-line-number*)
  (apply #'format *debug-io* args))

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

(defun ref-expand (line)
  (aif (position #\Syn line)
       (let* ((open-paren-pos (position #\( line :start it))
              (close-paren-pos (position #\) line :start open-paren-pos)))
         (text (subseq line 0 it))
         (with-element "ref"
           (attribute "id"  (subseq line (1+ open-paren-pos) close-paren-pos)))
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

(defstruct document title chapters)
(defstruct chapter name number introduction sections)
(defstruct section name number contents)

(defun read-chapter (filename)
  )

(defvar *unparsed-handlers* (make-hash-table))
(defvar *parsed-handlers* (make-hash-table))
(defvar *bolio-variables* (make-hash-table))

(defmacro define-unparsed-bolio-handler (name (arg) &body body)
  `(setf (gethash ',name *unparsed-handlers*)
    (lambda (,arg) ,@body)))

(defmacro define-bolio-handler (name (&rest args) &body body)
  `(setf (gethash ',name *parsed-handlers*)
    (lambda (,@args) ,@body)))

(define-unparsed-bolio-handler chapter (title)
  #+(or) (setf (context-chapter-title *current-context*) title)
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

(define-unparsed-bolio-handler c (comment)
  )

(define-bolio-handler lisp ()
  (with-element "lisp"
    (continue-parsing :stop-after 'end_lisp)))

(defun make-anchor (name)
  (with-element "a"
    (attribute "name" name)))

(defun dbg (format &rest args)
  (when *debug*
    (apply #'format *debug-io* format args)
    (terpri *debug-io*)))

#+(or)
(define-bolio-handler defun (name &rest args)
  (with-defun (name args)
    (dbg "defun ~A" name)
    (continue-parsing :stop-after 'end_defun)
    (dbg "done with defun ~A" name)))

#+(or)
(define-bolio-handler defun1 (name &rest args)
  (with-defun (name args)
    (dbg "defun1 ~A" name)
    ; Nothing to do
    ))

(define-unparsed-bolio-handler cindex (name)
  #+(or) (setf (chapter-index *current-chapter*) name))

(define-bolio-handler setq (name value)
  (setf (gethash name *bolio-variables*) (gethash value *bolio-variables*)))

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
   (type modifier name nil args) (#?r"^.def(\S+?)(|1|_no_index)\s+(\S+)(\s|$)(.*)" line)
   (let ((end-symbol (intern (format nil "~:@(end_def~A~)" type))))
     (when (equal type "un")
       (setf type "fun"))
     (with-element "define"
       (attribute "type" type)
       (attribute "name" name)
       (when (equal modifier "_no_index")
         (attribute "no-index" "1"))
       (unless (equal "" args)
         (with-element "args"
           (dolist (arg (split #?r"\s+" args))
             (with-element "arg"
               (text arg)))))
       (unless (equal "1" modifier)
         (continue-parsing :stop-after end-symbol))))))

(defun continue-parsing (&key stop-before stop-after stop-any)
  (let (last-line-position)
    (labels
        ((read-input-line ()
           (setf last-line-position (file-position *bolio-input-stream*))
           (read-line *bolio-input-stream* nil))
         (handle-bolio-command (line)
           (let ((command (intern (string-upcase (subseq line 1 (or (position #\Space line)
                                                                    (length line))))))
                 (arg-string (aif (position #\Space line)
                                  (subseq line (1+ it))
                                  "")))
             (cond
               ((find command (ensure-list stop-before))
                (file-position *bolio-input-stream* last-line-position)
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
      (loop for *current-line-number* from 1
            for line = (read-input-line)
            while line
            do (progn
                 (cond
                   ((and (not (zerop (length line))) (eq #\. (aref line 0)))
                    (when stop-any
                      (file-position *bolio-input-stream* last-line-position)
                      (return-from continue-parsing))
                    (handle-bolio-command line))
                   (t
                    (font-expand (unquote-line line))
                    (xml-newline))))))))

(defun process-bolio-file (name)
  (let ((input-pathname (merge-pathnames *input-directory*
                                         (make-pathname :name name :type "text")))
        (output-pathname (merge-pathnames *output-directory*
                                          (make-pathname :name name :type "xml"))))
    (ensure-directories-exist output-pathname)
    (format *debug-io* "~&Processing ~A => ~A" input-pathname output-pathname)
    (setq *current-file* (namestring input-pathname))
    (setq *font-stack* nil)
    (with-open-file (*bolio-input-stream* input-pathname)
      (with-open-file (output output-pathname
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede
                              :external-format charset:utf-8)
        (with-xml-output (make-character-stream-sink output)
          (continue-parsing))))
    (when *font-stack*
      (file-warn "imbalanced font specifications"))))

(defun process-bolio-files ()
  (dolist (name (mapcar #'string-downcase (mapcar #'symbol-name *manual-filenames*)))
    (process-bolio-file name)))

(defun show-bolio-variables ()
  (loop for key being the hash-keys of *bolio-variables*
        do (format *debug-io* "~A => ~A~%" key (gethash key *bolio-variables*))))