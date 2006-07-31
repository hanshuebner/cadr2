(in-package :cl-user)

(defvar *manual-files* #p"/home/hans/cadr2/mit/lmman/*.text")

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

(defun find-fonts ()
  (let ((result (make-hash-table)))
    (dolist (pathname (directory *manual-files*))
      (with-open-file (f pathname)
        (format *debug-io* "~&Processing ~A" pathname)
        (loop for line = (read-line f nil)
              while line
              do (loop with last-position = 0
                       for ack-position = (position #\ack line :start last-position)
                       while ack-position
                       do (setf last-position (1+ ack-position))
                       do (setf (gethash (aref line (1+ ack-position)) result) 1)))))
    (loop for char being the hash-keys of result
          collect char)))

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
        

(defun unquote ()
  (dolist (input-pathname (directory *manual-files*))
    (let ((output-pathname (merge-pathnames (make-pathname :name (format nil "~A-unicode" (pathname-name input-pathname)))
                                            input-pathname)))
      (with-open-file (input input-pathname)
        (with-open-file (output output-pathname :direction :output :if-does-not-exist :create :if-exists :supersede
                                :external-format (ext:make-encoding :charset 'charset:utf-8))
          (format *debug-io* "~&Processing ~A" input-pathname)
          (loop for line = (unquote-line (read-line input nil))
                while line
                do (princ line output)
                do (terpri output)))))))
