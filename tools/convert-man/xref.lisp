
(in-package :convert-man)

(enable-interpol-syntax)

(defvar *source-location-tables* (make-hash-table))

(defvar *report-duplicates* nil)

(defun ensure-table (type)
  (or (gethash type *source-location-tables*)
      (setf (gethash type *source-location-tables*) (make-hash-table :test #'equal))))

(defun enter-source-ref (type name &rest attrs)
  (let ((table (ensure-table type))
        (value (append (list :type type :name name) attrs)))
    (setf name (string-upcase name))
    (when (and (gethash name table)
               (not (equal value (gethash name table))))
      (when *report-duplicates*
        (warn "definition of ~A ~A changed from ~A to ~A" type name (gethash name table) value)))
    (setf (gethash name table) value)))

(defun lookup-source-ref (type name)
  (and (gethash type *source-location-tables*)
       (gethash (string-upcase name) (gethash type *source-location-tables*))))

(defun xref-source (pathname)
  (with-open-file (input pathname)
    (let ((first-line (read-line input)))
      (unless (scan #?r"(?i)Package:\s*(\w+)" first-line)
        (setf first-line "Package: USER")
        #+(or)
        (warn "no package found in ~A ~S" pathname first-line))
      (register-groups-bind (package-name) (#?r"(?i)Package:\s*(\W+)" first-line)
                            (do* ((position (file-position input) (file-position input))
                                  (line (read-line input nil 'end-of-file) (read-line input nil 'end-of-file)))
                                 ((eq 'end-of-file line))
                              (register-groups-bind (type name) (#?r"(?i)^\((def\S+)\s+(([^(]\S+)|(\([^)]*?\)))" line)
                                                    (enter-source-ref (intern type)
                                                                      name
                                                                      :package (string-downcase package-name)
                                                                      :file pathname
                                                                      :position position)))))))

(defun xref-sources ()
  (setf *source-location-tables* (make-hash-table))
  (dolist (file (directory *source-files*))
    (xref-source file)))