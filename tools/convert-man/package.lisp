(in-package :cl-user)

(defpackage :convert-man
  (:use :cl
        :cl-user
        :cl-ppcre
        :cl-interpol
        :cxml)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars))