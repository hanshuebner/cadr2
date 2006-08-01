;;; -*- Lisp -*-

(in-package :cl-user)

(defpackage :convert-man.system
  (:use :cl :asdf))

(in-package :convert-man.system)

(defsystem :convert-man
    :description "Convert CADR manual from Bolio format to XML"

    :depends-on (:cxml)

    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "convert-man" :depends-on ("package"))))