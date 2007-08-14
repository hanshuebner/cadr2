;;; -*- LISP -*-
;;; Package definition for assembling microcode on the real machine

;;; In a separate package since it wants its own copy of the stuff in QCOM
;;; since it may not be the same as in the running machine.  Note that this
;;; should not ever look at anything such as areas or system constants
;;; in the running machine, it should always have its own copy so you can
;;; change them and reassemble the microcode for the new incompatible world.
;;;
;;; As it stands, this doesn't win 100%, it should really shadow anything
;;; in QCOM that's global.  Otherwise, if you change those it will clobber
;;; the copy used by the running machine.

;;; Note, you need explicit QFASL's in the file names due to at least CMANY-QFASL-P

(defpackage "MICRO-ASSEMBLER" (:nicknames "UA") (:use :common-lisp))
(in-package :micro-assembler)

(asdf:defsystem :ua
    :serial t
    :components
    ((:file "compat")
     (:file "qcom")
     (:file "cadrlp")
     (:file "cdmp")
     (:file "qwmcr")
     (:file "defmic")
     (:file "cadsym")))

;     (:file "usymld"))
