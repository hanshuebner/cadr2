(in-package :convert-man)

;;; Paul Graham, On Lisp, p191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))

;;; by analogy
(defmacro awhen (test-form &rest then-forms)
  `(let ((it ,test-form))
    (when it ,@then-forms)))

