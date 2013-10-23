;;; ASDF file for cover-problem solver.
(defpackage :cover-problem-system
  (:use :common-lisp :asdf))

(in-package :cover-problem-system)

(defsystem "cover-problem"
  :version "0.1"
  :depends-on ("test-framework")
  :components ((:file "cover-problem-package")
               (:file "cover-problem" :depends-on ("cover-problem-package"))))
