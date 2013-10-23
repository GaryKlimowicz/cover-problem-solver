;;;; cover-problem solver
(defpackage "COM.GARYKLIMOWICZ.COVER-PROBLEM"
  (:nicknames "COVER-PROBLEM" "CP")
  (:use "COMMON-LISP")
  (:export cover-problem create-cover-problem solve mh column-names solutions-found col-header node u d l r c n o)
  (:documentation "Knuth's dancing links model application for solving cover problems with algorithm DLX."))
