(defpackage clink-tests
  (:use :cl
   :fiveam)
  (:export :run-tests))
(in-package :clink-tests)
(defparameter *debug* t)

(def-suite clink-test-suite)
(in-suite clink-test-suite)
(defun run-tests ()
  )
