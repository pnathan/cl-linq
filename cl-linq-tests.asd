;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-linq-test.asd
;;;; license: llgpl

(asdf:defsystem #:cl-linq-tests
  :depends-on ( #:cl-linq #:fiveam)
  :components ((:file "cl-linq-tests"))
  :name "cl-linq-tests"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "Tests for cl-linq")
