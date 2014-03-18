(asdf:defsystem #:clink-tests
  :depends-on ( #:clink
                #:checker)
  :components ((:file "clink-tests"))
  :name "clink-tests"
  :version "0.1"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "Tests for clink")
