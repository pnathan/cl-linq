;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clink.asd

(asdf:defsystem #:clink
  :depends-on ( #:alexandria
                #:anaphora
                #:local-time
                #:metabang-bind
                #:cl-containers)
  :components ((:file "clink"))
  :name "clink"
  :version "0.1"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "Common Lisp analytics system, designed for high
  performance querying of larger datasets")
