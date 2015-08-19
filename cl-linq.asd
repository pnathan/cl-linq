;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-linq.asd
;;;; license: llgpl

(asdf:defsystem #:cl-linq
  :depends-on ( #:alexandria #:anaphora)
  :components ((:file "cl-linq"))
  :name "cl-linq"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "CL LINQ style interface with strains of SQL"
  :long-description
  "DSL for managing and querying datasets in a SQL/LINQ style
  syntax. cl-linq provides a simple and usable set of primitives to
  make data examination straightforward. ")
