
;; (push (make-pathname :directory ".") asdf:*central-registry*)

(asdf:load-system :clink-tests :verbose nil)

(clink:run-tests)
(clink-tests:run-tests)
