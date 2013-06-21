(defpackage :cl-linq-tests
  (:use
   :common-lisp
   :fiveam
   :cl-linq)
  (:export
   #:run-tests))
(in-package :cl-linq-tests)

(def-suite cl-linq-tests
    :description "The tests")
(in-suite cl-linq-tests)

(test select-columns
  (let ((data
         '((a b c)
           (1 2 3)
           (4 5 6)))
        (data-assoc
         '(((:name . "bob") (:age . 20))
           ((:name . "frank") (:age . 25)))))
    (is (equalp
        '((a c)
          (1 3)
          (4 6))
        (cl-linq::select-columns data '(0 2))))
    (is (equalp
         '((3 (c b a))
           (3 (3 2 1))
           (3 (6 5 4)))
         (cl-linq::select-columns data (list #'length #'reverse))))
    (is (equalp
         '((20 "bob")
           (25 "frank"))
         (cl-linq::select-columns data-assoc '(:age :name))))))

(defun run-tests ()
  (run! 'cl-linq-tests))
