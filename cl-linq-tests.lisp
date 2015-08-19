(defpackage :cl-linq-tests
  (:use
   :common-lisp
   :cl-linq)
  (:export
   #:run-tests))
(in-package :cl-linq-tests)

(def-suite cl-linq-tests
    :description "CL-LINQ tests")
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


(defparameter *people*
  `(((:name . "stephen") (:email . "s@email.com") (:age . 33))
    ((:name . "bob") (:email . "b@email.com") (:age . 49))
    ((:name . "foo") (:email . "f@email.com") (:age . 10))))

(defparameter *subjects*
  '((itb001 1 john 4.0)
    (itb001 1 bob 4.0)
    (itb001 1 mickey 2.0)
    (itb001 2 jenny 4.0)
    (itb001 2 james 3.0)
    (mkb114 1 john 3.0)
    (mkb114 1 erica 3.3)))

(test more-data-tests
  (is (equalp
       '(("bob" 49) ("stephen" 33))
       (query
        :select '(:name :age)
        :from *people*
        :where #'(lambda (row)
                   (> (cdr (assoc :age row)) 21)))))
  (is (equalp
       '((ITB001 5) (MKB114 2))
       (query
        :select t
        :from *subjects*
        :group-by '(0)                       ;first index
        :aggregating-by (list #'length))))
  (is (equalp
       '(((ITB001 5) (ITB001 3.4))
         ((MKB114 2) (MKB114 3.15)))
       (query
        :select t
        :from *subjects*
        :group-by '(0)
        :aggregating-by
        (list #'length
              #'(lambda (data)
                  (/ (apply #'+ (mapcar #'fourth data)) (length data)))))))
  (is (equalp
       '((ITB001 . 4)
         (MKB114 . 2))
       (cl-linq:QUERY
        :select t
        :from *subjects*
        :where #'(lambda (row)
                   (> (fourth row) 2.0 ))
        :group-by '(0)
        :aggregating-by #'length))))

(defun run-tests ()
  (run! 'cl-linq-tests))
