;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cl-linq.lisp
;;;;
;;;; Author: Paul Nathan
;;;;
;;;; License: LLGPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rationale. LINQ has proven to be an effective and immensely
;;; popular addition to C#. After many times fussing and fiddling with
;;; data handling functions, it became badly apparent to me that LINQ
;;; or a SQL-esque knockoff designed around the idea of aggregating
;;; and quering (relatively) generic datasets was needed. LOOP is not
;;; enough. LOOP is an iteration construct that happens to have
;;; aggregation and selection capabilities.

(defpackage :cl-linq
  (:use
   :common-lisp
   :anaphora)
  (:export
   #:query
   #:cl-linq-select))
(in-package :cl-linq)

(defmacro while (condition &body body)
  "While `condition` is true, body executes. `condition` is tested at
the top of each iteration."
  `(do nil ,(list
             `(not ,condition))
     ,@body))

(defun selector-to-lambda (selector)
  "Convert a selector to a function."
  (typecase selector
    (function selector)
    (integer (lambda (row) (elt row selector)))
    (symbol (lambda (row) (cdr (assoc selector row))))
    (t (error "Unable to determine selector"))))

(defun select-columns (data-rows selectors)
  "Do a selection on a sequence, supports a sequence of
   sequences using MAP and produces a list of lists."
  (let ((selectors (if (consp selectors) selectors (list selectors))))
    (let ((selectors (mapcar #'selector-to-lambda selectors)))
      (map 'list
           (lambda (row)
             (map 'list
                  (lambda (f)
                    (funcall f row)) selectors)) data-rows))))

(defun group-by-tool (group-by results)
  ;;initial cut: taking the index of the table to group by

  ;; WARNING: we are grouping by EQUALP. Probably not ideal.
  (let ((dummy-table (make-hash-table :test #'equalp)))
    (loop for row in results
       do
         (loop for g in group-by
            do
              (let ((row-idx (elt row g)))
                (if (not (gethash row-idx dummy-table))
                    (setf (gethash row-idx dummy-table)
                          (list row))
                    (push row (gethash row-idx dummy-table))))))
    (alexandria:hash-table-alist dummy-table)))



;; peculiar name because it has to get exported. This should help
;; minimize any namespace clashes.
(defun cl-linq-select (columns data
               &key
                 (predicate nil)
                 (aggregation-functions nil)
                 (group-by nil))

  (let ((results)
        (data-length (length data)))

    ;; Conditionally select data
    (loop
       for i from 0 below data-length do
       ;; iteraton via elt to support the sequence abstraction
         (let ((row (elt data i)))

           (if predicate
               (when (funcall predicate row)
                 (push row results))
               (push row results))))

    ;; Select columns
    (let ((selected-results
           (if (or (eq columns t)
                   (eq columns '*))
               results
               (select-columns results columns))))

      ;; Based on the selected columns, determine if we need to GROUP
      ;; BY.
      (let ((grouped-results
             (if group-by
                 (group-by-tool
                  group-by selected-results)
                 selected-results)))

        (if aggregation-functions

            (loop for group in grouped-results
               collect
                 (let ((aggregation
                        ;; Special case it based on whether we get a
                        ;; list of functions or not.
                        (if (consp aggregation-functions)
                            (loop for function in aggregation-functions
                               collect
                                 ;; special case it: group-bys have a
                                 ;; different structure than regular
                                 ;; tables.
                                 (if group-by
                                     (funcall function (cdr group))
                                     (funcall function group)))
                            (if group-by
                                (funcall aggregation-functions (cdr group))
                                (funcall aggregation-functions group)))))

                   ;; cons in the key to the group
                   (if group-by
                       (cons (car group)
                             aggregation)
                       aggregation)))
            grouped-results)))))

(defmacro select-parser (&rest args)
  "SELECT (t | <list of zero-indexed columns>) FROM <data> (WHERE predicate)

Data is expected to be a 2D loopable list of lists."
  (unless (eq (second args) :FROM)
    (error "Expected FROM, got ~a" (second args)))

  (let ((where-pos (position :WHERE args)))
    (when where-pos
        (1+ where-pos)))


  (let ((where-form
          (list :predicate
                (awhen (position :WHERE args)
                  (elt args (1+ it)))))
         (group-by-form
          (list :group-by
                (awhen (position :GROUP-BY  args)
                  (elt args (1+ it)))))
         (aggregation-functions-form
          (list :aggregation-functions
                (awhen (position :AGGREGATING-BY  args)
                  (elt args (1+ it))))))

    (append `(cl-linq-select ,(first args) ,(third args))
            where-form
            group-by-form
            aggregation-functions-form)))


(defmacro all-parser (pred sequence)
  `(every ,pred  ,sequence))

;;(all-parser #'oddp '(1 3 5))

;;(all-parser #'(lambda (val) t) '(1 2 5))


(defmacro any-parser (pred seq)
  `(some ,pred ,seq))

;;(any-parser #'oddp '(1 2 5))

(defmacro contains-parser (val seq)
  `(any-parser
    #'(lambda (ele)
        (eql ele ,val))
    ,seq))

;;(contains-parser 3 '(1 2 3 4))

(defmacro sum-parser (data key)
  `(reduce #'+ ,data :key ,key))

(defmacro query (operation &rest args)
  (ecase operation
    (:min
     `(min-parser ,operation ,@args))
    (:all
     `(all-parser ,(first args) ,(second args)))
    (:contains
     `(contains-parser ,(first args) ,(second args)))
    (:any
     `(any-parser ,(first args) ,(second args)))
    (:sum
     `(sum-parser ,(first args) ,(second args)))
    (:reduce
     `(reduce ,(second args) ,(first args) ,(third args)))
    (:select
     `(select-parser ,@args))))



;; (defparameter *people*
;;   `(((:name . "stephen") (:email . "s@email.com") (:age . 33))
;;     ((:name . "bob") (:email . "b@email.com") (:age . 49))
;;     ((:name . "foo") (:email . "f@email.com") (:age . 10))))

;; (defparameter *subjects*
;;   '((itb001 1 john 4.0)
;;     (itb001 1 bob 4.0)
;;     (itb001 1 mickey 2.0)
;;     (itb001 2 jenny 4.0)
;;     (itb001 2 james 3.0)
;;     (mkb114 1 john 3.0)
;;     (mkb114 1 erica 3.3)))


;; (query
;;  select '(:name :age)
;;  from *people*
;;  where #'(lambda (row)
;;            (> (cdr (assoc :age row)) 21)))

;; (query
;;  select t
;;  from *subjects*
;;  group-by '(0)                          ;first index
;;  aggregating-by (list #'length))

;; (query
;;  select t
;;  from *subjects*
;;  group-by '(0)
;;  aggregating-by
;;  (list #'length
;;        #'(lambda (data)
;;            (/ (apply #'+ (mapcar #'fourth data)) (length data)))))

;; (((ITB001 5) (ITB001 3.4))
;;  ((MKB114 2) (MKB114 3.15)))


;; (cl-linq:QUERY
;;  :select t
;;  :from *subjects*
;;  :where #'(lambda (row)
;;          (> (fourth row) 2.0 ))
;;  :group-by '(0)
;;  :aggregating-by #'length)

(defun test-select-columns ()
  (let ((data
         '((a b c)
           (1 2 3)
           (4 5 6)))
        (data-assoc
         '(((:name . "bob") (:age . 20))
           ((:name . "frank") (:age . 25)))))
    (assert (equalp
             '((a c)
               (1 3)
               (4 6))
             (select-columns data '(0 2))))
    (assert (equalp
             '((3 (c b a))
               (3 (3 2 1))
               (3 (6 5 4)))
             (select-columns data (list #'length #'reverse))))
    (assert (equalp
             '((20 "bob")
               (25 "frank"))
             (select-columns data-assoc '(:age :name))))
    'success))
