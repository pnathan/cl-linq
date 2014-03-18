;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clink-tests.lisp
;;;;
;;;;
;;;; Organization of file:
;;;;
;;;; Package: clink-tests. This test set handles the external exports
;;;; of clink (that is, the interface.
;;;;
;;;; Package: clink.  This test set handles the internal
;;;; tests. General policy: check type safety and exercise the typical
;;;; code paths.


(defpackage clink-tests
  (:use :cl)
  (:export :run-tests))

;;; Remark. I usually like fiveam for testing, but it has some interesting
;;; evaluating issues related to LET forms. Unacceptable, sadly.

(in-package :clink-tests)

(defparameter *debug* t)

(defun run-tests ()

  )


;; Note that this file doesn't get loaded unless clink-tests.asd is called.
(in-package :clink)


(defun check-headers-test ()
  ;; Can we get at the important information?
  (let ((header (make-instance 'header
                               :canonical-name "Column 0"
                               :canonical-index 0)))
    (checker:check (string= (canonical-name header)
                     "Column 0"))
    (checker:check (= (canonical-index header)
               0)))

  ;; Can we check equality and have it come out correctly?
  (let ((header (make-instance 'header
                               :canonical-name "Column 0"
                               :canonical-index 0)))

    (checker:check (eq (equalg header header) t)))

  ;; Can we use the helper function things-to-headers correctly?
  (let ((header-one (make-instance 'header
                                   :canonical-name "id"
                                   :canonical-index 0))
        (header-two (make-instance 'header
                                   :canonical-name "name"
                                   :canonical-index 1))
        (headers
         (things-to-headers '("id" "name"))))

    (checker:check (equalg (first headers) header-one))
    (checker:check (equalg (second headers) header-two)))

  ;; Header ~ string mapping
  (let ((header (make-instance 'header
                                   :canonical-name "id"
                                   :canonical-index 0)))

    (checker:check (equalg header "id"))
    (checker:check (equalg "id" header)))

  (let ((header (make-instance 'header
                               :canonical-name "id"
                               :canonical-index 30
                               :alias-list
                               '("identity" "ID" :id 'ID))))
    (checker:check (thing-to-header-p header "id"))
    (checker:check (thing-to-header-p header 30))
    (checker:check (thing-to-header-p header "ID"))
    (checker:check (thing-to-header-p header "identity"))
    (checker:check (thing-to-header-p header 'ID))
    (checker:check (thing-to-header-p header :id)))

  (let* (
         (header-one (make-instance 'header
                                   :canonical-name "id"
                                   :canonical-index 0))
         (header-two (make-instance 'header
                                   :canonical-name "name0"
                                   :canonical-index 1))
         (header-three (make-instance 'header
                                   :canonical-name "name1"
                                   :canonical-index 2))
         (header-four (make-instance 'header
                                    :canonical-name "name2"
                                    :canonical-index 3))
         (header-five (make-instance 'header
                                   :canonical-name "name3"
                                   :canonical-index 4))
         (header-list-a
           (list header-one header-two header-three))
         (header-list-b
           (list header-four header-five)))

    (checker:check (equal-headers header-list-a header-list-a))
    (checker:check (not (equal-headers header-list-a header-list-b)))
    (checker:check (not (equal-headers header-list-b header-list-a)))))

(defmethod checker:run-checker-tests ((specializer (eql :clink-headers)))
  (check-headers-test))

(defun check-data-frame ()
  (checker:check (make-instance 'data-frame))
  (checker:check (= 0 (width (make-instance 'data-frame)))))

(defmethod checker:run-checker-tests ((specializer (eql :data-frame)))
  (check-data-frame))

(defun run-tests ()
  (checker:run-checker-tests :clink-headers)
  (checker:run-checker-tests :data-frame)
  t)
