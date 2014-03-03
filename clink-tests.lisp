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


(defun check (testform ))

(defun check-headers-test ()
  ;; Can we get at the important information?
  (let ((header (make-instance 'header
                               :canonical-name "Column 0"
                               :canonical-index 0)))
    (assert (string= (canonical-name header)
                     "Column 0"))
    (assert (= (canonical-index header)
               0)))

  ;; Can we test equality and have it come out correctly?
  (let ((header (make-instance 'header
                               :canonical-name "Column 0"
                               :canonical-index 0)))

    (assert (eq (equalg header header) t)))

  ;; Can we use the helper function things-to-headers correctly?
  (let ((header-one (make-instance 'header
                                   :canonical-name "id"
                                   :canonical-index 0))
        (header-two (make-instance 'header
                                   :canonical-name "name"
                                   :canonical-index 1))
        (headers
         (things-to-headers '("id" "name"))))

    (assert (equalg (first headers) header-one))
    (assert (equalg (second headers) header-two)))

  ;; Header ~ string mapping
  (let ((header (make-instance 'header
                                   :canonical-name "id"
                                   :canonical-index 0)))

    (assert (equalg header "id"))
    (assert (equalg "id" header)))

  (let ((header (make-instance 'header
                               :canonical-name "id"
                               :canonical-index 30
                               :alias-list
                               '("identity" "ID" :id 'ID))))
    (assert (thing-to-header-p header "id"))
    (assert (thing-to-header-p header 30))
    (assert (thing-to-header-p header "ID"))
    (assert (thing-to-header-p header "identity"))
    ; (assert (thing-to-header-p header 'ID))
    ; (assert (thing-to-header-p header :id))
    ))

(defun run-tests ()
  (check-headers-test))
