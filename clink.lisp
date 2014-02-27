;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; clink library
;;;; LLGPL
;;;;
;;;; pluggable in-memory database system, inspired by LINQ.
;;;;
;;;; Architecture: operators are singular, take one or more data
;;;; frames and return a data frame. Data frames store metadata,
;;;; including headers.
;;;;
;;;; Implementations of data frames can be done via subclassing
;;;; data-frame and implementing a "few good defmethods".  The example
;;;; data frame is a 2D array.
;;;;
;;;; Data-frames also include indexes, which are linked to one or more
;;;; headers. When an index exists, the hypothetical query planner
;;;; uses that & associated search algorithms to hunt down the
;;;; appropriate methods. Indicies should be reflective at run-time to
;;;; allow the query planner to discover its properties and use
;;;; them. Ideally, multiple indicies for the same key using different
;;;; access characteristics could be implemented (e.g., a hash table
;;;; for an EXISTS-style query, a binary tree for a sargable query..).
;;;;
;;;; The hypothetical & TBD query planner will exist as two layers of
;;;; optimization: layer 1 is a code *parser* of the queries in an
;;;; attempt to perform optimization on the query requested, and layer
;;;; 2 is a straight-forward optimizer examining sizes of data tables,
;;;; etc; the usual query planner in the literature.
;;;;
;;;; This library should also be sufficiently generic to support
;;;; streaming datasets and queries.
;;;;

(ql:quickload '(;:cl-store
                :alexandria
                :anaphora
                :local-time
                :metabang-bind
                :cl-containers))

(defpackage clink
  (:use
   :common-lisp
   :alexandria
   :anaphora
   :metabang-bind
   :local-time)
  ;; TBD.
  (:export ))

(in-package :clink)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug & utility functions
(defparameter *debug* nil "Set to enable debug logging to *debug-stream*")
(defparameter *debug-stream* t "Print to this stream")

(defun debug-log (string)
  "Prints `string` to a nicely formatted string"
  (when *debug*
    (format *debug-stream* "~&[~a] ~a~%" (local-time:now) string)))

(defmacro with-debugging (&body body)
  "Within this form, debug-log prints t"
  `(let ((*debug* t))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic functions that are common across the different classes in clink.

(defgeneric size (object)
  (:documentation "The size of the object. Typically, it will count up
  the number of elements"))

(defgeneric equalg (a b)
  (:documentation "Generic equality function. Long-term, rename and rewrite to
  conform to CDR 8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass header ()
  ((canonical-name
    :reader canonical-name
    :initarg :canonical-name
    :initform nil
    :documentation "Will default to a text rendering of the index")
   (canonical-index
    :reader canonical-index
    :initarg :canonical-index
    :initform (error "must provide a canonical index")
    :documentation "This specifies the offset for the index")
   (alias-list
    :accessor alias-list
    :initarg :alias-list
    :initform nil
    :documentation "These are the values can also refer to the
    header.")
   (index
    :accessor index
    :initform nil
    :initarg :indices
    :documentation "Index data structure for the column, if any")
   (comparator
    :accessor comparator
    :initform nil
    :initarg :comparator
    :documentation "Comparison function for the column datatype")
   (equality
    :accessor equality
    :initform nil
    :initarg :equality
    :documentation "Equality function for the column datatype")
   (datatype
    :accessor datatype
    :initform nil
    :initarg :datatype
    :documentation "Datatype known for the column. If this is not
    known at time of request, nil is returned."))
  (:documentation "Header object: it describes the names of
  a column, as well as the actual index of said column. In part,
  headers also define the metadata for a given column."))

(defgeneric thing-to-header (header candidate)
  (:documentation "Evaluate `candidate` to see if it could be resolved
  to the `header` object passed in"))

(defmethod thing-to-header ((header header) (candidate string))
  (if (string= (canonical-name header) candidate)
      header
      (loop for alias in (alias-list header)
            do
               (when
                   (string= candidate alias)
                 (return-from thing-to-header header)))))

(defmethod thing-to-header ((header header) (candidate number))
  (when (= (canonical-index header) candidate)
    header))

(defmethod equalg ((a header) (b header))
  (and
   (string= (canonical-name a) (canonical-name b))
   (= (canonical-index a) (canonical-index b))))

(defmethod equalg ((a header) (b string))
  (and
   (string= (canonical-name a) b)))

(defmethod equalg ((a string) (b header))
  (and
   (string= a (canonical-name b))))

(defun equal-headers (a b)
  "Assumes a and b are lists of headers"
  (loop
    for i in a
    for j in b
    do
       (if (not (equalg i j))
           (return-from equal-headers nil)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The data frame forms the fundamental unit of the clink system.
(defclass data-frame ()
  ((headers
    :accessor headers
    :initform nil
    :initarg :headers
    :documentation "The headers, each one corresponding to each column
    in the data")
   (data
    :accessor data
    :initform (make-array '(2) :adjustable t :fill-pointer 0)
    :initarg :data
    :documentation "A remark on the data format. The `data-frame` is
    designed to be used as part of a data system with indexes applied
    across the system. Since an efficiently accessed data format
    relies on the sort method (whether in a vector or by a tree), and
    this is a *general* system, the design decision was made to keep
    data quickly accessible once the index was known - hence, the
    array format. ")
   (data-count
    :accessor data-count
    :initform nil
    :documentation "Returns the last known length of the data, as of
    the setting the data in the data-frame"))
  (:documentation "A dataframe is the data + metadata for a given
  page."))

(defmethod width ((obj data-frame))
  (length (headers obj)))

(defmethod lengthg ((obj data-frame))
  (data-count obj))

(defmethod (setf data) :after (new-value (obj data-frame))
  (setf (data-count obj) (first (array-dimensions (data obj)))))

(defun things-to-headers (thing-list)
  (loop
    for h in thing-list
    for i from 0
    collect
    (etypecase h
      (header
       h)
      (number
       (make-instance 'header
                      :canonical-name (format nil "~a" h)
                      :canonical-index h))
      (string
       (make-instance 'header
                      :canonical-name h
                      :canonical-index i)))))

(defmethod (setf headers) :after (new-headers (obj data-frame))
  (with-slots (headers)
      obj
    (setf headers
          (things-to-headers new-headers))))

(defgeneric cast-to-vector (source-object)
  (:documentation "Builds an array of arrays from source-object and returns
  the data and the number of rows thereof "))

(defmethod initialize-instance :after ((obj data-frame)  &key)
  (multiple-value-bind (data length)
      (cast-to-vector (data obj))
    (setf (data obj) data)
    (setf (data-count obj) length)))

;; Assumes a simple data form - not an alist, plist, or hash table.
(defun make-data-frame (data &optional headers)
  (let ((obj (make-instance 'data-frame
                            :data data)))
    (setf (headers obj)
          (if headers
              headers
              (loop for i from 0 below (length (aref (data obj) 0))
                    collect i)))
    obj))

(defmethod cast-to-vector ((obj data-frame))
  (values (data obj) (lengthg obj)))

(defmethod cast-to-vector ((list list))
  "This presumes that `list` is two dimensions."
  (let* ((length (length list))
         (data
           (make-array
            (list length)
            :adjustable t
            :fill-pointer length)))
    (loop
      for row in list
      for i from 0
      do
         (setf (aref data i) (concatenate 'vector row)))
    (values data length)))

(defmethod cast-to-vector ((array array))
  (values array (array-dimension array 0)))

(defmethod cutout-clone ((frame data-frame))
  "A \"cutout\" clone of the data frame, with the same headers and
  width, but without the data."
  (make-instance 'data-frame
                 :headers (headers frame)
                 :width (width frame)))

(defmethod insert ((frame data-frame) (row vector) &key )
  (vector-push-extend row (data frame))
  (incf (data-count frame))
  frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backing store
(defclass store ()
  ()
  (:documentation "A store is a generalized resource locator, suitable
  for accessing anything."))

(defgeneric read-store (store-resource-locator)
  (:documentation "Reads from store-resource-locator and returns a page"))
(defgeneric write-store (store-resource-locator page))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Page: backing store + in-memory + metadata. The page controls the
;;; loading of the data frame and retains information about where to
;;; reread the data if it is unloaded.

(defclass page ()
  ((first-index
    :accessor first-key
    :initform nil
    :initarg :first-key)
   (last-key
    :accessor last-key
    :initform nil
    :initarg :last-key)
   (locked-p
    :accessor locked-p
    :initform nil
    :initarg :locked-p
    :documentation "A table can set the locked bit on a page. Locked
    pages ought not be assigned to. ")
   (data
    :initform nil
    :initarg :data
    :accessor data
    :documentation "Data is either nil (unloaded) or a data-frame.")
   (backing-store
    :initform nil
    :initarg :backing-store
    :accessor backing-store))
  (:documentation "A table has multiple pages. Each page has an actual
  backing object - i.e., a file on-disk. Pages are known by the range
  of primary keys they contain, along with the actual data rows. A
  page A page can unload its data frame on request, but requests to
  read will cause a reload of the data frame."))

(defmethod initialize-instance :after ((page page) &key)
  (with-slots (data)
      page
    (etypecase data
      (data-frame
       t)
      (list
       (setf data
             (make-instance 'data-frame :data data)))
      (vector
       (setf data
             (make-instance 'data-frame :data data))))))

(defmethod ensure-data ((page page))
  (with-slots (data)
      page
    (unless data
      ;; TODO: This function is 100% ded.
      (load-page page)))
  page)

(defmethod data :before ((page page))
  "Automatically load up from the store if it isn't already there."
  (ensure-data page))

(defmethod data :around ((page page))

  (let ((result (call-next-method)))
    (when result
      (data result))))

(defmethod data-frame ((page page))
  (with-slots (data)
      page
    data))

(defmethod (setf data-frame) (new-value (page page))
  (setf (data page) new-value)
  (data-frame page))

(defmethod (setf data) :around (new-data (page page))
  "Ensures that data being inserted is a data frame."
  (when (locked-p page)
    (error "Attempting to modify a locked page."))
  ;; If it's being not being set to nil and it's not a data-frame...
  (if (and new-data
           (not (typep new-data 'data-frame)))
      (call-next-method (make-data-frame new-data) page)
      (call-next-method)))

(defmethod lengthg ((page page))
  (if (ensure-data page)
      (lengthg (data-frame page ))
      ;; This should only happen if the page was unloaded without flushing.
      (error "Data missing")))

(defmethod unload-item  ((page page))
  (with-slots (data) page
    (setf data nil)))

(defmethod load-item ((page page))
  (setf (data page)
        (data (backing-store page))))

(defmethod flush-item ((page page))
  (setf (data (backing-store page)) page))

(defmethod headers ((page page))
  (headers (data-frame page)))

(defun make-page (backing-store &optional data (headers nil))
  (let ((obj (make-instance 'caching-page :backing-store backing-store)))
    (when data
      (setf (data obj)
            (make-data-frame data headers)))
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The caching logic manages loading and unloading in reasonably smart ways.

;;; This is what can be put in a cache
(defclass cachable-item ()
  ((cached
    :accessor cached
    :initform nil)
   (cache
    :accessor cache
    :initform nil))
  (:documentation "The cachable item knows whether it is cached or
  not, and to which cache it belongs. Note that the current design only
  allows for belonging to one cache"))

(defgeneric unload-item (item)
  (:documentation "Unloads the internal backing data from memory"))

(defgeneric flush-item (item)
  (:documentation "A cachable item should occasionally flush to the
  backing store in case it gets evicted"))

(defgeneric load-item (item)
  (:documentation "Loads item from its cache"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Next up should be a tool to manage cache size automatically,
;;; without the clients calling evict-to-size.
(defclass cache ()
  ((size
    :accessor size
    :initform 0)

   ;; This cache data structure ain't working out.
   (members
    :accessor members
    :initform (cl-containers:make-container 'cl-containers:heap-container)
    :initarg :members
    :documentation "Elements should conform to cachable-item")
   (test
       :accessor test
     :initform #'eql
     :initarg :test)
   (cache-overflow-p
    :accessor cache-overflow-p
    :initform #'(lambda (self) (> (size self) 32))
    :initarg :caching-page-overflow-p))
  (:documentation "The cache knows how large it is, keeps a list of
  its members, knows how to compare the members, and knows how to
  determine if it is overflowing (and thus eviction time)"))

;; This API is badly named.

(defmethod note-cached ((item cachable-item) (cache cache))
  (setf (cache item) cache)
  (setf (cached item) t))

(defmethod note-uncached ((item cachable-item) (cache cache))
  (setf (cache item) nil)
  (setf (cached item) nil))

;; Two things need to happen: adding the item to the cache-specific
;; data structure and then fiddling the generic cache data bits.

(defgeneric insert-cache-item (item cache)
  (:documentation "Insert cache item is intended to be only an
  insertion into members, and overridden by child classes. It should
  be repeatable without significantly changing the behavior."))
(defmethod insert-cache-item ((item cachable-item) (cache cache))
  (cl-containers:insert-item (members cache)
                             item))

(defgeneric delete-cache-item (item cache)
  (:documentation "delete cache item is intended to be only an
  deletion from members, and overridden by child classes"))
(defmethod delete-cache-item ((item cachable-item) (cache cache))
  (cl-containers:delete-item (members cache) item))


(defgeneric add-to-cache (item cache)
  (:documentation "add to cache calls insert-cache-item, which is
  expected to be implemented for a given cache type, and also handles
  general management of caches"))
(defmethod add-to-cache ((item cachable-item) (cache cache))
  (insert-cache-item item cache)
  (note-cached item cache)
  (incf (size cache)))

(defgeneric remove-from-cache (item cache)
  (:documentation "Remove from cache calls delete-cache-item, which is
  expected to be implemented for a given cache type, and also handles
  general management of caches"))
(defmethod remove-from-cache ((item cachable-item) (cache cache))
  ;; This may already have been removed.
  (when (cached item)
    (delete-cache-item item cache)
    (note-uncached item cache)
    (decf (size cache))))

(defgeneric evict-to-size (cache)
  (:documentation "Evict-to-size removes all overflowing members of cache
  based on evict-worst's choices."))
(defmethod evict-to-size ((cache cache))
  (when (funcall (cache-overflow-p cache) cache)
    (loop until (not (funcall (cache-overflow-p cache) cache))
          do
             (evict-worst cache))
    t))

(defmethod evict-worst (cache)
  :documentation "Evict-worst finds the worst item, based on
  find-worst-item's choice, unloads it, then calls delete-cache-item
  on the item.")
(defmethod evict-worst ((cache cache))
  (let ((old-item (find-worst-item cache)))

    (unload-item old-item)
    (remove-from-cache old-item cache))
  (decf (size cache)))

(defmethod access ((item cachable-item) (cache cache))
  (error "The generic cache isn't working out too well (some bugs got
  in the hacker's head and left a few holes). Advise using LRU-CACHE..."))

;;; LRU performs well on hot data.
(defclass lru-cache (cache)
  ((members
    :accessor members
    :initform nil
    :documentation "A LRU cache has a particular insertion/deletion
    pattern.")))

;; Note that this implementation uses linked lists. A vector of
;; limited size would be more useful. However, since the cache doesn't
;; have an automatic eviction mechanism, vectors would require
;; reallocs.

;; One trick would be to use a hash table + a fixed-size vector. The
;; hash table has fast add & delete. The vector is accessed as a
;; circular buffer with overwrite; each access would stuff the
;; accessing item on the top of the buffer if it didn't already exist there,

(defmethod insert-cache-item ((item cachable-item) (cache lru-cache))
  ;; Remove remark of item in the history, then add it in on the top.
  (setf (members cache)
        ;; It is to be hoped that this is a nice shiny pointer-based
        ;; delete, as that will be O(n) to find item, then a quick
        ;; fiddle of the CDR to repoint to the CDDR.
        (delete item (members cache)))
  (push item (members cache)))

(defmethod delete-cache-item ((item cachable-item) (cache lru-cache))
  (setf (members cache)
        ;; It is to be hoped that this is a nice shiny pointer-based
        ;; delete, as that will be O(n) to find item, then a quick
        ;; fiddle of the CDR to repoint to the CDDR.
        (delete item (members cache))))

(defmethod access ((item cachable-item) (cache lru-cache))
  (insert-cache-item item cache))

(defmethod data :after ((item cachable-item))
  (access item (cache item)))

(defmethod find-worst-item ((cache lru-cache))
  ;; In our LRU cache, the worst is the furthest back.
  (car (last (members cache))))

;;;;;;;;;;
;;; Future-proofing note: Don't make this a class variable. Really
;;; frustrating bugs relating to when the class instance was created
;;; cropped up.
(defparameter *page-cache*
  (make-instance 'lru-cache
                 :test #'eql))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The page of the caching.
(defclass caching-page (page cachable-item)
  ()
  (:documentation "This serves as the apex of the page management plus
  the cachable-item mixin"))

(defmethod initialize-instance :after ((page caching-page) &key)
  "Automatically add the page to the cache on creation."
  (add-to-cache page *page-cache*))

(defmethod unload-item :before ((page caching-page))
  "Safely ensure that page is written out to its backing store before
dropping it from memory."
  (flush-item page))

(defmethod unload-page :after ((page caching-page))
  (remove-from-cache page *page-cache*))

(defmethod load-item :after ((page caching-page))
  (add-to-cache page *page-cache*))

(defmethod flush-item ((cache cache))
  "Flushes the currently cached pages out to the backing store."
  (loop for page in *page-cache*
        do (flush-item page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Different backing stores.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass disk-store (store)
  ((filename
    :accessor filename
    :initform (error "Requires filename")
    :initarg :filename)))

(defmethod data ((store disk-store))
  (read-store store))

(defmethod (setf data) (new-value (store disk-store))
  (write-store store new-value))

;; (defmethod read-store ((store disk-store))
;;   (cl-store:restore (filename store)))

;; (defmethod write-store ((store disk-store) (page page))
;;   (cl-store:store
;;    (data page)
;;    (filename store)))

(defclass memory-store (store)
  ((data :accessor data :initform nil :initarg :data))
  (:documentation "A purely in-memory backing store. Useful for testing"))

(defmethod read-store ((store memory-store))
  (data store))

(defmethod write-store ((store memory-store) (page page))
  (setf (data store) (data page)))

(defclass url-store (store)
  ((url :accessor url :initform (error "Requires url") :initarg :url))
  (:documentation "A store backed by a url."))

(defmethod read-store ((store url-store))
  (error "Unimplemented"))

(defmethod write-store ((store url-store) (page page))
  (error "Unimplemented"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; degenerate k-d tree.
;; points are composed of (x1 x2 data)
;; each range DOES NOT INTERSECT other ranges.
;;; Note: No (add | delete | update) point routines. Just create and
;;; search.
(defun k-d-tree (points)
  "Points should be (x0 x1 data) where x0 and x1 are unique and do not
intersect any other x0,x1 in the system"
  (when points
    (let*
       ((points (copy-list points))
        (sorted-points (sort points #'< :key #'car))
        (median-idx (floor (/ (length sorted-points) 2))))
     (list
      (elt points median-idx)
      (k-d-tree (subseq points 0 median-idx))
      (k-d-tree (subseq points (1+ median-idx)))))))

(defun find-nearest-key (kd-tree key)
  "Determine the range that key lies between; return the data."
  (when kd-tree
    (let ((node (first kd-tree)))
      (cond
        ((and
          (>= key (first node))
          (<= key (second node)))
         (third node))
        ((< key (first node))
         (find-nearest-key (second kd-tree) key))
        ((> key (second node))
         (find-nearest-key (third kd-tree) key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass iterable ()
  ((finished-iterating
    :accessor finished-iterating-p
    :initform nil)
   (current-iterator
    :initform nil
    :accessor current-iterator
    :documentation "This is the current iterator's pointer"))
  (:documentation "This class supports lazy iteration"))

(defgeneric reset-iteration (seq)
  (:documentation "Reset the current and future to the point of
  root. Reset-iteration should not be destructive to the data stream;
  see RESETTABLE-P to check."))

(defmethod reset-iteration :before ((iterable iterable))
  (setf (finished-iterating-p iterable) nil))

(defgeneric resettable-p (iterable)
  (:documentation "Is the iterable resettable? This should be t for
  file and memory data structures; network based or stream data
  structures should be nil. The default for the iterable hierarchy is t"))

(defmethod resettable-p ((iterable iterable))
  t)

(defgeneric next (seq)
  (:documentation "Set the iterable to the next item"))

(defgeneric root (seq)
  (:documentation "Head of the iterator"))

(defgeneric current  (seq)
  (:documentation "Value of the current iterator"))

(defgeneric future (seq)
  (:documentation "Stuff to come"))

(defgeneric move (seq)
  (:documentation "A stepper function; get data, move to the next
  element. Returns the data gotten from current."))

(defmethod move ((iter iterable))
  (let ((data (current iter)))
    (next iter)
    data))

(defmacro with-fresh-iteration ((iterator) &body body)
  "Pushes the current iteration point and the finished iteration state
and puts iterator into a start point state; completion or error pops
the state and returns to the calling iteration state."
  (let ((current-iteration (gensym))
        (finished-iterating (gensym)))

   `(let ((,current-iteration (current-iterator ,iterator))
          (,finished-iterating (finished-iterating-p ,iterator)))

      (unless (resettable-p ,iterator)
          (error "~a is not resettable; fresh iterations not possible" ,iterator))

      (reset-iteration ,iterator)
      (unwind-protect
           (progn
             ,@body)
        (setf (current-iterator ,iterator) ,current-iteration)
        (setf (finished-iterating-p ,iterator) ,finished-iterating)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The table is the entry point for all queries. A query is executed
;;; upon a table, and returns a table.
;;;
;;; Present design is that tables are append-only, no delete. The
;;; intended use pattern is a load->analyze->new-table->new-analysis
;;; pipeline.

(defclass table (iterable)
  ((headers
    :initform nil
    :accessor headers
    :initarg :headers)
   (current-iterator
    :initform 0)
   (unanalyzed
    :initform t
    :accessor unanalyzed
    :documentation "Has the data been properly analyzed & prepared for
    any sort of analysis doable on static data?")
   (accesses-to-next-eviction-default
    :initform 10
    :accessor accesses-to-next-eviction-default)
   (accesses-to-next-eviction
    :initform 10
    :initarg :accesses-to-next-eviction
    :accessor accesses-to-next-eviction
    :documentation "When this counter reaches zero, the cache eviction
    request is executed.")
   (page-key-index
    :initform nil
    :accessor page-key-index
    :documentation "A tree indexing artifical keys (for access) into
    pages. ")
   (size
    :initform nil
    :accessor size)
   (pages :initform (make-array '(2) :adjustable t :fill-pointer 0)
          :accessor pages))
  (:documentation "A table contains all rows of a given columnar
    structure. This is done via the page mechanism. "))

(defmethod initialize-instance :after ((table table) &key)
  (let ((given-headers (headers table)))
    (setf (headers table) (things-to-headers given-headers))))

(defmethod find-canonical-header ((table table) candidate)
  (let ((headers (headers table)))
    (loop for h in headers
          do
          (when (thing-to-header h candidate)
            (return-from find-canonical-header h)))))

;; This method name is bad. It actually creates an index on the
;; synthetic keys tables have.
(defmethod create-index-on-keys ((table table))
  (let ((index
          (k-d-tree
           (loop for page across (pages table)
                 nconc
                 (list (list (first-key page) (last-key page) page))))))
    (setf (page-key-index table) index)))

(defmethod analyze-table ((table table))
  (let ((counter 0))
    (loop for page across (pages table)
          do
             (setf (first-key page) counter)
             (setf (last-key page) (1- (+ counter (lengthg page))))
             (setf counter (+  counter (lengthg page)))))
  (create-index-on-keys table)
  (setf (unanalyzed table) nil)
  t)

(defmethod (setf root) (new-value (table table))
  (error "Unable to set table's root"))

(defmethod root ((table table))
  ;; the root of table is actually the first page in pages
  table)

(defmethod reset-iteration ((table table))
  (setf (current-iterator table) 0)
  table)

(defmethod page-key-index :before ((table table))
  (when (unanalyzed table)
    (analyze-table table)))

(defmethod cache ((table table))
  (when (pages table)
    (cache (aref (pages table) 0))))

(defmethod flush-item ((table table))
  (loop for page across (pages table)
        do
        (flush-item page)))

(defmethod current-page ((table table))
  (find-nearest-key (page-key-index table) (current-iterator table)))

(defmethod page-with ((table table) index)
  (find-nearest-key (page-key-index table) index))

(defmethod item-at ((table table) (index number))
  (let ((current-page
          (page-with table index)))

    (elt (data current-page)
         (- index (first-key current-page)))))

(defmethod item-at ((table table) (indicies vector))
  (map 'vector #'(lambda (i) (item-at table i)) indicies))

(defmethod item-at :after ((table table) (index number))
  (declare (ignore index))
  (decf (accesses-to-next-eviction table))
  (when (= (accesses-to-next-eviction table) 0)
    (evict-to-size (cache table))
    (setf (accesses-to-next-eviction table)
          (accesses-to-next-eviction-default table))))

(defmethod current ((table table))
  (when (unanalyzed table)
    (analyze-table table))

  (when (finished-iterating-p table)
    (error "Unable to seek data; iterator finished"))

  (item-at table (current-iterator table)))

(defmethod next ((table table))
  (if (> (1- (lengthg table)) (current-iterator table) )
      (incf (current-iterator table))
      (setf (finished-iterating-p table) t)))

(defmethod lengthg ((table table))
  (loop for page across (pages table)
        sum (lengthg page)))

(defmethod data ((table table))
  "Collects all of the data in the pages and returns it as a single
vector of vectors (suitable for putting into a new table or using as a
new singular page). A very bad idea for large data sets."
  (let ((retval (make-array '(2) :adjustable t :fill-pointer 0)))

    (loop for page across (pages table)
          do
             (loop for row across (data page)
                   do (vector-push-extend row retval))
             (evict-to-size *page-cache*))
    retval))

(defmethod (setf data) (new-value (table table))
  (error "Unable to directly set the data on a table"))

(defmethod insertable-page ((table table))
  "Return the page that data can be inserted into."
  (aref (pages table) (1- (length (pages table)))))

(defmethod insert :before ((table table) (anything t) &key)
  (setf (unanalyzed table) t))

(defmethod insert ((table table) (page page) &key)
  (unless (equal-headers (headers page) (headers table))
    (error "Header mismatch: ~a /= ~b" (headers page) (headers table)))

  (vector-push-extend page (pages table))
  table)

(defmethod insert ((table table) (row list) &key)
  (insert table (concatenate 'vector row)))

(defmethod insert ((table table) (row vector) &key)
  (insert (data-frame (insertable-page table)) row)
  table)

;; assumes a 2D list structure
(defmethod bulk-insert ((table table) (rows list))
  ;; Better implemention would be to create a page w/ the rows and append the page.
  (loop for row in rows
        do
        (insert table row)))

(defparameter *foo* nil)
(defun make-dummy-table ()
  (let ((table (make-instance 'table :headers '("0" "1" "2" "3"))))
    (loop for i from 0 upto 32 by 2
          do
             (insert
              table
              (make-page (make-instance 'memory-store)
                         (loop for j from (* i 10)  below (+ (* i 10) 10)
                               collect
                               `(,j ,(format nil "~a" i) ,(gensym "V") ,(random 1000))))))
    (analyze-table table)
    table))

;;;; Implementation of a range tree. This is used as the second
;;;; fundamental block of the index system. Any particular query where
;;;; X > Y && X < Z will fall into this area. Note that Y can be -inf
;;;; and Z +inf.

(defclass range-tree ()
  ((root
    :accessor root
    :initform nil
    :initarg :root
    :documentation "Root node of the range tree. Nil implies
    uninitialized.")
   (comparator
    :accessor comparator
    :initform #'>
    :initarg :comparator
    :documentation "Compares elements. Must be isomorphic to Z. ")
   (equality
    :accessor equality
    :initform #'=
    :initarg :comparator
    :documentation "Compares elements. Must be isomorphic to Z. "))


  (:documentation "A data structure useful for indexing & building
  sargable queries (https://en.wikipedia.org/wiki/Sargable). I.e.,
  select * from table where key > foo and key < bar => range query."))

(defclass range-tree-node ()
  ((tree
    :accessor tree
    :initform nil
    :initarg :tree
    :documentation "A node knows where it lives.")
   (key
    :accessor key
    :initform (error "A key must be supplied to the node")
    :initarg :key
    :documentation "The value that is compared against.")
   (data
    :accessor data
    :initform nil
    :initarg :data
    :documentation "The actual data (row) of the node.")
   ;; (next
   ;;  :accessor next
   ;;  :initform nil
   ;;  :initarg :next
   ;;  :documentation "If set, the next node in the leaf list")
   ;; (previous
   ;;  :accessor previous
   ;;  :initform nil
   ;;  :initarg :previous
   ;;  :documentation "If set, the previous node in the leaf list")
   (parent
    :accessor parent
    :initform nil
    :initarg :parent
    :documentation "The parent of the node. Nil only if the root node.")
   (left
    :accessor left
    :initform nil
    :initarg :left
    :documentation "The left node. Nil for leaf nodes.")
   (right
    :accessor right
    :initform nil
    :initarg :right
    :documentation "The right node. Nil for leaf nodes")))

;; (defmethod leaf-p ((node range-tree-node))
;;   (not (or (left node) (right node) (key node))))

(defun make-range-tree (comparator &optional data)
  (assert (functionp comparator))
  (let ((tree
          (make-instance 'range-tree
                         :comparator comparator)))
    (if data
        (insert tree data)
        tree)))

(defmethod insert ((tree range-tree) key &key data)
  (let ((temp (root tree)))
    (if temp
        (insert temp key :data data)
        (setf (root tree)
              (make-instance 'range-tree-node
                             :key key
                             :tree tree
                             :data data))))
  tree)

(defmacro new-node (parent-node node-form key data)
  (let ((node-name (gensym)))
    `(let ((,node-name ,node-form))
      (if ,node-name
          (insert ,node-form ,key :data ,data)
          (setf ,node-form
                (make-instance
                 'range-tree-node
                 :key ,key
                 :data ,data
                 :parent ,parent-node
                 :tree (tree ,parent-node)))))))

;; inserts with standard binary search tree insertion approach. Data
;; is stored in-node.
(defmethod insert ((node range-tree-node) key &key data)
  (if (funcall (comparator (tree node)) (key node) key)
      (new-node node (left node) key data)
      (new-node node (right node) key data))
  node)


(defgeneric map-over (container funcallable-for-node &key)
  (:documentation "Maps over the elements of container, executing
  funcallable on each of them."))

(defmethod map-over ((tree range-tree) func &key)
  (when (root tree)
    (map-over (root tree) func)))

(defmethod map-over ((node range-tree-node) func &key (level 1))
  "Pre-order call order of func on node and its children. Passes level
to children."
  (list
   (funcall func node level)
   (when (left node)
     (map-over (left node) func :level (1+ level)))

   (when (right node)
    (map-over (right node) func :level (1+ level)))))

(defun find-range (top bottom node)
  (let ((result))
    ;; non-pruning implementation of a range find. O(n)
    (map-over node
              #'(lambda (node depth)
                  (declare (ignore depth))
                  (when (and (>= (key node) bottom)
                             (< (key node) top))
                    (push (key node) result))))
    result))

(defun println (&rest args)
  (funcall #'format t "~&~A~%" args))

(defmacro sure (condition &body body)
  ;; Sure, it returns true when cond is not true.
  ;; Sure, bro. Sure.
  (let ((result (gensym)))
    `(let ((,result ,condition))
       (cond (,result
              ,@body)
             (t t)))))

(defun time-to-prune-p (node cut-point)
  (let ((key (key node))
        (left (when (left node)
                (key (left node))))
        (right (when (right node)
                 (key (right node)))))
    (when (and (> cut-point key)
               (sure left
                 (> cut-point left))
               (sure right
                 (< cut-point right)))
        :yes)))

(defun find-at-most-in-tree (node top)
  (labels ((finder (node top)
             (if (funcall (comparator (tree node)) (key node) top)
                 (cond ((left node)
                        (finder (left node) top)))
                 (cond ((right node)
                        (finder (right node) top))))
             (when (time-to-prune-p node top)
                (return-from find-at-most-in-tree node))))
    (finder node top)))

(defun find-range-in-tree (node bottom top)
  "[bottom, top)"
  (assert node)
  (let ((top-node (find-at-most-in-tree node top)))
    (loop
      for prev = top-node then (find-predecessor prev)
      until (< (key prev) bottom)
      collect prev)))

(defmethod leaf-p ((node range-tree-node))
  (not (or (left node)
            (right node))))

(defgeneric find-minimum (node))
(defmethod find-minimum ((node range-tree-node))
  "Minimum is leftmost"
  ;; We're at the end.
  (if (or (leaf-p node)
          (not (left node)))
      node
      (find-minimum (left node))))

(defgeneric find-maximum (node))
(defmethod find-maximum ((node range-tree-node))
  "Maximum is rightmost"
  (if (or (leaf-p node)
          (not (right node)))
      node
      (find-maximum (right node))))

(defmethod find-predecessor ((node range-tree-node))
  "Finds the closest node that's smaller than `node`"
  (if (left node)
      (find-minimum (left node))
      (find-right-motion node)))

(defmethod find-successor ((node range-tree-node))
  "Finds the one node that's bigger than `node`"
  (if (right node)
      (find-minimum (right node))
      (find-left-motion node)))

;; The find-*-motions attempt to find the next "leftmost" or
;; "rightmost" node.

(defun find-left-motion (node)
  (if (parent node)
      (if (funcall (equality (tree node)) (left (parent node)) node)
          (parent node)
          (find-left-motion (parent node)))
      nil))

(defun find-right-motion (node)
  (if (parent node)
      (if (funcall (equality (tree node))
                   (right (parent node)) node)
          (find-right-motion (parent node))
          (parent node))
      nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass index ()
  ((referent
    :accessor referent
    :initform nil
    :initarg :referent
    :documentation "Must be of class table or nil. The underlying table
   the index is over.")
   (target-column
   :accessor target-column
   :initform nil
   :initarg :target-column
   :documentation "The column the index has indexed. Must be of type
   header or nil.")
   (index
    :accessor index
    :initform nil
    :initarg :index
    :documentation "The actual index. Provides O(fast) time lookup for
    each element in target-column.")
   (built
    :accessor built
    :initform nil
    :initarg :built
    :documentation "Is the index generated?"))
  (:documentation "Base class for indexes. The invariant for this
  index family is that the table does not change under it. This
  implies an immutable data structure for the lifetime of a valid
  index."))

(defmethod initialize-instance :after ((index index) &key)
  (assert (eq (type-of (target-column index)) 'header))
  (assert (eq (type-of (referent index)) 'table)))

(defun guess-comparator-function (value)
  "Reckons a probable comparison function for value. Returns nil if
nothing could be guessed."
  (cond
    ((numberp value)
     #'>)
    ((stringp value)
     #'string>)
    ((symbolp value)
     (lambda (a b)
       (string> (string a)
                (string b))))
    ((characterp value)
     #'char>)))

(defun guess-equality-function (value)
  "Reckons a probable comparison function for value. Returns nil if
nothing could be guessed."
  (cond
    ((numberp value)
     #'=)
    ((stringp value)
     #'string=)
    ((symbolp value)
     (lambda (a b)
       (eq a b)))
    ((characterp value)
     #'char=)))

(defun make-rb-index (table header)
  (let* ((comparator (comparator header))
           (new-index
             (make-instance
              'index
              :target-column header
              :referent table
              :index (cl-containers:make-container
                      ;; Our index-data lives in CAR, our
                      ;; indexed referent lives in CDR.
                      ;; i.e., (item-at table (cdr x))
                      'cl-containers:red-black-tree
                      :key #'car
                      :sorter #'(lambda (x y)
                                  (funcall comparator x y))))))
      (setf (index header) new-index)
      (build-index (index header))
      new-index))

(defmethod build-index ((index index))
  "Creates an index over the oids of the table given, mapping oid to
content. Runs in O(n)."
  (let ((table (referent index))
        (column (target-column index)))
    (with-fresh-iteration (table)
      (loop
        until (finished-iterating-p table)
        do
           (insert index
                   ;; Cons the current element that we're indexing
                   ;; against the oid.
                   (cons
                    (ref (current table)
                         :column
                         column)
                    (current-iterator table)))
           (next table))
      (setf (built index) t))))


;;; Top-level function; use to attach the index on the header.
(defmethod attach-index ((table table) generalized-header &key
                                                    (strict-comparator))

  (let ((header (find-canonical-header table generalized-header)))

    (when (not (comparator header))
      (if strict-comparator
          (error "Header ~a must have a comparator to attach an index" header)
          (unless (setf (comparator header)
                        (guess-comparator-function (ref table :cell
                                                        `((:column ,(canonical-index header))
                                                          (:row 0)))))
            (error "Unable to guess a comparator function"))))

    (when (not (equality header))
      (if strict-comparator
          (error "Header ~a must have an equality tester to attach an index" header)
          (unless (setf (equality header)
                        (guess-equality-function (ref table :cell
                                                        `((:column ,(canonical-index header))
                                                          (:row 0)))))
            (error "Unable to guess a comparator function"))))

    ;; Build the index with a r-b tree.
    (make-rb-index table header)))

(defmethod oid-of ((table table) generalized-header item)
  "Returns the array of oids of the item, if any. This allows a quick
lookup into the pages of the table"
  (let* ((header (find-canonical-header table generalized-header))
         (equal-foo (equality header)))

    (map 'vector
         #'cdr
         (cl-containers:collect-elements
                 (index (index header ))
                 :filter #'(lambda (x)
                             (funcall equal-foo item (car x)))))))

(defmethod lengthg ((index index))
  (cl-containers:size (index index)))

(defmethod insert ((index index) (item t) &key)
  (cl-containers:insert-item (index index) item))



;; select * from *foo* where *foo*."0" == 201
;; =>
;; (item-at *foo* (oid-of 0 201))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defgeneric ref (container type selector)
  (:documentation "The REF api allows a simple generalized reference
approach to addressing data.

A ref should be able to point at a column; said column should be
denoted by a header, which should then direct to the appropriate
index.  A ref should also be able to point at a given row, which be
returnable.

And finally, a cell should be denotable by a combination row and
column. Cells can be implemented with an alist `((:row ,r) (:column
,c))
"))


;; ref on tables
(defmethod ref ((table table) (type (eql :column)) selector)
  "Returns the column; no more can be found here"
  (find-canonical-header table selector))

(defmethod ref ((table table) (header header) item)
  (let ((it
          (oid-of table (index header) item)))
    (when it
      (item-at table it))))

(defmethod ref ((table table) (type (eql :row)) index)
  (item-at table index))

(defmethod ref ((table table) (type (eql :cell)) (pair list))
  "Address by column x table-oid"
  (let ((row (item-at table (cadr (assoc :row pair)))))
    (ref row :column (cadr (assoc :column pair)))))

;; Ref on vectors

(defmethod ref ((item vector) (type (eql :column)) (selector header))
  (ref item :column (canonical-index selector)))

(defmethod ref ((item vector) (type (eql :column)) (selector integer))
  (aref item selector))

;; Ref on data frames
(defmethod ref ((data-frame data-frame) (type (eql :row)) selector)
  (error "Unimplemented"))
(defmethod ref ((data-frame data-frame) (type (eql :column)) selector)
  (error "Unimplemented"))
(defmethod ref ((data-frame data-frame) (type (eql :cell)) selector)
  (error "Unimplemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finally, the query.
(defclass query ()
  ())


;;; This is the top layer of access to the clink system
(defgeneric yield (data-frame query &key &allow-other-keys))
(defgeneric update (data-frame query &key &allow-other-keys))
(defgeneric where (data-frame condition &key &allow-other-keys))
(defgeneric group-by (data-frame condition &key &allow-other-keys))
(defgeneric join (data-frame-a data-frame-b &key &allow-other-keys))
(defgeneric inner (data-frame query &key &allow-other-keys))
(defgeneric insert (container object &key &allow-other-keys))
(defmethod where ((obj data-frame) condition &key &allow-other-keys)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; random functions


(defmethod current ((seq list))
  (car seq))

(defmethod future ((seq list))
  (cdr seq))

(defmethod next ((seq list))
  (setf (car seq) (cadr seq))
  (setf (cdr seq) (cddr seq)))
