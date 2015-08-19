cl-linq
===

A simple queryable interface for tabular datasets.

Examples:

```
CL-USER> (cl-linq:query :contains 1 '(1 2 3 4))
T
CL-USER> (cl-linq:query :all #'oddp '(1 2 3 ))
NIL
CL-USER> (cl-linq:query :all #'oddp '(1 3 5 ))
T
CL-USER>
CL-USER> (defparameter *people*
          `(((:name . "stephen") (:email . "s@email.com") (:age . 33))
            ((:name . "bob") (:email . "b@email.com") (:age . 49))
            ((:name . "foo") (:email . "f@email.com") (:age . 10))))
*PEOPLE*
CL-USER> (cl-linq:query
          :select '(:name :age)
          :from *people*
          :where #'(lambda (row)
                     (> (cdr (assoc :age row)) 21)))
(("bob" 49) ("stephen" 33))
```

Bugs
---

* grouo-by appears not to work

Maintainer:
---

None. This project has algorithmic inefficiencies structurally. If
someone wants to take it and run, please do so without worrying about
asking for permission.


Contributors:
---

* Stephen "deliciousrobots" Goss
* Paul Nathan


License
---
LLGPL
