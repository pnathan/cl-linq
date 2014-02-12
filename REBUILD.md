This incarnation of cl-linq is largely a semi-maintainable dead end.

The ideas are good, but need another iteration to refine them.

A few problems: lists of lists are slow, as any CS freshman could tell
you; no JOINS, an incredibly slow GROUP BY.

Loosely, what is needed is a function layer to implement each point of
the relational calculus/SQL: JOIN, GROUP-BY, WHERE/HAVING, etc, and a
macro layer to optimize these.  In particular, I anticipate that
WHEREs could be optimized to filter as soon as their conditions become
examinable, thus reducing numbers of rows propagating through the
query.

Next. The data representation and interface is not acceptable.

The data storage needs to have columns headers and other metadata stored separately from the rows.

    (DEFGENERIC HEADERS (data-dump))

The data storage needs to store rows in a reasonable efficient format
for access when in-memory work is attempted.  For Common Lisp, that
implies vectors, with their O(k) access patterns.

    (DEFGENERIC REF (data-dump &rest index-or-column-heading))


Further, streaming needs to be supported (suppose you are reading data
from a database that's too large for memory).


   (DEFGENERIC YIELD (data-dump continuation))



The data storage class needs to be able to read lists of lists,
alists, and vectors.  Note that this implements a SCHEMA system for
each table in effect.


I've looked at the existing data-tables and data-frame libs for Lisp
and I don't think they are sufficient unto the task.  In fact, the
data table supporting CL-LINQ will probably be a solid and useful
library in its own right.
