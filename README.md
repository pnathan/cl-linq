clink

An attempt to provide effective analytics and data manipulation
capabilities in Common Lisp. Microsoft's wildly popular LINQ system is
taken as the API basis for this effort, along with a healthy dose of SQL.

clink began as cl-linq, a list-based set of tools that was capable of
most of the SQL commands save for join. Regrettably, linked lists are
slow, and small real world datasets bogged cl-linq down.

Enter clink. Clink is designed for larger real-world datasets being
stored in such media as databases, csv files, json files, and so
forth. This storage should be invisible after initial load and the
analysis functions to join, search, select, etc should be available at
speed thereafter.

Clink has an unusual design: it is an *immutable* database with
single-assignment semantics at the user level. What this means is that
a table can not be modifed (i.e., INSERTS, UPDATE), but can be
processed and the result stored in a *new* table. This behavior should
be familiar to users of Clojure, Haskell, and other systems which
share immutable data structures.

Clink's speed comes from its ability to cache and compute only the
information needed for the query to succeed.

clink uses Semantic Versioning.

clink is licensed under the Lisp LGPL.
