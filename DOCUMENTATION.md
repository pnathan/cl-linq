CLINK documentation.
===

This API is subject to change until 1.0, as per the Semantic
Versioning description.

LOADING
===

Loading may take place from one or more sources. The `LOAD-TABLE`
command integrates the sources into a table suitable for data
preparation or analysis.

* `LOAD-TABLE &rest list-of-sources`

DATA PREP
===
* `ATTACH-INDEX table column &key index-type`

`ATTACH-INDEX` attaches an index of `index-type` to `column`, building
an ephemeral data structure enabling high speed querying and
processing.

* `ANALYZE-TABLE table`

`ANALYZE-TABLE` performs internal housekeeping upon a table and
ensures it is in a goodly and valid state.


QUERYING
===
* `QUERY table query-commands`

Querying is founded upon the relational calculus.

* `PROJECT table out-columns`
* `WHERE table predicate &optional column-hints`
* `INNER-JOIN table1 table2 predicate &optional column1-hints column2-hints`

Column hints specify up-front what columns to examine, allowing
optimizations to pick out only what is required for transport into the
analysis engine.


* `REF`

`REF` is a specialized API designed for accessing tables, rows,
columns, or cells in the CLINK system.

STORING
===

Stored results can be used as loadable sources

* `CREATE-DESTINATION uri &rest parameters`

* `ARCHIVE table destination`
