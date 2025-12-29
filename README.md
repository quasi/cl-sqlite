# CL-SQLITE

## Abstract

CL-SQLITE package is an interface to the SQLite embedded relational database engine.

The code is in public domain so you can basically do with it whatever you want.

This documentation describes only the CL-SQLITE package, not the
SQLite database itself. SQLite documentation is available at <http://sqlite.org/docs.html>

CL-SQLITE together with this documentation can be downloaded from
https://github.com/quasi/cl-sqlite/


This package is forked and updated from the original at http://github.com/TeMPOraL/cl-sqlite/

Original Authors: TeMPOraL
Updated by: quasi



## Contents

1.  [Installation](#installation)
2.  [Example](#example)
3.  [Usage](#usage)
4.  [The SQLITE dictionary](#dictionary)
    1.  [`bind-parameter`](#bind-parameter)
    2.  [`clear-statement-bindings`](#clear-statement-bindings)
    3.  [`connect`](#connect)
    4.  [`disconnect`](#disconnect)
    5.  [`execute-non-query`](#execute-non-query)
    6.  [`execute-non-query/named`](#execute-non-query/named)
    7.  [`execute-one-row-m-v`](#execute-one-row-m-v)
    8.  [`execute-one-row-m-v/named`](#execute-one-row-m-v/named)
    9.  [`execute-single`](#execute-single)
    10. [`execute-single/named`](#execute-singled/named)
    11. [`execute-to-list`](#execute-to-list)
    12. [`execute-to-list/named`](#execute-to-list/named)
    13. [`finalize-statement`](#finalize-statement)
    14. [`last-insert-rowid`](#last-insert-rowid)
    15. [`prepare-statement`](#prepare-statement)
    16. [`reset-statement`](#reset-statement)
    17. [`sqlite-error`](#sqlite-error)
    18. [`sqlite-constraint-error`](#sqlite-constraint-error)
    19. [`sqlite-error-code`](#sqlite-error-code)
    20. [`sqlite-error-db-handle`](#sqlite-error-db-handle)
    21. [`sqlite-error-message`](#sqlite-error-message)
    22. [`sqlite-error-sql`](#sqlite-error-sql)
    23. [`sqlite-handle`](#sqlite-handle)
    24. [`sqlite-statement`](#sqlite-statement)
    25. [`statement-bind-parameter-names`](#statement-bind-parameter-names)
    26. [`statement-column-names`](#statement-column-names)
    27. [`statement-column-value`](#statement-column-value)
    28. [`step-statement`](#step-statement)
    29. [`with-transaction`](#with-transaction)
    30. [`with-open-database`](#with-open-database)
5.  [Support](#support)
6.  [Changelog](#changelog)
7.  [Acknowledgements](#ack)


## Installation

-- coming soon --

## Example


    (use-package :sqlite)
    (use-package :iter)

    (defvar *db* (connect ":memory:")) ;;Connect to the sqlite database. :memory: is the temporary in-memory database

    (execute-non-query *db* "create table users (id integer primary key, user_name text not null, age integer null)") ;;Create the table

    (execute-non-query *db* "insert into users (user_name, age) values (?, ?)" "joe" 18)
    (execute-non-query/named *db* "insert into users (user_name, age) values (:user_name, :user_age)" 
                             ":user_name" "dvk" ":user_age" 22)
    (execute-non-query *db* "insert into users (user_name, age) values (?, ?)" "qwe" 30)
    (execute-non-query *db* "insert into users (user_name, age) values (?, ?)" nil nil) ;; ERROR: constraint failed

    (execute-single *db* "select id from users where user_name = ?" "dvk")
    ;; => 2
    (execute-one-row-m-v *db* "select id, user_name, age from users where user_name = ?" "joe")
    ;; => (values 1 "joe" 18)

    (execute-to-list *db* "select id, user_name, age from users")
    ;; => ((1 "joe" 18) (2 "dvk" 22) (3 "qwe" 30))

    ;; Use iterate
    (iter (for (id user-name age) in-sqlite-query "select id, user_name, age from users where age < ?" on-database *db* with-parameters (25))
          (collect (list id user-name age)))
    ;; => ((1 "joe" 18) (2 "dvk" 22))

    ;; Use iterate with named parameters
    (iter (for (id user-name age) in-sqlite-query/named "select id, user_name, age from users where age < :age"
          on-database *db* with-parameters (":age" 25))
          (collect (list id user-name age)))
    ;; => ((1 "joe" 18) (2 "dvk" 22))

    ;; Use prepared statements directly
    (loop
       with statement = (prepare-statement *db* "select id, user_name, age from users where age < ?")
       initially (bind-parameter statement 1 25)
       while (step-statement statement)
       collect (list (statement-column-value statement 0) (statement-column-value statement 1) (statement-column-value statement 2))
       finally (finalize-statement statement))
    ;; => ((1 "joe" 18) (2 "dvk" 22))

    ;; Use prepared statements with named parameters
    (loop
       with statement = (prepare-statement *db* "select id, user_name, age from users where age < :age")
       initially (bind-parameter statement ":age" 25)
       while (step-statement statement)
       collect (list (statement-column-value statement 0) (statement-column-value statement 1) (statement-column-value statement 2))
       finally (finalize-statement statement))
    ;; => ((1 "joe" 18) (2 "dvk" 22))

    (disconnect *db*) ;;Disconnect


## Usage

Two functions and a macro are used to manage connections to the
database:

- Function [connect](#connect) connects to the database
- Function [disconnect](#disconnect) disconnects from the database
- Macro [with-open-database](#with-open-database) opens the database and
  ensures that it is properly closed after the code is run

To make queries to the database the following functions are provided:

- [execute-non-query](#execute-non-query)
  ([execute-non-query/named](#execute-non-query/named)) executes the
  query and returns nothing
- [execute-single](#execute-single)
  ([execute-single/named](#execute-single/named)) returns the first
  column of the first row of the result
- [execute-one-row-m-v](#execute-one-row-m-v)
  ([execute-one-row-m-v/named](#execute-one-row-m-v/named)) returns the
  first row of the result as multiple values
- [execute-to-list](#execute-to-list)
  ([execute-to-list/named](#execute-to-list/named)) returns all rows as
  the list of lists

Macro [with-transaction](#with-transaction) is used to execute code
within transaction.

Support for [ITERATE](http://common-lisp.net/project/iterate/) is
provided. Use the following clause:

> (for (vars) in-sqlite-query sql on-database db &optional with-parameters (&rest parameters))

This clause will bind *vars* (a list of variables) to the values of the
columns of query.

Additionally, it is possible to use the prepared statements API of
sqlite. Create the prepared statement with
[prepare-statement](#prepare-statement), bind its parameters with
[bind-parameter](#bind-parameter), step through it with
[step-statement](#step-statement), retrieve the results with
[statement-column-value](#statement-column-value), and finally reset it
to be used again with [reset-statement](#reset-statement) or dispose of
it with [finalize-statement](#finalize-statement).

Positional and named parameters in queries are supported. Positional
parameters are denoted by question mark in SQL code, and named
parameters are denoted by prefixing color (:), at sign (@) or dollar
sign (\$) before parameter name.

Following types are supported:

- Integer. Integers are stored as 64-bit integers.
- Float. Stored as double. Single-float, double-float and rational may
  be passed as a parameter, and double-float will be returned.
- String. Stored as an UTF-8 string.
- Vector of bytes. Stored as a blob.
- Null. Passed as NIL to and from database.


## The SQLITE dictionary

\
\[Function\]\
[**bind-parameter** *statement parameter value*]{#bind-parameter .none}

> \
> Sets the *parameter* in *statement* to the *value*.\
> *parameter* is an index (parameters are numbered from one) or the name
> of a parameter.\
> Supported types:\
>
> - Null. Passed as NULL
> - Integer. Passed as an 64-bit integer
> - String. Passed as a string
> - Float. Passed as a double
> - (vector (unsigned-byte 8)) and vector that contains integers in
>   range \[0,256). Passed as a BLOB

\
\[Function\]\
[**clear-statement-bindings** *statement*]{#clear-statement-bindings
.none}

> \
> Binds all parameters of the statement to NULL.

\
\[Function\]\
[**connect** *database-path* *&key* *busy-timeout* =\>
*sqlite-handle*]{#connect .none}

> \
> Connect to the sqlite database at the given *database-path*
> (*database-path* is a string or a pathname). If *database-path* equal
> to `":memory:"` is given, a new in-memory database is created. Returns
> the [sqlite-handle](#sqlite-handle) connected to the database. Use
> [disconnect](disconnect) to disconnect.\
> Operations will wait for locked databases for up to *busy-timeout*
> milliseconds; if *busy-timeout* is NIL, then operations on locked
> databases will fail immediately.

\
\[Function\]\
[**disconnect** *handle*]{#disconnect .none}

> \
> Disconnects the given *handle* from the database. All further
> operations on the handle and on prepared statements (including freeing
> handle or statements) are invalid and will lead to memory corruption.

\
\[Function\]\
[**execute-non-query** *db sql `&rest` parameters*]{#execute-non-query
.none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns nothing.\
> Example:\
>
>     (execute-non-query db "insert into users (user_name, real_name) values (?, ?)" "joe" "Joe the User")
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-non-query/named** *db sql `&rest`
parameters*]{#execute-non-query/named .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns nothing. Parameters are alternating names and values.\
> Example:\
>
>     (execute-non-query/named db "insert into users (user_name, real_name) values (:user_name, :real_name)"
>                              ":user_name" "joe" ":real_name" "Joe the User")
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-one-row-m-v** *db sql `&rest` parameters* =\> (values
*result\**)]{#execute-one-row-m-v .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns the first row as multiple values.\
> Example:\
>
>     (execute-one-row-m-v db "select id, user_name, real_name from users where id = ?" 1)
>     =>
>     (values 1 "joe" "Joe the User")
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-one-row-m-v/named** *db sql `&rest` parameters* =\> (values
*result\**)]{#execute-one-row-m-v .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns the first row as multiple values. Parameters are alternating
> names and values.\
> Example:\
>
>     (execute-one-row-m-v/named db "select id, user_name, real_name from users where id = :id" ":id" 1)
>     =>
>     (values 1 "joe" "Joe the User")
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-single** *db sql `&rest` parameters* =\>
*result*]{#execute-single .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns the first column of the first row as single value.\
> Example:\
>
>     (execute-single db "select user_name from users where id = ?" 1)
>     =>
>     "joe"
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-single/named** *db sql `&rest` parameters* =\>
*result*]{#execute-single/named .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns the first column of the first row as single value. Parameters
> are alternating names and values.\
> Example:\
>
>     (execute-single/named db "select user_name from users where id = :id" ":id" 1)
>     =>
>     "joe"
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-to-list** *db sql `&rest` parameters* =\>
*results*]{#execute-to-list .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns the results as list of lists.\
> Example:\
>
>     (execute-to-list db "select id, user_name, real_name from users where user_name = ?" "joe")
>     =>
>     ((1 "joe" "Joe the User")
>      (2 "joe" "Another Joe")) 
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**execute-to-list/named** *db sql `&rest` parameters* =\>
*results*]{#execute-to-list/named .none}

> \
> Executes the query *sql* to the database *db* with given *parameters*.
> Returns the results as list of lists. Parameters are alternating names
> and values.\
> Example:\
>
>     (execute-to-list db "select id, user_name, real_name from users where user_name = :name" ":name" "joe")
>     =>
>     ((1 "joe" "Joe the User")
>      (2 "joe" "Another Joe")) 
>
> See [bind-parameter](#bind-parameter) for the list of supported
> parameter types.

\
\[Function\]\
[**finalize-statement** *statement*]{#finalize-statement .none}

> \
> Finalizes the *statement* and signals that associated resources may be
> released.\
> Note: does not immediately release resources because statements are
> cached.

\
\[Function\]\
[**last-insert-rowid** *db* =\> *result*]{#last-insert-rowid .none}

> \
> Returns the auto-generated ID of the last inserted row on the database
> connection *db*.

\
\[Function\]\
[**prepare-statement** *db sql* =\>
*sqlite-statement*]{#prepare-statement .none}

> \
> Prepare the statement to the DB that will execute the commands that
> are in *sql*.\
> Returns the [sqlite-statement](#sqlite-statement).\
> *sql* must contain exactly one statement.\
> *sql* may have some positional (not named) parameters specified with
> question marks.\
> Example:\
>
>     (prepare-statement db "select name from users where id = ?")

\
\[Function\]\
[**reset-statement** *statement*]{#reset-statement .none}

> \
> Resets the *statement* and prepares it to be called again. Note that
> bind parameter values are not cleared; use
> [clear-statement-bindings](#clear-statement-bindings) for that.

\
\[Condition\]\
[**sqlite-error**]{#sqlite-error .none}

> \
> Error condition used by the library.

\
\[Condition\]\
[**sqlite-constraint-error**]{#sqlite-constraint-error .none}

> \
> A subclass of sqlite-error used to distinguish constraint violation
> errors.

\
\[Accessor\]\
[**sqlite-error-code** *sqlite-error* =\> *keyword or
null*]{#sqlite-error-code .none}

> \
> Returns the SQLite error code represeting the error.

\
\[Accessor\]\
[**sqlite-error-db-handle** *sqlite-error* =\> *sqlite-handle or
null*]{#sqlite-error-db-handle .none}

> \
> Returns the SQLite database connection that caused the error.

\
\[Accessor\]\
[**sqlite-error-message** *sqlite-error* =\> *string or
null*]{#sqlite-error-message .none}

> \
> Returns the SQLite error message corresponding to the error code.

\
\[Accessor\]\
[**sqlite-error-sql** *sqlite-error* =\> *string or
null*]{#sqlite-error-sql .none}

> \
> Returns the SQL statement source string that caused the error.

\
\[Standard class\]\
[**sqlite-handle**]{#sqlite-handle .none}

> \
> Class that encapsulates the connection to the database.

\
\[Standard class\]\
[**sqlite-statement**]{#sqlite-statement .none}

> \
> Class that represents the prepared statement.

\
\[Accessor\]\
[**statement-bind-parameter-names** *statement* =\> *list of
strings*]{#statement-bind-parameter-names .none}

> \
> Returns the names of the bind parameters of the prepared statement. If
> a parameter does not have a name, the corresponding list item is NIL.

\
\[Accessor\]\
[**statement-column-names** *statement* =\> *list of
strings*]{#statement-column-names .none}

> \
> Returns the names of columns in the result set of the prepared
> statement.

\
\[Function\]\
[**statement-column-value** *statement column-number* =\>
*result*]{#statement-column-value .none}

> \
> Returns the *column-number*-th column\'s value of the current row of
> the *statement*. Columns are numbered from zero.\
> Returns:\
>
> - NIL for NULL
> - integer for integers
> - double-float for floats
> - string for text
> - (simple-array (unsigned-byte 8)) for BLOBs

\
\[Function\]\
[**step-statement** *statement* =\> *boolean*]{#step-statement .none}

> \
> Steps to the next row of the resultset of *statement*.\
> Returns T is successfully advanced to the next row and NIL if there
> are no more rows.

\
\[Macro\]\
[**with-transaction** *db* `&body` *body*]{#with-transaction .none}

> \
> Wraps the *body* inside the transaction. If *body* evaluates without
> error, transaction is commited. If evaluation of *body* is
> interrupted, transaction is rolled back.

\
\[Macro\]\
[**with-open-database** (*db* *path* *&key* *busy-timeout*) `&body`
*body*]{#with-open-database .none}

> \
> Executes the *body* with *db* being bound to the database handle for
> database located at *path*. Database is open before the *body* is run
> and it is ensured that database is closed after the evaluation of
> *body* finished or interrupted.\
> See [CONNECT](#connect) for meaning of *busy-timeout* parameter.

\
 \

## Support

This package is written by [Kalyanov Dmitry](mailto:Kalyanov.Dmitry@gmail.com).\
This project has a [cl-sqlite-devel](http://common-lisp.net/mailman/listinfo/cl-sqlite-devel)
mailing list.\


## Changelog

- [23 Jan 2009]{style="color:gray"} **0.1** Initial version
- [03 Mar 2009]{style="color:gray"} **0.1.1** Fixed bug with access to
  recently freed memory during statement preparation
- [22 Mar 2009]{style="color:gray"} **0.1.2** [disconnect](#disconnect)
  function now ensures that all non-finalized statements are finalized
  before closing the database (otherwise errors are signaled when
  database is being closed).
- [28 Apr 2009]{style="color:gray"} **0.1.3** Added support for passing
  all values of type REAL (including RATIONAL) as query parameter.
  cl-sqlite is made available as git repository.
- [10 May 2009]{style="color:gray"} **0.1.4** Added test suite (based on
  [FiveAM](http://common-lisp.net/project/bese/FiveAM.html) testing
  framework); changed foreign library definition to work on Mac OS X
  (thanks to Patrick Stein) and removed the dependency on
  sqlite3_next_stmt function that appeared only in sqlite 3.6.0 (making
  cl-sqlite work with older sqlite versions)
- [13 June 2009]{style="color:gray"} **0.1.5** Allow passing pathnames
  to [CONNECT](#connect) function.
- [24 Oct 2009]{style="color:gray"} **0.1.6** Add busy-timeout argument
  to [CONNECT](#connect). Fix library defininitions for running on
  Microsoft Windows.
- [14 Nov 2010]{style="color:gray"} **0.2** Added support for named
  parameters. Made statement reset and connection close more safe by
  clearing statements\' bindings and unbinding slot of connection
  object. Added error condition for SQLite errors. Changes are courtesy
  of Alexander Gavrilov.
- [02 Aug 2019]{style="color:gray"} **0.2.1** Added metadata to system
  definitions. Fixed symbol conflict with FiveAM in tests. Project
  maintenance is now handled by Jacek Złydach.
- [29 Dec 2025] **0.3** Moved documentation to simpler MD
  format. Updated testes. Updated CFFI - Abhijit 'quasi' Rao


## Acknowledgements

