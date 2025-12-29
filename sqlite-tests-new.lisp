(in-package :sqlite-tests)

(in-suite sqlite-suite)

(test test-transaction-commit
  (with-open-database (db ":memory:")
    (execute-non-query db "create table test (id integer primary key)")
    (with-transaction db
      (execute-non-query db "insert into test (id) values (1)"))
    (is (= 1 (execute-single db "select count(*) from test")))))

(test test-transaction-rollback
  (with-open-database (db ":memory:")
    (execute-non-query db "create table test (id integer primary key)")
    (ignore-errors
      (with-transaction db
        (execute-non-query db "insert into test (id) values (1)")
        (error "Something went wrong")))
    (is (= 0 (execute-single db "select count(*) from test")))))

(test test-blob
  (with-open-database (db ":memory:")
    (execute-non-query db "create table test (data blob)")
    (let ((data (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4 5))))
      (execute-non-query db "insert into test (data) values (?)" data)
      (let ((retrieved (execute-single db "select data from test")))
        (is (equalp data retrieved))))))

(test test-blob-large
  (with-open-database (db ":memory:")
    (execute-non-query db "create table test (data blob)")
    (let ((data (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 255)))
      (execute-non-query db "insert into test (data) values (?)" data)
      (let ((retrieved (execute-single db "select data from test")))
        (is (equalp data retrieved))))))
