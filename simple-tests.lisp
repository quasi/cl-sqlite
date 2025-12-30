(in-package :sqlite-tests)

(test test-simple-create-table
  (with-open-database (db ":memory:")
    (create-table db :users
                  '((:id :integer :primary-key :autoincrement)
                    (:name :text :not-null)
                    (:age :integer)))
    (execute-non-query db "INSERT INTO users (name, age) VALUES ('Alice', 30)")
    (is (equal (execute-single db "SELECT name FROM users WHERE id = 1") "Alice"))))

(test test-simple-insert
  (with-open-database (db ":memory:")
    (create-table db :users '((:name :text) (:age :integer)))
    (insert db :users '(:name "Bob" :age 25))
    (is (equal (execute-single db "SELECT name FROM users") "Bob"))
    (is (= (execute-single db "SELECT age FROM users") 25))))

(test test-simple-select
  (with-open-database (db ":memory:")
    (create-table db :users '((:name :text) (:age :integer)))
    (insert db :users '(:name "Alice" :age 30))
    (insert db :users '(:name "Bob" :age 25))
    (insert db :users '(:name "Charlie" :age 35))

    (is (equal (length (select db :users)) 3))
    (is (equal (select db :users :where '(:= :name "Alice") :columns '(:age))
               '((30))))
    (is (equal (select db :users :where '(:> :age 28) :order-by '(:age :desc))
               '(("Charlie" 35) ("Alice" 30)))) ;; Charlie 35, Alice 30
    ))

(test test-simple-select-columns
  (with-open-database (db ":memory:")
    (create-table db :users '((:name :text) (:age :integer)))
    (insert db :users '(:name "Alice" :age 30))
    (is (equal (select db :users :columns '(:age :name))
               '((30 "Alice"))))))

(test test-simple-update
  (with-open-database (db ":memory:")
    (create-table db :users '((:name :text) (:age :integer)))
    (insert db :users '(:name "Alice" :age 30))
    (update-table db :users '(:age 31) :where '(:= :name "Alice"))
    (is (= (execute-single db "SELECT age FROM users WHERE name = 'Alice'") 31))))

(test test-simple-delete
  (with-open-database (db ":memory:")
    (create-table db :users '((:name :text) (:age :integer)))
    (insert db :users '(:name "Alice" :age 30))
    (insert db :users '(:name "Bob" :age 25))
    (delete-from db :users :where '(:= :name "Alice"))
    (is (= (execute-single db "SELECT count(*) FROM users") 1))
    (is (equal (execute-single db "SELECT name FROM users") "Bob"))))

(test test-simple-where-clauses
  (with-open-database (db ":memory:")
    (create-table db :items '((:id :integer) (:val :integer)))
    (insert db :items '(:id 1 :val 10))
    (insert db :items '(:id 2 :val 20))
    (insert db :items '(:id 3 :val 30))

    (is (= (length (select db :items :where '(:and (:> :val 15) (:< :val 25)))) 1))
    (is (= (length (select db :items :where '(:or (:= :id 1) (:= :id 3)))) 2))
    (is (= (length (select db :items :where '(:in :id 1 2))) 2))
    (is (= (length (select db :items :where '(:not (:= :id 2)))) 2))))

(test test-simple-error-handling
  (with-open-database (db ":memory:")
    (create-table db :items '((:id :integer)))
    (signals error (select db :items :where '(:unknown-op :id 1)))))
