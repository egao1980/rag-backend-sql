(in-package #:rag-backend-sql/tests)

(defun %vec (&rest xs)
  (map 'vector (lambda (x) (float x 1f0)) xs))

(defun %chunk (id text emb &key (document-id "d"))
  (rag-protocol:make-rag-chunk :id id :document-id document-id
                               :text text :embedding emb))

(defmacro with-sql-store ((store &rest args) &body body)
  `(let ((,store (rag-backend-sql:make-sql-vector-store
                  :driver :sqlite3 :database-name ":memory:" ,@args)))
     (unwind-protect (progn ,@body)
       (rag-backend-sql:close-sql-vector-store ,store))))

(deftest use-sql-binds
  (let ((rag-protocol:*rag-store* nil)
        (store nil))
    (unwind-protect
         (progn
           (setf store (rag-backend-sql:use-sql-vector-store
                        :driver :sqlite3 :database-name ":memory:"))
           (ok (typep rag-protocol:*rag-store* 'rag-backend-sql:sql-vector-store)))
      (when store
        (rag-backend-sql:close-sql-vector-store store)
        (setf rag-protocol:*rag-store* nil)))))

(deftest upsert-query-ranks
  (with-sql-store (store)
    (rag-protocol:upsert store
                         (list (%chunk "a" "alpha" (%vec 1 0))
                               (%chunk "b" "beta" (%vec 0 1))))
    (let ((hits (rag-protocol:query-store store (%vec 1 0) :top-k 2)))
      (ok (= 2 (length hits)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits)))))
      (ok (> (rag-protocol:rag-hit-score (first hits))
             (rag-protocol:rag-hit-score (second hits)))))))

(deftest persist-across-reconnect-same-db
  (uiop:with-temporary-file (:pathname path :prefix "rag-sql-" :type "sqlite")
    (let ((store (rag-backend-sql:make-sql-vector-store
                  :driver :sqlite3 :database-name (namestring path))))
      (rag-protocol:upsert store (%chunk "a" "keep" (%vec 1 0)))
      (rag-backend-sql:close-sql-vector-store store))
    (let ((store (rag-backend-sql:make-sql-vector-store
                  :driver :sqlite3 :database-name (namestring path))))
      (let ((hits (rag-protocol:query-store store (%vec 1 0) :top-k 1)))
        (ok (equal "keep" (rag-protocol:rag-chunk-text
                           (rag-protocol:rag-hit-chunk (first hits)))))
        (ok (= 2 (rag-backend-sql:sql-store-dimension store))))
      (rag-backend-sql:close-sql-vector-store store))))

(deftest replace-same-id
  (with-sql-store (store)
    (rag-protocol:upsert store (%chunk "a" "old" (%vec 1 0)))
    (rag-protocol:upsert store (%chunk "a" "new" (%vec 0 1)))
    (let ((hits (rag-protocol:query-store store (%vec 0 1) :top-k 1)))
      (ok (equal "new" (rag-protocol:rag-chunk-text
                        (rag-protocol:rag-hit-chunk (first hits))))))))

(deftest delete-and-missing
  (with-sql-store (store)
    (rag-protocol:upsert store (%chunk "a" "x" (%vec 1 0)))
    (ok (equal '("a") (rag-protocol:delete-ids store '("a"))))
    (ok (signals (rag-protocol:delete-ids store "a")
                 'rag-protocol:rag-not-found))))

(deftest dimension-mismatch
  (with-sql-store (store)
    (rag-protocol:upsert store (%chunk "a" "x" (%vec 1 0)))
    (ok (signals (rag-protocol:upsert store (%chunk "b" "y" (%vec 1 0 0)))
                 'rag-protocol:rag-dimension-mismatch))
    (ok (signals (rag-protocol:query-store store (%vec 1 0 0) :top-k 1)
                 'rag-protocol:rag-dimension-mismatch))))

(deftest query-filter
  (with-sql-store (store)
    (rag-protocol:upsert store
                         (list (%chunk "a" "keep" (%vec 1 0))
                               (%chunk "b" "drop" (%vec 1 0))))
    (let ((hits (rag-protocol:query-store
                 store (%vec 1 0) :top-k 5
                 :filter (lambda (ch)
                           (equal "keep" (rag-protocol:rag-chunk-text ch))))))
      (ok (= 1 (length hits)))
      (ok (equal "a" (rag-protocol:rag-chunk-id
                      (rag-protocol:rag-hit-chunk (first hits))))))))

(deftest invalid-table-name
  (ok (signals (rag-backend-sql:make-sql-vector-store
                :driver :sqlite3 :database-name ":memory:"
                :table "chunks;drop")
               'rag-protocol:rag-error)))
