(defpackage #:rag-backend-sql
  (:use #:cl)
  (:export #:sql-vector-store
           #:make-sql-vector-store
           #:use-sql-vector-store
           #:close-sql-vector-store
           #:ensure-sql-store-schema
           #:sql-store-connection
           #:sql-store-table
           #:sql-store-dimension))

(in-package #:rag-backend-sql)
