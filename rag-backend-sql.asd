(defsystem "rag-backend-sql"
  :version "0.1.0"
  :description "sql-protocol vector store for rag-protocol (Lisp cosine, not pgvector)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("rag-protocol" "sql-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "rag-backend-sql/tests"))))

(defsystem "rag-backend-sql/tests"
  :depends-on ("rag-backend-sql" "sql-backend-sqlite3" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
