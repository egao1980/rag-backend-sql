# rag-backend-sql

[`sql-protocol`](https://github.com/egao1980/sql-protocol) vector store for [`rag-protocol`](https://github.com/egao1980/rag-protocol). Persist chunks in SQLite / Postgres. Nearest-neighbor is **Lisp cosine**, not pgvector.

```lisp
(asdf:load-system "sql-backend-sqlite3")
(asdf:load-system "rag-backend-sql")

(let ((store (rag-backend-sql:make-sql-vector-store
              :driver :sqlite3 :database-name ":memory:")))
  (stack-rag:upsert store
                    (stack-rag:make-rag-chunk
                     :id "a" :text "alpha" :embedding #(1.0 0.0)))
  (stack-rag:query-store store #(1.0 0.0) :top-k 5)
  (rag-backend-sql:close-sql-vector-store store))
```

Pass an existing `sql-connection` with `:connection` if you already own the handle. Table default `rag_chunks` (`[A-Za-z_][A-Za-z0-9_]*` only). Dimension is fixed on first upsert and reloaded from the table.

Not here: pgvector, hybrid BM25, dialect `ON CONFLICT`.

Part of [cl-stack](https://github.com/egao1980/cl-stack). Cookbook: [rag.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/rag.md).

## License

MIT — see [LICENSE](LICENSE).
