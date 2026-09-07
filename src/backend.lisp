(in-package #:rag-backend-sql)

;;; Persist chunks via sql-protocol. Nearest-neighbor is Lisp cosine
;;; (same as rag-backend-memory). pgvector / dialect UPSERT stay out.

(defclass sql-vector-store (rag-protocol:rag-vector-store)
  ((connection :initarg :connection :accessor sql-store-connection)
   (owns-connection :initarg :owns-connection :accessor sql-store-owns-connection
                    :initform nil)
   (table :initarg :table :accessor sql-store-table :initform "rag_chunks")
   (dimension :initarg :dimension :accessor sql-store-dimension :initform nil)))

(defun %table-name (name)
  (let ((s (string name)))
    (unless (and (plusp (length s))
                 (alpha-char-p (char s 0))
                 (every (lambda (c)
                          (or (alphanumericp c) (char= c #\_)))
                        s))
      (error 'rag-protocol:rag-error
             :message (format nil "invalid table name ~s" name)))
    s))

(defun %as-list (x)
  (if (listp x) x (list x)))

(defun %encode-lisp (value)
  (with-standard-io-syntax
    (let ((*print-readably* t)
          (*print-pretty* nil)
          (*package* (find-package :cl)))
      (prin1-to-string value))))

(defun %decode-lisp (string)
  (when (and string (plusp (length string)))
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package :cl)))
        (read-from-string string)))))

(defun %encode-embedding (vec)
  (%encode-lisp (map 'list (lambda (x) (float x 1d0)) vec)))

(defun %decode-embedding (string)
  (map 'vector (lambda (x) (float x 1f0)) (%decode-lisp string)))

(defun %encode-metadata (meta)
  (when meta
    (%encode-lisp meta)))

(defun %decode-metadata (string)
  (%decode-lisp string))

(defun %exec (store sql &optional params)
  (sql-protocol:execute (sql-store-connection store) sql params))

(defun %fetch (store sql &optional params)
  (sql-protocol:fetch (%exec store sql params)))

(defun %fetch-all (store sql &optional params)
  (sql-protocol:fetch-all (%exec store sql params)))

(defun ensure-sql-store-schema (store)
  (let ((table (%table-name (sql-store-table store))))
    (%exec store
           (format nil
                   "CREATE TABLE IF NOT EXISTS ~a (
  id TEXT PRIMARY KEY,
  document_id TEXT,
  text TEXT NOT NULL,
  embedding TEXT NOT NULL,
  metadata TEXT,
  dim INTEGER NOT NULL)"
                   table))
    (unless (sql-store-dimension store)
      (let ((row (%fetch store (format nil "SELECT dim FROM ~a LIMIT 1" table))))
        (when row
          (setf (sql-store-dimension store) (getf row :dim)))))
    store))

(defun make-sql-vector-store (&key connection
                                   (driver :sqlite3)
                                   database-name
                                   (table "rag_chunks")
                                   dimension
                                   (ensure-schema t))
  (let* ((owns (null connection))
         (conn (or connection
                   (apply #'sql-protocol:connect
                          :driver driver
                          (when database-name
                            (list :database-name database-name)))))
         (store (make-instance 'sql-vector-store
                               :connection conn
                               :owns-connection owns
                               :table (%table-name table)
                               :dimension dimension)))
    (when ensure-schema
      (ensure-sql-store-schema store))
    store))

(defun use-sql-vector-store (&rest args &key &allow-other-keys)
  (setf rag-protocol:*rag-store* (apply #'make-sql-vector-store args)))

(defun close-sql-vector-store (store)
  (when (and (sql-store-owns-connection store)
             (sql-store-connection store))
    (ignore-errors (sql-protocol:disconnect (sql-store-connection store)))
    (setf (sql-store-connection store) nil
          (sql-store-owns-connection store) nil))
  store)

(defun %accepted-embedding (store chunk)
  (let ((emb (rag-protocol:rag-chunk-embedding chunk)))
    (unless (and emb (plusp (length emb)))
      (error 'rag-protocol:rag-error
             :message (format nil "chunk ~s has no embedding"
                              (rag-protocol:rag-chunk-id chunk))))
    (tagbody
     :retry
       (let ((dim (length emb)))
         (cond
           ((null (sql-store-dimension store))
            (setf (sql-store-dimension store) dim)
            (return-from %accepted-embedding emb))
           ((= dim (sql-store-dimension store))
            (return-from %accepted-embedding emb))
           (t
            (restart-case
                (error 'rag-protocol:rag-dimension-mismatch
                       :expected (sql-store-dimension store)
                       :actual dim
                       :id (rag-protocol:rag-chunk-id chunk)
                       :message (format nil "chunk ~s: expected dim ~d, got ~d"
                                        (rag-protocol:rag-chunk-id chunk)
                                        (sql-store-dimension store)
                                        dim))
              (continue ()
                :report "Skip this chunk"
                (return-from %accepted-embedding nil))
              (use-value (value)
                :report "Use a supplied embedding vector"
                (setf emb value
                      (rag-protocol:rag-chunk-embedding chunk) value)
                (go :retry)))))))))

(defun %chunk-from-row (row)
  (rag-protocol:make-rag-chunk
   :id (getf row :id)
   :document-id (getf row :document_id)
   :text (or (getf row :text) "")
   :embedding (%decode-embedding (getf row :embedding))
   :metadata (%decode-metadata (getf row :metadata))))

(defmethod rag-protocol:upsert ((store sql-vector-store) chunks)
  (let ((table (%table-name (sql-store-table store))))
    (sql-protocol:with-transaction ((sql-store-connection store))
      (dolist (ch (%as-list chunks))
        (let ((emb (%accepted-embedding store ch)))
          (when emb
            (unless (rag-protocol:rag-chunk-id ch)
              (error 'rag-protocol:rag-error :message "chunk id required for upsert"))
            (let ((id (rag-protocol:rag-chunk-id ch)))
              (%exec store (format nil "DELETE FROM ~a WHERE id = ?" table) (list id))
              (%exec store
                     (format nil
                             "INSERT INTO ~a (id, document_id, text, embedding, metadata, dim)
VALUES (?, ?, ?, ?, ?, ?)"
                             table)
                     (list id
                           (rag-protocol:rag-chunk-document-id ch)
                           (rag-protocol:rag-chunk-text ch)
                           (%encode-embedding emb)
                           (%encode-metadata (rag-protocol:rag-chunk-metadata ch))
                           (sql-store-dimension store)))))))))
  store)

(defmethod rag-protocol:delete-ids ((store sql-vector-store) ids)
  (let* ((table (%table-name (sql-store-table store)))
         (ids (%as-list ids))
         (missing '())
         (deleted '()))
    (sql-protocol:with-transaction ((sql-store-connection store))
      (dolist (id ids)
        (if (%fetch store (format nil "SELECT id FROM ~a WHERE id = ?" table) (list id))
            (progn
              (%exec store (format nil "DELETE FROM ~a WHERE id = ?" table) (list id))
              (push id deleted))
            (push id missing))))
    (setf missing (nreverse missing)
          deleted (nreverse deleted))
    (when missing
      (restart-case
          (error 'rag-protocol:rag-not-found
                 :ids missing
                 :message (format nil "unknown chunk ids: ~s" missing))
        (continue ()
          :report "Skip missing ids"
          (return-from rag-protocol:delete-ids deleted))
        (use-value (value)
          :report "Return a supplied value"
          (return-from rag-protocol:delete-ids value))))
    deleted))

(defmethod rag-protocol:query-store ((store sql-vector-store) query &key top-k filter)
  (let ((vec (rag-protocol:query-vector query))
        (table (%table-name (sql-store-table store)))
        (hits '()))
    (when (and (sql-store-dimension store)
               (/= (length vec) (sql-store-dimension store)))
      (error 'rag-protocol:rag-dimension-mismatch
             :expected (sql-store-dimension store)
             :actual (length vec)
             :message (format nil "query dim ~d, store dim ~d"
                              (length vec) (sql-store-dimension store))))
    (dolist (row (%fetch-all store
                             (format nil
                                     "SELECT id, document_id, text, embedding, metadata, dim FROM ~a"
                                     table)))
      (let ((chunk (%chunk-from-row row)))
        (when (or (null filter) (funcall filter chunk))
          (push (rag-protocol:make-rag-hit
                 :chunk chunk
                 :score (rag-protocol:cosine-similarity
                         vec (rag-protocol:rag-chunk-embedding chunk)))
                hits))))
    (rag-protocol:rerank (rag-protocol:make-identity-reranker)
                         query hits :top-k (or top-k 5))))
