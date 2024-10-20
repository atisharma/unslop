"
Load and process parquet (or other) files with duckdb.
"

(require hyrule [of -> ->>]) 
(import hyjinx [first group
                mkdir
                short-id now
                progress
                jsave spit])

(import duckdb)
(import json)
(import pathlib [Path])

(import noteworthy.embeddings [token-count])


(defclass DbError [RuntimeError])


(defn duck-load [files * [query "SELECT * FROM read_parquet('{files}')"]]
  "Load parquet files (or other SQL query) and
  give a generator over rows returned.
  Each row is returned as a dict."
  (let [con (duckdb.connect ":memory:")
        cursor (.cursor con)
        row True]
    (.execute cursor (.format query :files files))
    (let [columns (lfor desc cursor.description (first desc))]
      (while row
        (yield (setx row (dict (zip columns (.fetchone cursor)))))))
    (.close cursor)
    (.close con)))

(defn duck-load-batch [files * [batch 1000] [query "SELECT * FROM read_parquet('{files}')"]]
  "Batch-load parquet files (or other SQL query) and
  give a generator over rows returned.
  Each row is returned as a dict."
  (let [con (duckdb.connect ":memory:")
        cursor (.cursor con)
        result True]
    (.execute cursor (.format query :files files))
    (let [columns (lfor desc cursor.description (first desc))]
      (while result
        (setx result (.fetchmany cursor batch))
        (for [row result]
          (yield (dict (zip columns row))))))
    (.close cursor)
    (.close con)))

(defn duck-load-json [files * [query "SELECT * FROM read_json('{files}')"]]
  "Load json files (or other SQL query) and
  give a generator over rows returned.
  Each row is returned as a dict."
  (duck-load files :query query))

(defn duck-load-batch-json [files * [batch 1000] [query "SELECT * FROM read_json('{files}')"]]
  "Batch-load json files (or other SQL query) and
  give a generator over rows returned.
  Each row is returned as a dict."
  (duck-load-batch files :batch batch :query query))

(defn duck-save [#^ (of list dict) data out-dir * [fmt "PARQUET"]]
  "Save a list of dicts as parquet."
  (duckdb.sql f"EXPORT DATABASE '{out-dir}' (FORMAT {fmt});"))

(defn chunk-parquet [files field * [rows 150]] 
  "Load a set of parquet files and yield in groups of `rows`."
  (for [batch (group (duck-load-batch files) rows)]
    (yield (lfor row batch
             :if row
             (get row field)))))


;; apply template over parquet?

(defn spit-from-parquet [files out-directory field * [rows 150]]
  "Load a set of parquet files save (:field row) from them as
  json chunks of `N` rows under out-dir."
  (let [total 0
        failed 0
        log (Path out-directory "spit-from-parquet.log")]
    (mkdir out-directory)
    (print "\n\n\n\n\n")
    (for [[n batch] (enumerate (chunk-parquet files field :rows rows))]
      (try
        (+= total (len batch))
        (let [text (.join "\n" batch)
              j {"id" (short-id text 8)
                 "added" (now)
                 "length" (token-count text)
                 "extract" text}]
          (progress
            (.join "\n"
              ["id: {id}"
               "tokens: {tokens}"
               "total rows: {total}"
               "total batches {n}"
               "failed: {failed}"])
            :id (:id j)
            :n n
            :tokens (:length j)
            :total total
            :failed failed)
          (jsave j (Path out-directory (+ (:id j) ".json"))))
        (except [e [RuntimeError]]
          (+= failed 1)
          (spit log f"{(now)} Error: {(repr e)}\n" :mode "a"))))))
