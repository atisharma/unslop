"
Utilities that don't fit anywhere else.
"

(import hyjinx [jload jsave filenames progress])

(import pathlib [Path])


(defn filter-json [key value in-directory]
  "Return list of path objects of json files under `in-directory`
  for which the value of `key` is `value`." 
  (filter None
          (lfor fname (filenames in-directory)
            (try
              (let [j (jload fname)]
                (when (= value (.get j key None))
                  fname))
              (except [e [Exception]]
                None)))))
          
(defn move-json [key value in-directory out-directory]
  "Copy json files under `in-directory` to `out-directory`
  when the value of `key` is `value`." 
  ;; realise the list first so we don't step on our own toes
  (let [failed 0
        total 0
        fnames (filter-json key value in-directory)]
    (for [fname fnames]
      (try
        (let [j (jload fname)]
          (progress
            (.join "\n"
              ["file: {fname}"
               "{key}: {value}"
               "total: {total}"
               "failed: {failed}"])
            :fname fname
            :key key
            :value value
            :total total
            :failed failed)
          (when (= value (.get j key None))
            (jsave j (Path out-directory (. (Path fname) name)))))
        (except [e [Exception]]
          (+= failed 1)
          (print f"Error processing: {fname} {(repr e)}"))))
    (print "\n\n\n\n")))
          
(defn delete-json-key [key directory]
  "Remove the key-value pair in json files under `directory`."
  (for [fname (filenames directory)]
    (try
      (let [j (jload fname)]
        (progress f"file: {fname}")
        (when (.pop j key None)
          (jsave j fname)))
      (except [e [Exception]]
        (print f"Error processing: {fname} {(repr e)}"))))
  (print "\n\n\n\n"))
          
