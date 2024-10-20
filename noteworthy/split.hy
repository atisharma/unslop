"
Split plain text documents on sensible boundaries into chunks for processing.

The split-file function reads a text file and writes a series of files of chunks.
The split function does the same for either a file or all files under the given directory.
"

(import logging)

(require hyrule [of -> ->>]) 
(require hyjinx [rest]) 

(import hyjinx [first
                flatten chain
                progress
                short-id now filenames
                mkdir
                jsave
                blog slurp])

(import re)
(import json)
(import pathlib [Path])

(import noteworthy.embeddings [token-count])


(defclass SplitError [RuntimeError])

;; the separators on which we split plain text files/strings.
(setv text-separators
      ["\n\n\n\n" "\n\n\n" ; plain text, high-level
       "\n# " "\n## " "\n#{1,6} " "```\n" "\n\\*\\*\\*+\n" "\n---+\n" "\n___+\n"  ; markdown
       "\n=+\n" "\n-+\n" "\n\\*+\n" "\n\n.. *\n\n"  ; rst
       "\n\n" "\n" "\t" " " ""])   ; plain text, lower-level

(defn chunk [#^ str text * #^ (of list str) [separators text-separators] [length None]]
  "Recursively bisect text on separator characters
  until each chunk is under the appropriate token length (default 2500)."
  (flatten
    (if (>= (token-count text)
            (or length 2500))
      (let [sep (first separators)
            occurrences (lfor m (re.finditer sep text) (.start m))
            point (if occurrences
                      (get occurrences (// (len occurrences) 2))
                      0)
            sections [(cut text point) (cut text point None)]]
        ;; if the split works, go down the tree, otherwise try the next separator
        (if point
            (lfor s sections :if s (chunk s :separators separators :length length))
            (chunk text :separators (rest separators) :length length)))
      [(.strip text)])))

(defn split-file [fname * [length None]]
  "Split according to file type (with error handling).
  Return generator of dicts with extract and metadata."
  (try
    (gfor c (chunk (slurp fname) :length length)
      {"source" fname
       "added" (now)
       "id" (short-id c 8)
       "length" (token-count c)
       "extract" c})
    (except [e [Exception]]
      {"source" fname
       "added" (now)
       "length" 0
       "id" "ERR"
       "extract" None
       "error" f"Error while loading {fname}: {(repr e)}."})))

(defn split [fname-or-directory * [length None]]
  "Create a generator of dicts for a file,
  or from all files under a directory (recursively)."
  (cond
    (.is-file (Path fname-or-directory))
    (split-file fname-or-directory :length length)

    ;; There is some edge case in which this returns the chained
    ;; dict instead, but I don't yet know when it is.
    (.is-dir (Path fname-or-directory))
    (chain.from-iterable
      (map (fn [f] (split-file f :length length))
          (filenames fname-or-directory)))

    :else
    (raise (FileNotFoundError fname-or-directory))))

(defn spit-split [fname-or-directory out-directory * [length None]]
  "Create json files of chunks for a file,
  or from all files under a directory (recursively).
  Save them under `out-directory`."
  (let [total 0
        failed 0
        log (Path out-directory (+ fname-or-directory ".log"))]
    (mkdir out-directory)
    (for [[n j] (enumerate (split fname-or-directory :length length))]
      (try
        (+= total (:length j))
        (progress
          (.join "\n"
            ["source: {source}"
             "id: {id}, chunk {n}"
             "tokens: {tokens}"
             "total: {total}"
             "failed: {failed}"])
          :source (:source j)
          :id (:id j)
          :n n
          :tokens (:length j)
          :total total
          :failed failed)
        (jsave j (Path out-directory (+ (:id j) ".json")))
        (except [e [Exception]]
          (+= failed 1)
          (blog log e))))
    (print "\n\n\n\n\n")))
