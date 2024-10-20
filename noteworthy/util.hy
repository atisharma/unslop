"
Utilities that don't fit anywhere else.
"

(require hyrule [of -> ->>]) 
(import hyjinx [config
                jload jsave
                filenames
                blog
                progress
                llm])

(import os)
(import pathlib [Path])
(import platformdirs [user-config-dir])


(defclass NoteWorthyError [RuntimeError])

;; * file and toml utilities
;; -----------------------------------------------------------------------------

(defn file-exists [path]
  "Return Path object if it exists as a file, otherwise None."
  (when (.exists path)
    path))

(defn find-toml-file [#^ str name]
  "Locate a toml file.
  It will look under, in order:
    `~/.config/noteworthy/`
    `$pwd/templates`
    `$module_dir/templates`
  "
  (let [fname (+ name ".toml")]
    (or
      (file-exists (Path (user-config-dir "noteworthy") fname))
      (file-exists (Path "templates" fname))
      (file-exists (Path (os.path.dirname __file__) "templates" fname))
      (raise (FileNotFoundError fname)))))

(defn load-config [#^ str [fname "config"]]
  "Load a config file.
  Defaults to `.config/noteworthy/config.toml`.
  See `find-toml-file` for search order."
  (config (find-toml-file fname)))

(defn chat-client [#^ str client]
  "Create a chat client object from the specification in the config file.
  See `hyjinx.llm` for methods and further documentation."
  (let [client-cfg (get (load-config) client)
        provider (.pop client-cfg "provider")
        model (.pop client-cfg "model" None)
        client (match provider
                 "anthropic" (llm.Anthropic #** client-cfg)
                 "openai" (llm.OpenAI #** client-cfg)
                 "tabby" (llm.TabbyClient #** client-cfg)
                 "huggingface" (llm.Huggingface #** client-cfg))]
    (when model
      (llm.model-load client model))
    client))

;; * utilities for munging json files
;; -----------------------------------------------------------------------------

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
          (print f"Error processing {fname}: {(str e)}"))))
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
        (print f"Error processing {fname}: {(str e)}"))))
  (print "\n\n\n\n"))

(defn process-json-field [f in-key out-key in-directory out-directory]
  "Apply function `f` to each value associated with `in-key`
  for every file in `directory` and write it back under
  `out-key` in `out-directory`."
  (for [fname (filenames in-directory)]
    (try
      (let [j (jload fname)
            v (.pop j in-key None)]
        (progress f"file: {fname}")
        (when v
          (jsave {#** j out-key (f v)} fname)))
      (except [e [Exception]]
        (print f"Error processing {fname}: {(str e)}"))))
  (print "\n\n\n\n"))

