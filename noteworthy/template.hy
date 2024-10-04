"
Apply LLM prompts to strings, files, or stdin.
"

(require hyjinx.macros [defmethod lmap])

(import sys)
(import os)
(import platformdirs [user-config-dir])
(import pathlib [Path])
(import jinja2)

(import hyjinx [llm config
                inc first
                progress
                now filenames
                mkdir
                spit slurp
                extract-json jload jsave])


(import pansi [ansi])

(import noteworthy.embeddings [token-count])


(defclass TemplateError [RuntimeError])

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
  (let [client-cfg (get (load-config) client)
        provider (.pop client-cfg "provider")
        model (.pop client-cfg "model" None)
        client (match provider
                 "anthropic" (llm.Anthropic #** client-cfg)
                 "openai" (llm.OpenAI #** client-cfg)
                 "tabby" (llm.TabbyClient #** client-cfg))]
    (when model
      (llm.model-load client model))
    client))

(defn load-template-str [#^ str config-file #^ str template-name]
  "Load a template with name `config-file.toml` from the config directory."
  (try
    (get (load-config config-file) template-name)
    (except [KeyError]
      (raise (TemplateError f"Template {template-name} not found in {config-file}.")))))

(defn apply-jinja [#^ str client #^ str config-file #^ str template-name *
                   [echo False]
                   [image None]
                   [system-prompt None]
                   [llm-args {}]
                   #** kwargs]
  "Load the named jinja template.
  Optionally, you may specify a system prompt in the config file as `system_prompt`.
  Apply LLM."
  (let [jenv (jinja2.Environment)
        template (jenv.from-string (load-template-str config-file template-name))
        prompt (template.render #** kwargs)
        system-prompt (or system-prompt (:system-prompt (load-config config-file) None))
        msgs []]
    ;; TODO handle list of messages
    (llm.instruct (chat-client client) prompt
                  :system-prompt system-prompt 
                  :image image 
                  :echo echo 
                  #** llm-args)))

(defn apply [#^ str client #^ str config-file #^ str template-name *
             [echo False]
             [image None]
             [system-prompt None]
             [llm-args {}]
             #** kwargs]
  "Load the named template and apply python string `.format` method.
  Replace each `{kwarg}` with its value to form the one-shot user prompt.
  Optionally, you may specify a system prompt in the config file as `system_prompt`.
  Apply LLM."
  (let [template (load-template-str config-file template-name)
        prompt (.format template #** kwargs)
        system-prompt (or system-prompt (:system-prompt (load-config config-file) None))]
    (llm.instruct (chat-client client) prompt
                  :system-prompt system-prompt 
                  :image image 
                  :echo echo 
                  #** llm-args)))

(defn map-dir [#^ str client
               #^ str config-file
               #^ str template-name
               in-directory
               out-directory
               *
               [llm-args {}]
               [overwrite False]]
  "Apply the template to all (json) files in `in-directory`.
  Save the output in `out-directory`."
  (let [total 0
        failed 0
        skipped 0
        log (Path out-directory (+ template_name ".log"))]
    (mkdir out-directory)
    (spit log (+ (now) "\n"))
    (print "\n\n\n\n\n\n\n")
    (for [[n f] (enumerate (filenames in-directory))]
      (try
        (let [j (jload f)
              output (if (or overwrite
                             (not (in template_name j)))
                       (apply client config-file template-name :llm-args llm-args #** j)
                       (do
                         (+= skipped 1)
                         (spit log f"Skipped {f}: {template-name} exists\n" :mode "a")
                         (continue)))
              output-length (token-count output)
              json-output (extract-json output)
              ;; lists are also valid json, but can't be unpacked in this context.
              output-dict (if (isinstance json-output dict)
                            json-output
                            {})]
            (+= total output-length)
            (progress
              (.join "\n"
                (+ ["source: {source}"
                    "id: {id}, chunk {n}"
                    "in tokens: {length}"
                    "out tokens: {out}"
                    "{template_name}: {output}"
                    "total output tokens: {total}"
                    "wrote: {wrote}"
                    "failed: {failed}"
                    "skipped: {skipped}"]
                   (lfor [k v] (.items output-dict)
                     f"{k}: {v}")))
              :n n
              :out output-length
              :total total
              :output (str output)
              :template-name template-name
              :wrote (- (inc n) failed skipped)
              :failed failed
              :skipped skipped
              #** j)
            (jsave {#** j template-name output #** json-output} (Path out-directory (+ (:id j) ".json")))
            (spit log f"Wrote {f}\n" :mode "a"))
        (except [e [Exception]]
          (+= failed 1)
          (spit log f"Error {f}: {(repr e)}\n" :mode "a"))))))

(defn nw []
  "Load a config file that defines templates (first arg).
  Apply the named template (second arg) to stdin, which will replace `{{stdin}}` in the template."
  (apply-template (get sys.argv 1) (get sys.argv 2) :stdin (sys.stdin.read)))
