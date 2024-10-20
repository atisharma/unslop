"
Apply LLM prompts to strings, files, or stdin.
"

(require hyjinx.macros [defmethod lmap])

(import sys)
(import pathlib [Path])
(import jinja2)

(import hyjinx [llm
                config
                inc first
                progress
                filenames
                now
                mkdir
                spit slurp blog
                extract-json jload jsave])


(import unslop.embeddings [token-count])
(import unslop.util [file-exists find-toml-file load-config chat-client])


(defclass TemplateError [RuntimeError])


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
    (for [[n f] (enumerate (filenames in-directory))]
      (try
        (let [j (jload f)
              output (if (or overwrite
                             (not (in template_name j)))
                       (apply client config-file template-name :llm-args llm-args #** j)
                       (do
                         (+= skipped 1)
                         (blog log f"Skipped {f}: {template-name} exists")
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
                    "client: {client}"
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
              :client client
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
            (blog log f"Wrote {f}"))
        (except [e [Exception]]
          (+= failed 1)
          (blog log f e))))
    (print "\n\n\n\n\n\n\n")))
