"
Data preparation: functions that clean strings and datasets / message lists.
"

(require hyrule [of unless
                 -> ->>]) 
(require hyjinx [defmethod rest])

(import hyjinx [first last
                progress
                filenames slurp
                jload])

(import json)
(import re)
(import tomllib)
(import pathlib [Path])


;; common slop phrases
(let [sloplists (tomllib.loads (slurp (Path (. (Path __file__) parent) "slop.toml")))]
  (setv slop (:antislop sloplists)))


;; * Pipelines
;; -----------------------------------------------------------------------------

(defn slop-score [j [in-key "extract"] [out-key "perspective"]]
  "A heuristic to measure slop.
  Twice the ratio of slop in the output (out-key of json j),
  to the input (in-key of json j) plus output,
  with a offset to act as a penalty for a small sample.
  High (much over 1.0) indicates a slop factory or short sample."
  (let [in-count (slop-count (.get j in-key ""))
        out-count (slop-count (.get j out-key ""))]
    (* 2 (/ (+ 10 out-count)
            (+ 1e-2 in-count out-count)))))

(defmethod clean [#^ str s * [first-speaker "{user}:"] [second-speaker "{narrator}:"]]
  "The full cleaning pipeline (str->str)."
  (-> s
    defence
    (alternate-speaker)
    str-to-messages
    sharegpt
    start-end-speaker))

(defmethod clean [#^ str in-directory #^ str out-fname *
                  [in-key "extract"] [out-key "perspective"]
                  [extract-quality ["good" "excellent" "outstanding"]]
                  [rating ["good" "excellent" "outstanding"]]
                  [bbfc-rating ["U" "PG" "12" "15" "18" "R18" "BAN"]]
                  [slop-threshold 1.3]]
  "The full cleaning pipeline applied to all json files in a directory.
  Consolidates them into a jsonl file."
  (for [[n f] (enumerate (filenames in-directory))]
    (try
      (let [j (jload f)
            q (:extract-quality j "unrated")
            r (:rating j "unrated")
            bbfc (:bbfc-rating j "unrated")
            out-value (.get j out-key "")
            approve (and out-value
                         (in r rating)
                         (in q extract-quality)
                         (in bbfc bbfc-rating))]
        (progress (.join "\n"
                    ["{in_dir} -> {out_fname}"
                     "id: {id}, chunk {n}"
                     "quality: {q}"
                     "rating: {r}"
                     "BBFC: {bbfc}"
                     "approved: {approve}"])
                  :in-dir in-directory
                  :out-fname out-fname
                  :n n :q q :r r :bbfc bbfc
                  :id (:id j None)
                  :approve approve)
        ;; defer slop calculation because it's the slowest step
        ;; using short-circuit property of and
        (when (and approve
                   (< (slop-score j :in-key in-key :out-key out-key) slop-threshold))
          ;; It is inefficient to open the file again each time
          ;; to write to it, but if your datasets are <10K or so
          ;; lines, it doesn't really matter.
          (jsonl-append f"{out-fname}.jsonl" (clean out-value))
          (jsonl-append f"{out-fname}_{bbfc}.jsonl" (clean out-value))))
      (except [e [json.JSONDecodeError IndexError Exception]]))))

;; * write output
;; -----------------------------------------------------------------------------

(defn jsonl-append [fname messages]
  "Append the messages list as one line of jsonl."
  (with [f (open fname "a")]
    (f.write (+ (json.dumps messages) "\n"))))

;; * Clean up common errors in template output strings
;; -----------------------------------------------------------------------------

(defmethod defence [#^ str s]
  "Remove markdown code fences."
  (.join "\n"
    (lfor l (.split s "\n")
      :if (not (.startswith (.strip l) "```"))
      l)))


(defmethod alternate-speaker [#^ str s]
  (let [pattern (re.compile r"(\{(user|narrator)\}:.*?)(\n\1:.*?)(?=\n(\{(?:user|narrator)\}:))" re.DOTALL)]
    (while True
      (setv new-s (pattern.sub r"\1\n\n\2" s))
      (when (= new-s s)
        (break))
      (setv s new-s))
    s))
  

(defmethod slop-count [#^ str s]
  "Number of matches against slop phrases."
  (sum (lfor word slop
         (if (in word s) 1 0))))

;; * Deal with sharegpt message format
;; -----------------------------------------------------------------------------

(defmethod str-to-messages [#^ str s]
  "Convert string of messages to dict of messages (OpenAI format).
  Also remove bolding of speaker that sometimes is added by a model."
  (let [messages []]
    (for [line (.split s "\n")]
      (let [speaker (re.match r"^[\s\*]*\{([a-zA-Z]*)\}[:\s\*]*" line)]
        (if speaker
          (.append messages
            {"role" (speaker.group 1)
             "content" (re.sub r"^[\s\*]*\{[a-zA-Z]*\}[:\s\*]*" "" line :count 1)})
          (setv (get messages -1)
                (| (last messages)
                   {"content" (+ (:content (last messages)) "\n" line)})))))
    ;; remove trailing whitespace on message content
    (lfor msg messages
      (dfor [k v] (.items msg) k (.strip v)))))
                                                 
(defmethod start-end-speaker [#^ (of list dict) messages]
  "Ensure the first speaker (after optional system) is human and last is gpt."
  (let [human-value "I wait."
        last-from (:from (last messages))]
    ;; last message should be gpt
    (unless (= last-from "gpt")
      (setv messages (cut messages 0 -1)))
    (let [first-from (:from (first messages))
          second-from (:from (get messages 1))]
      ;; first message should be human or system
      (if (= first-from "system")
        (if (= second-from "human")
          messages
          [(first messages) ; system msg
           {"from" "human" "value" human-value} ; insert human message
           #* (rest messages)])
        (if (= first-from "gpt")
          [{"from" "human" "value" human-value} ; insert human message
           #* messages]
          messages)))))

(defmethod sharegpt [#^ (of list dict) messages]
  "Translate from OpenAI's user/assistant roles
  to the human/gpt sharegpt format."
  (lfor m messages
    (match (:role m)
      "system" {"from" "system" "value" (:content m)}
      "user" {"from" "human" "value" (:content m)}
      "assistant" {"from" "gpt" "value" (:content m)}
      "narrator" {"from" "gpt" "value" (:content m)})))
