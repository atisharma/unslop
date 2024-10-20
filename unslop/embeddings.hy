"
Stuff to do with tokens and embeddings.
"

(import tiktoken)


;; the default encoder / tokenizer
(setv default-tokenizer (tiktoken.get-encoding "cl100k_base"))


(defn token-count [x * [tokenizer default-tokenizer]]
  "Return the number of embedding tokens, roughly, of x
  (anything with a meaningful __str__ or __repr__).
  The tokenizer defaults to tiktoken's cl100k_default,
  because it does not require pytorch."
  (len (tokenizer.encode (str x))))

