"
This module provides functions that generate text (markdown) from urls.
"

(require hyrule.argmove [-> ->>])

(require hyjinx [lmap])

(import hyrule [inc dec])
(import hyjinx [first
                spit slurp mkdir
                filenames
                is-url short-id
                progress])

(import os)
(import lxml)
(import re)
(import requests)
(import wikipedia :as wiki)

(import pathlib [Path])
(import urllib.parse [urlparse])
(import lxml-html-clean [Cleaner])
(import markdownify [markdownify])


(defclass WebError [RuntimeError])

(defn url [url]
  "Fetch a URL's content as cleaned markdown text."
  ;; could use trafilatura instead, but this is not bad and is light
  (if (is-url url)
      (let [response (.get requests url)
            cleaner (Cleaner :javascript True :style True)]
        (if response.ok
            (-> response.text
                (lxml.html.fromstring) 
                (cleaner.clean_html)
                (lxml.html.tostring)
                (markdownify :heading-style "ATX" :strip "style")
                (.replace "\r\n" "\n")
                (.replace "\r" "\n")
                (.strip))
            (.raise_for_status response)))
      (raise (WebError f"Fetching {url} failed (implausible url)."))))

(defn wikipedia [topic [index 0]]
  "Get the full Wikipedia page on a topic (as text)."
  (try
    (let [pages (wiki.search topic)
          best (get pages index)
          summary (wiki.summary best :auto-suggest False)
          page (wiki.page best :auto-suggest False)]
        (.join "\n"
               [f"### Wikipedia page {page.title}"
                f"Title: {page.title}"
                f"URL: {page.url}"
                f"{page.content}"
                "\nRelated wikipedia pages:"
                (.join ", " pages)]))
    (except [wiki.exceptions.DisambiguationError]
      (wikipedia topic :index (inc index)))))

(defn filename-from-url [url]
  "Sanitise a url into a filename."
  (let [parsed_url (urlparse url)
        netloc parsed_url.netloc
        path parsed_url.path
        fname f"{netloc}_{(os.path.basename path)}"]
    (+ (re.sub r"[^a-zA-Z0-9_.-]" "_" fname)
       "_" (short-id fname))))

(defn spit-urls-from-file [sources-fname out-directory]
  "Get a series of urls from a text file, one on each line,
  and save them under out-directory."
  (mkdir out-directory)
  (let [urls (set (.split (slurp sources-fname) "\n"))
        N (len urls)]
    (for [[n u] (enumerate urls)]
      (try
        (let [text (url (.strip u))
              out-fname (filename-from-url u)]
          (progress
            "url: {url}\nfile: {out_fname}\n#{n} / {N}"
            :url u
            :out-fname out-fname
            :n (inc n) :N N)
          (spit (Path out-directory (+ out-fname ".md")) text))
        (except [e [Exception]]
          (progress
            "url: {url}\nerror {error}\n#{n} / {N}"
            :url u
            :error (repr e)
            :n (inc n) :N N))))))

(defn clean-file [in-fname out-fname * [bad "#`|"]]
  "Rewrite a single text file with each line stripped,
  and (stripped) lines starting with bad characters removed.
  Save as out-fname."
  (spit out-fname
        (.replace 
          (.join "\n"
            (lfor line (.split (slurp in-fname) "\n")
              ;; allow blank lines, but not 'bad' lines
              :if (if line
                    (not (in (first (.strip line)) bad))
                    True)
                (.strip line)))
          "\n\n\n" "\n")))

(defn clean [fname-or-directory out-directory * [bad "#`|"]]
  "Rewrite text files in a directory with each line stripped,
  and lines starting with bad characters removed.
  Save them under out-directory."
  (mkdir out-directory)
  (for [in-fname (filenames fname-or-directory)]
    (let [out-fname (Path out-directory (. (Path in-fname) name))]
      (progress "input:  {in}\noutput: {out}"
        :in in-fname
        :out out-fname)
      (clean-file in-fname out-fname :bad bad))))
