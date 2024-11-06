"
This module provides functions that generate text (markdown)
from a variety of non-plaintext sources on the web.
"

(require hyrule.argmove [-> ->>])

(import hyrule [inc dec])
(import hyjinx.lib [first is-url now])

(import httpx)
(import locale)
(import lxml)
(import os)
(import re)

(import lxml-html-clean [Cleaner])
(import markdownify [markdownify])
(import urllib.parse [urlparse])

(import arxiv [Search :as arxiv-search])
(import wikipedia :as wiki)
(import youtube_transcript_api [YouTubeTranscriptApi])
(import youtube_transcript_api.formatters [TextFormatter])
(import youtube_transcript_api._errors [TranscriptsDisabled])

(require trag.template [deftemplate])


; TODO: maybe full text of a single arXiv paper - maybe just chat over file?
;       consider arXiv latex -> markdown directly (with pandoc)


(deftemplate retrieval)

;; * YouTube
;; ----------------------------------------------------

(defn youtube-meta [youtube-id]
  "Return the title and source of the youtube video."
  (let [url f"https://www.youtube.com/oembed?url=https://www.youtube.com/watch?v={youtube-id}&format=json"
        response (.get httpx url)]
    (match response.status-code
      200 (let [data (.json response)
                title (:title data "No title provided")
                author (:author-name data "No author provided")]
            {#** data
             "title" title
             "author" author})
      otherwise (.raise_for_status response))))

(defn _get-transcript [youtube-id]
  "Fetch a transcript, failing gracefully where it's not available."
  (try
    (let [languages [(get (locale.getlocale) 0) "en" "en-GB"]
          avail-transcripts (.list-transcripts YouTubeTranscriptApi youtube-id)
          transcript (.fetch (.find-transcript avail-transcripts languages))
          formatter (TextFormatter)]
      (.format_transcript formatter transcript))
    (except [TranscriptsDisabled]
      "Transcripts are disable for this video.")))
  
(defn get-youtube [youtube-id [punctuate False]]
  "Load (and optionally punctuate) youtube transcript.
  Youtube 'transcripts' are normally just a long list of words with no
  punctuation or identification of the speaker.
  We can apply punctuation filter, which can give much higher quality text,
  but this takes VRAM (1-2GB) and requires pytorch.
  To do so, pass `puncuate` as `True`.
  Defaults to user's locale, this may not be desirable for summarization."
  (let [transcript (_get-transcript youtube-id)
        meta-info (youtube-meta youtube-id)]
    (when punctuate
      (do
        ; lazy import here because not everyone will want to spend the VRAM.
        (import deepmultilingualpunctuation [PunctuationModel])
        (setv text (.restore-punctuation (PunctuationModel) transcript))))
    {"transcript" transcript
     "accessed" (now)
     "youtube_id" youtube-id
     #** meta-info}))

(defn youtube [youtube-id #** kwargs]
  "Load (and optionally punctuate) youtube transcript as text."
  (let [ytd (get-youtube youtube-id #** kwargs)]
    (retrieval "youtube" #** ytd)))


;; * Web URL
;; ----------------------------------------------------

(defn get-url [url]
  "Fetch a URL's content as cleaned markdown text."
  (if (is-url url)
      (let [response (.get httpx url)
            cleaner (Cleaner :javascript True :style True)]
        (match response.status-code
          200 (-> response.text
                  (lxml.html.fromstring) 
                  (cleaner.clean_html)
                  (lxml.html.tostring)
                  (markdownify :heading-style "ATX" :strip "style")
                  (.replace "\r\n" "\n")
                  (.replace "\r" "\n")
                  (.strip)
                  (clean-web-md))
          otherwise (.raise_for_status response)))
      (raise (ValueError f"Fetching {url} failed (implausible url)."))))

(defn filename-from-url [url]
  "Sanitise a url into a filename."
  (let [parsed_url (urlparse url)
        netloc parsed_url.netloc
        path parsed_url.path
        fname f"{netloc}_{(os.path.basename path)}"]
    (+ (re.sub r"[^a-zA-Z0-9_.-]" "_" fname)
       "_" (short-id fname))))

(defn clean-web-md [text * [bad "#`|"]]
  "Web-sourced markdown strings often have multiple bad characters
  and repeated newlines.
  This function rewrites a string with each line stripped,
  and (stripped) lines starting with bad characters removed."
  (re.sub r"\n\n\n[\n]+" "\n" 
    (.join "\n"
      (lfor line (.split text "\n")
        ;; allow blank lines, but not 'bad' lines
        :if (if line
              (not (in (first line) bad))
              True)
          (.strip line)))))

(defn url [url]
  "Load a URL as markdown text."
  (retrieval "url"
    :accessed (now)
    :url url
    :document (get-url url)))
  
  
;; * arXiv
;; ----------------------------------------------------

(defn arxiv [topic [n 12]]
  "Get `n` relevant arxiv summaries on a topic (as text)."
  (let [results (.results (arxiv-search :query topic :max-results n))
        summaries (lfor paper results
                        (let [authors (.join ", " (map str paper.authors))]
                          (retrieval "arxiv_summary"
                            :title paper.title
                            :authors authors
                            :date paper.published
                            :entry-id paper.entry-id
                            :doi paper.doi
                            :summary paper.summary)))]

    (retrieval "arxiv_search"
      :topic topic
      :summaries (.join "\n---\n" summaries))))


;; * Wikipedia
;; ----------------------------------------------------

(defn wikipedia [topic [index 0]]
  "Get the full Wikipedia page on a topic (as text).
  Disambiguates onto the first disambiguation."
  (try
    (let [pages (wiki.search topic)
          best (get pages index)
          summary (wiki.summary best :auto-suggest False)
          page (wiki.page best :auto-suggest False)]
      (retrieval "wikipedia"
        :title page.title
        :url page.url
        :content page.content
        :related (.join ", " pages)))
    (except [wiki.exceptions.DisambiguationError]
      (wikipedia topic :index (inc index)))))
