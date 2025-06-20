# TRAG

`trag` is glue intended for use with (in particular) `fvdb` for retreival
and `hyjinx.llm` for generation. But, it's abstract enough for other uses.

- Get sources from a url, youtube, arxiv, wikipedia and produce markdown strings.
- A very simple templating system, for when jinja is overkill.

`trag` does not specify or require any particular LLM or vector database solutions.


## Templating

Example usage, uses standard `summary.toml` template:
```hy
(require trag.template [deftemplate])

(deftemplate summary)

(summary "bullet" :text "Here is some text. The main points involve a cat, a dog and an ambulance. The shop closed for lack of stock.")

;; and so on
```

## Web sources

Example usage:

```hy
(import trag [web])

(web.arxiv "Retrieval-augmented generation" :n 3)
(web.youtube "dQw4w9WgXcQ")
(web.wikipedia "Retrieval-augmented generation")
(web.url "https://www.gutenberg.org/cache/epub/11/pg11.txt")
```

## Command-line usage

Proceeds similarly,
```bash
$ trag --help
```
