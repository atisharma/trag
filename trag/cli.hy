"
trag - glue intended for use with `fvdb` for retreival
and `hyjinx.llm` for generation.
But, it's abstract enough for other uses.

Command-line utilities are provided for fetching markdown text to stdout.
"

(import click)

(import trag [web])


(defn [(click.group)]
      cli [])

(defn [(click.command) (click.argument "query") (click.option "-r" "--top" :default 6 :type int :help "Return just top n results.")]
  arxiv [query top]
  (click.echo (web.arxiv query -n top)))

(cli.add-command arxiv)


(defn [(click.command) (click.argument "location")] url [location]
  (click.echo (web.url location)))

(cli.add-command url)

  
(defn [(click.command) (click.argument "query")] wikipedia [query]
  (click.echo (web.wikipedia query)))
  
(cli.add-command wikipedia)

  
(defn [(click.command) (click.argument "id")] youtube [id]
  (click.echo (web.youtube id)))
  
(cli.add-command youtube)
