# DOI lookup workspace

Each dataset directory contains committed lookup or adjudication scripts and
Markdown review notes. Local results are divided into `derived/` (API caches,
candidates, reviews and validation artefacts) and `final/` (accepted identifier
mappings). Both are ignored: the committed `data/*.rds` files are the canonical
record of attached DOI values.

Run DOI scripts explicitly; canonical processors and `main.R` do not make
network requests. For a new lookup, place its durable script in
`doi/<dataset>/`, write disposable lookup material to `derived/`, and write a
mapping used by the processor to `final/`.
