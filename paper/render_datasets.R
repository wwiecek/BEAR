# Render the paper-only dataset appendix as an Overleaf-ready TeX fragment.
# Run from the BEAR project root after changing its canonical dataset notes.

markdown_file <- tempfile("bear-datasets-", fileext = ".md")
tex_file <- tempfile("bear-datasets-", fileext = ".tex")
on.exit(unlink(c(markdown_file, tex_file)), add = TRUE)

invisible(knitr::knit("paper/datasets.Rmd", output = markdown_file, quiet = TRUE))
rmarkdown::pandoc_convert(
  markdown_file,
  to = "latex",
  output = tex_file,
  options = c("--biblatex", "--wrap=auto")
)

tex_lines <- readLines(tex_file, warn = FALSE)
tex_lines <- tex_lines[tex_lines != "\\tightlist"]

writeLines(
  c("% Do not edit by hand; generated from paper/datasets.Rmd.", tex_lines),
  "paper/datasets.tex"
)
