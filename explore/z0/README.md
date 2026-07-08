# z = 0 mixture sensitivity

This is a BEAR-internal, maintainer-facing exploration. It was developed with
LLM assistance and should be treated as scratch analysis code, not as polished
paper documentation.

Explores how the Cochrane BEAR-subset mixture fit changes when exact
`z = 0` values are treated as left-censored at different bounds.

Run from the project root:

```r
source("explore/z0/fit_cochrane_z0_sensitivity.R")
```

Outputs are written under `explore/z0/output/`.
Generated outputs and fit caches are ignored by default. Re-run the script from
the project root when those artifacts are needed locally.
