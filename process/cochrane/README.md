# Cochrane RM5 Workflow

Run these scripts from the BEAR project root. A fresh download requires a CSV
manifest with a column named `DOI` or `doi`. Review ID, abstract, and review
group columns are optional; the workflow derives `CD######` identifiers from
DOIs and leaves unavailable annotations missing.

Downloading or parsing RM5 files requires the `cochrane` package:

```r
remotes::install_github("schw4b/cochrane")
```

To download and parse reviews with a current manifest:

```sh
BEAR_COCHRANE_MANIFEST=/path/to/current_cdsr_manifest.csv \
Rscript --vanilla process/cochrane/Cochrane_download_data.R
```

The downloader writes RM5 files to `data/Cochrane/rm5` and a resumable parsed
checkpoint to `data_raw/Cochrane/data/cdsr_rm5_results.rds`. It skips existing
RM5 files, saves every 20 reviews, and sleeps after download attempts to avoid
placing unnecessary load on Cochrane.

To process an existing checkpoint or local RM5 files:

```sh
Rscript --vanilla process/cochrane/Cochrane_process_data.R
```

If the checkpoint is absent, the processor finds `*StatsDataOnly.rm5` files,
parses them, and creates the checkpoint. No manifest is required in this mode.
When a manifest is available, abstracts support likely-RCT classification and
review-group codes supply specialty metadata; both annotations remain missing
otherwise. The final processed dataset is `data/Cochrane.rds`.

All three paths can be overridden:

- `BEAR_COCHRANE_MANIFEST`
- `BEAR_COCHRANE_RM5_DIR`
- `BEAR_COCHRANE_CHECKPOINT`
- `BEAR_COCHRANE_OUTPUT` (defaults to `data/Cochrane.rds`)

Full refreshes should remain resumable and polite. Do not delete checkpoints or
existing RM5 files unless a clean re-download is specifically required.
