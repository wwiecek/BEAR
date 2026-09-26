# Cochrane/CDSR download and processing workflow

This workflow builds `data/Cochrane.rds` from Cochrane intervention reviews. It
has two stages: download and parse each review's RM5 file, then convert the
study-level results into the format used by BEAR. Run all commands from the BEAR
project root.



## Note on refreshing and removing data

> Generally, do not delete the checkpoint or existing
> RM5 files unless a clean re-download is specifically required. See below.




## Obtaining the list of reviews to download

This step is semi-manual, but straightforward. Go to:

https://www.cochranelibrary.com/cdsr/reviews

Select **Interventions** only, then **Select all**. There were 9,177 intervention
reviews on 9 July 2026. Export all selected reviews as CSV and include abstracts.
The export can take some time to prepare.

Save the CSV under `data_raw/Cochrane/data/` with the snapshot date in its name,
for example `cdsr_interventions_09jul2026.csv`. Check that it contains `DOI`,
`Abstract`, and `Cochrane Review Group Code` columns. Only `DOI` is required for
the download; the other two fields are used later to classify likely RCT-only
reviews and add review-group metadata.




## Configure the scripts

Downloading and parsing RM5 files requires the `cochrane` package:

```r
remotes::install_github("schw4b/cochrane")
```

The input and output paths are local variables near the top of each script:

-  `manifest_path`: CSV exported from CDSR. The download script reads its DOI 
    list; the processing script also reads abstracts and review-group codes 
    when available. 
    Defaults to `data_raw/Cochrane/data/cdsr_interventions_9jul2026.csv`
-  `rm5_dir`: Directory containing one downloaded `*StatsDataOnly.rm5` file per 
    review. Defaults to `data_raw/Cochrane/rm5`
-  `checkpoint_path`: Resumable RDS checkpoint. 
    The download script updates it; the processing script reads it, or creates 
    it from existing RM5 files if it is absent. 
    Defaults to `data_raw/Cochrane/data/cdsr_rm5_results.rds`.
-  `output_path`: Final processed dataset, `data/Cochrane.rds` by default.

The checked-in values resume the July 2026 data cut from the local checkpoint.
To use another CDSR export, edit `manifest_path`, `rm5_dir`, and
`checkpoint_path` in both scripts. Keep the values identical across the two
scripts. Use new, dated RM5 and checkpoint paths for an independent data cut.
Change `output_path` in `Cochrane_process_data.R` only if the processed dataset
should be saved somewhere other than `data/Cochrane.rds`.




## Downloading and parsing reviews

After checking the paths at the top of the script, run:

```sh
Rscript --vanilla process/cochrane/Cochrane_download_data.R
```

`Cochrane_download_data.R` derives each `CD######` review identifier from its
DOI, downloads the corresponding RM5 file, and parses it into the checkpoint.
It skips RM5 files already on disk, resumes from reviews already recorded in
the checkpoint, saves progress every 20 reviews, and waits 15 seconds between
download attempts. It records HTTP 429 responses as `rate_limited` and HTTP
404 responses as `review_not_found`, then saves the checkpoint and stops after
three consecutive rate-limit, access-block, unavailable, or non-XML responses.

HTTP 429, access-block, unavailable, and non-XML responses are logged in
`cdsr_rm5_retryable_failures.rds`, so they
are retried automatically on the next refresh of the dataset. Non-XML files are 
moved to `data_raw/Cochrane/rm5_failed/retryable/`. Other failed downloads or 
parses are retained in the checkpoint with an error message and a `failure_reason` 
value.



## Building the processed dataset

After the download stage, run:

```sh
Rscript --vanilla process/cochrane/Cochrane_process_data.R
```

`Cochrane_process_data.R` reads the checkpoint, expands the review data to
study-level result rows, cleans study years, classifies outcomes, and adds the
optional review annotations from the CSV file (aka "manifest"). It recalculates 
continuous effects as standardized mean differences and binary effects on the 
probit scale, then saves `data/Cochrane.rds`. See dataset documentation for a 
bit more info on this.

If the checkpoint is absent but RM5 files are available,
`Cochrane_process_data.R` parses those files and creates the checkpoint first.
The manifest is optional in this case, although the RCT and review-group
annotations will be missing.

