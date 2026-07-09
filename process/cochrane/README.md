# Cochrane/CDSR download and processing workflow

This workflow builds `data/Cochrane.rds` from Cochrane intervention reviews. It
has two stages: download and parse each review's RM5 file, then convert the
study-level results into the format used by BEAR. Run all commands from the BEAR
project root.

## Obtaining the list of reviews

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

| Variable | Used by | Purpose and default |
|---|---|---|
| `manifest_path` | Download and processing | CSV exported from CDSR. The download script reads its DOI list; the processing script also reads abstracts and review-group codes when available. Defaults to `data_raw/Cochrane/data/cdsr_interventions_19nov2025.csv`. |
| `rm5_dir` | Download and processing | Directory containing one downloaded `*StatsDataOnly.rm5` file per review. Defaults to `data/Cochrane/rm5`. |
| `checkpoint_path` | Download and processing | Parsed, resumable RDS checkpoint. The download script updates it; the processing script reads it, or creates it from existing RM5 files if it is absent. Defaults to `data_raw/Cochrane/data/cdsr_rm5_results.rds`. |
| `output_path` | Processing only | Final processed dataset, `data/Cochrane.rds` by default. |

The checked-in values reproduce the November 2025 data cut. To use a newer
CDSR export, edit `manifest_path`, `rm5_dir`, and `checkpoint_path` in both
scripts before running them. Keep the values identical across the two scripts.
Use new, dated RM5 and checkpoint paths for a new data cut; otherwise the
download script will intentionally reuse the existing files and checkpoint.
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
the checkpoint, saves progress every 20 reviews, and pauses between download
attempts to avoid placing unnecessary load on Cochrane.

Failed downloads or parses are retained in the checkpoint with an error message.
This makes the run auditable and allows a later run to continue without
repeating successful work. Failed rows are also treated as completed when
resuming; remove a failed row from the checkpoint before intentionally retrying
that DOI.

## Building the processed dataset

After the download stage, run:

```sh
Rscript --vanilla process/cochrane/Cochrane_process_data.R
```

`Cochrane_process_data.R` reads the checkpoint, expands the review data to
study-level result rows, cleans study years, classifies outcomes, and adds the
optional review annotations from the manifest. It recalculates continuous
effects as standardized mean differences and binary effects on the probit
scale, then saves `data/Cochrane.rds`.

If the checkpoint is absent but RM5 files are available,
`Cochrane_process_data.R` parses those files and creates the checkpoint first.
The manifest is optional in this case, although the RCT and review-group
annotations will be missing.

Full refreshes should remain resumable. Do not delete the checkpoint or existing
RM5 files unless a clean re-download is specifically required.
