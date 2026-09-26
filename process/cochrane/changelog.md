# Changelog

## 2026-07-09

- Moved the Cochrane download, processing, and RCT-classification scripts into
  this folder.
- Replaced the private-CSV dependency with a DOI-manifest interface and
  optional abstract/specialty annotations.
- Added resumable parsing from existing RM5 files and explicit input/output
  configuration near the top of each runnable script, while retaining
  `data/Cochrane.rds` as the default final output.
- Updated study-year validation to allow the current year plus one and made
  continuous/dichotomous effect calculation robust when one type is absent.
