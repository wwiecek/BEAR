## Data dictionary

One row is one study result within a Cochrane review analysis. Summaries count rows, not distinct reviews.

### Review identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `cochrane_id` | Cochrane review identifier. |  |
| `doi_meta` | DOI of the review. |  |
| `withdrawn` | Whether the review was withdrawn. | 0 733,563 / 760,486 (96.5%); 1 26,923 / 760,486 (3.5%) |

### Review characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `specialty` | Cochrane specialty / review group. | Pregnancy and Childbirth 66,114 / 760,486 (8.7%); Hepato-Biliary 52,397 / 760,486 (6.9%); Heart 41,515 / 760,486 (5.5%); Common Mental Disorders 40,320 / 760,486 (5.3%); Airways 35,504 / 760,486 (4.7%); remaining 524,636 / 760,486 (69.0%); missing 0 / 760,486 (0.0%) |
| `rct` | Whether review eligibility criteria indicate RCTs only. | TRUE 544,731 / 760,486 (71.6%); FALSE 215,755 / 760,486 (28.4%) |
| `id` | Cochrane review identifier supplied by the RM5 parser. |  |

### Analysis identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `comparison.nr` | Comparison number within the review. |  |
| `comparison.name` | Comparison label. |  |
| `comparison.id` | Cochrane internal identifier for the comparison. |  |
| `outcome.nr` | Outcome number within the comparison. |  |
| `outcome.name` | Outcome label. |  |
| `outcome.measure` | Effect measure reported by Cochrane. |  |
| `outcome.id` | Cochrane internal identifier for the outcome. |  |
| `outcome.flag` | Cochrane outcome type. | DICH 510,861 / 760,486 (67.2%); CONT 249,625 / 760,486 (32.8%) |
| `subgroup.nr` | Subgroup number within the outcome. |  |
| `subgroup.name` | Subgroup label. |  |
| `subgroup.id` | Cochrane internal identifier for the subgroup. | missing 248,547 |

### Study characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `study.id` | Cochrane internal identifier for the study. BEAR uses `study.name` as the study identifier. |  |
| `study.name` | Study label; used as `studyid` in BEAR. |  |
| `study.year` | Study year inferred from the Cochrane year field or study label. | missing 27,883 |
| `study.data_source` | Source of the study data: published only (`PUB`), unpublished only (`UNPUB`), published with unpublished data sought but not used (`SOUGHT`), or a mixture of published and unpublished data (`MIX`). | PUB 630,749 / 760,486 (82.9%); MIX 101,642 / 760,486 (13.4%); SOUGHT 17,328 / 760,486 (2.3%); UNPUB 10,767 / 760,486 (1.4%) |

### Reported effects

| Variable | Definition | Summary |
|:--|:--|:--|
| `effect.size` | Effect estimate reported by Cochrane. |  |
| `se` | Reported standard error. |  |
| `ci.lower` | Reported lower confidence limit. |  |
| `ci.upper` | Reported upper confidence limit. |  |

### Arm inputs

| Variable | Definition | Summary |
|:--|:--|:--|
| `events1` | Event count in the treatment/experimental arm for binary outcomes. |  |
| `total1` | Participant count in the treatment/experimental arm. |  |
| `mean1` | Mean in the treatment/experimental arm for continuous outcomes. |  |
| `sd1` | Standard deviation in the treatment/experimental arm. |  |
| `events2` | Event count in the control/comparator arm for binary outcomes. |  |
| `total2` | Participant count in the control/comparator arm. |  |
| `mean2` | Mean in the control/comparator arm for continuous outcomes. |  |
| `sd2` | Standard deviation in the control/comparator arm. |  |

### BEAR calculations

| Variable | Definition | Summary |
|:--|:--|:--|
| `measure_group` | Cochrane effect measure, standardised to `RR`, `OR`, `PETO_OR`, `RD`, `MD`, or `SMD`. | RR 397,896 / 760,486 (52.3%); MD 179,356 / 760,486 (23.6%); OR 77,446 / 760,486 (10.2%); SMD 70,269 / 760,486 (9.2%); PETO_OR 23,480 / 760,486 (3.1%); RD 12,039 / 760,486 (1.6%) |
| `measure` | Effect measure used by BEAR: SMD for continuous outcomes or probit difference for binary outcomes. | probit 510,861 / 760,486 (67.2%); SMD 249,625 / 760,486 (32.8%) |
| `measure_detailed` | Detailed label for the BEAR effect measure. | probit 510,861 / 760,486 (67.2%); SMD (Hedges' g) 249,625 / 760,486 (32.8%) |
| `outcome_group` | Outcome category derived from comparison, outcome, and subgroup labels: efficacy, safety, dropouts, or bias. | efficacy 595,500 / 760,486 (78.3%); safety 100,241 / 760,486 (13.2%); bias 46,480 / 760,486 (6.1%); dropouts 18,265 / 760,486 (2.4%) |

