## Data dictionary

One row is one study result within a Cochrane review analysis. Summaries count rows, not distinct reviews.

### Review identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `cochrane_id` | Stable CD review identifier from the RM5 filename. |  |
| `doi` | DOI of the RM5 source edition, where established; a checkpoint label is used only when no contrary evidence exists. |  |
| `withdrawn` | Whether the source RM5 review has withdrawal status W; repeated on every study-result row. | 0 733,563; 1 26,923 |

### Review characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `specialty` | Cochrane Review Group code for the source edition. | missing 36,034 |
| `rct` | Whether the matching edition abstract's eligibility section supports RCT-only classification. | TRUE 505,795; FALSE 187,878; NA 66,813 |
| `id` | Review identifier supplied by the RM5 parser. |  |

### Analysis identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `comparison.nr` | Comparison number within the review. |  |
| `comparison.name` | Comparison label. |  |
| `comparison.id` | Internal comparison identifier. |  |
| `outcome.nr` | Outcome number within the comparison. |  |
| `outcome.name` | Outcome label. |  |
| `outcome.measure` | Effect measure named for the source outcome. |  |
| `outcome.id` | Internal outcome identifier. |  |
| `outcome.flag` | Source result family. |  |
| `subgroup.nr` | Subgroup number within the outcome. |  |
| `subgroup.name` | Subgroup label. |  |
| `subgroup.id` | Internal subgroup identifier. | missing 248,547 |

### Study characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `study.id` | Internal source study identifier. |  |
| `study.name` | Study label used as studyid in BEAR. |  |
| `study.year` | Year parsed from the source study label or year field; implausible years are missing. | missing 27,883 |
| `study.data_source` | Source status of the study data. | PUB 630,749; MIX 101,642; SOUGHT 17,328; UNPUB 10,767 |

### Reported effects

| Variable | Definition | Summary |
|:--|:--|:--|
| `effect.size` | Effect estimate reported in the RM5 study table. |  |
| `se` | Reported standard error for effect.size. |  |
| `ci.lower` | Reported confidence interval lower limit. |  |
| `ci.upper` | Reported confidence interval upper limit. |  |
| `weight` | Analysis weight reported in the RM5 study table. |  |
| `order` | Source ordering of the study result within the analysis. |  |

### Arm inputs

| Variable | Definition | Summary |
|:--|:--|:--|
| `events1` | Event count in arm 1 for dichotomous outcomes. | missing 249,625 |
| `total1` | Participant count in arm 1. |  |
| `mean1` | Mean in arm 1 for continuous outcomes. | missing 510,861 |
| `sd1` | Standard deviation in arm 1 for continuous outcomes. | missing 510,861 |
| `events2` | Event count in arm 2 for dichotomous outcomes. | missing 249,625 |
| `total2` | Participant count in arm 2. |  |
| `mean2` | Mean in arm 2 for continuous outcomes. | missing 510,861 |
| `sd2` | Standard deviation in arm 2 for continuous outcomes. | missing 510,861 |

### BEAR calculations

| Variable | Definition | Summary |
|:--|:--|:--|
| `measure_group` | Broad effect measure category from outcome.measure. |  |
| `outcome_group` | Heuristic outcome category from comparison, outcome and subgroup labels. | efficacy 595,500; safety 100,241; bias 46,480; dropouts 18,265 |
| `phase` | Unused placeholder for trial phase. | missing 760,486 |
| `yi` | Recalculated Hedges g or probit difference from arm inputs. | missing 2,611 |
| `vi` | Sampling variance of yi. | missing 2,611 |
| `measure` | Broad label for the recalculated effect. | probit 510,861; SMD 249,625 |
| `measure_detailed` | Detailed label for the recalculated effect. |  |
| `z` | yi divided by the square root of vi. | missing 2,611 |

