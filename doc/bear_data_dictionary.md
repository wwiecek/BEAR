This dictionary describes `method` and `measure` in `BEAR.rds`. Individual
dataset downloads retain their own coding and may contain more detailed
information. Other variables will be documented here later.

## Method

Study design or identification strategy, using the information available in
each dataset. Coverage varies: some classifications use source labels, others
use review eligibility criteria or title keywords. `NA` includes source labels
that do not receive a classification in BEAR; it does not necessarily mean
that the author supplied no information or that a study is observational.

| Value | Definition |
| --- | --- |
| `rct` | Randomised controlled trial, as classified in the source dataset. |
| `observational` | Observational research, as classified in the source. |
| `quasi_experimental` | Quasi-experimental design, without a more specific classification. |
| `did` | Difference-in-differences studies. |
| `iv` | Instrumental variables estimation. |
| `rd` | Regression discontinuity studies. |
| `mixed` | Mixed designs or approaches, as classified in the source. |
| `NA` | Design or identification strategy not classified in BEAR. |

Where source classifications are unavailable, BEAR also uses review eligibility
criteria for Cochrane and a title-based flag for Jager and Leek; these do not
verify randomisation for each study. The dataset pages describe these conventions.

For Lang, some parenthetical details are omitted. OLS, matching, system GMM
and IV–DID with matching are coded as `NA`, affecting 203 estimates. OLS alone
does not identify a study design and may be used with experimental or
observational data. The original labels remain available in `Lang.rds`.

## Measure

Type of effect estimate. A shared category does not establish common units,
identical estimators or comparable study populations. Where relevant, the
scale is recorded separately in `effect_scale`. Renaming or grouping measures
does not change the retained estimates or their standard errors.

| Value | Definition |
| --- | --- |
| `smd` | Standardised mean difference, combining Cohen's d, Hedges' g and generic SMD labels. |
| `mean_difference` | Difference between means, in the outcome's units. |
| `median_difference` | Reported difference between medians. |
| `percentage_difference` | Reported difference in percentages; the label alone does not establish a binary risk difference or a common unit convention. |
| `risk_difference` | Difference between event probabilities. |
| `probit_difference` | Difference between probit-transformed event probabilities, defined below. |
| `correlation` | Correlation, recorded on its original scale or after Fisher's transformation. |
| `response_ratio` | Ratio of means, often recorded on a logarithmic scale. |
| `odds_ratio` | Ratio of event odds, where odds are probability divided by one minus probability. |
| `risk_ratio` | Ratio of event probabilities. |
| `hazard_ratio` | Ratio of instantaneous event hazards. |
| `rate_ratio` | Ratio of event rates, such as events per person-year. |
| `geometric_ratio` | Ratio of geometric means, as classified from the reported measure. |
| `ratio` | Ratio whose more specific type is not distinguished. |
| `regression_coefficient` | Regression coefficient or slope; units and standardisation depend on the source. |
| `eta_squared` | Proportion of total variation attributed to an effect. |
| `partial_eta_squared` | Variation attributed to an effect divided by the variation attributed to that effect and its associated error. |
| `other` | Reported measure outside the retained categories. |
| `NA` | Effect measure not classified; a p-value or test statistic alone need not identify it. |

### Response ratios

A response ratio compares means, $\bar y_1/\bar y_0$. Ecological `lnRR`
records its logarithm. A risk ratio instead compares event probabilities,
$p_1/p_0$. The distinction concerns the estimand, not the discipline.

### Probit differences

A probit difference is $\Phi^{-1}(p_1)-\Phi^{-1}(p_0)$, where $\Phi$ is the
standard normal distribution function. Probit alone can describe a single
transformed proportion or a regression link. Cochrane and ClinicalTrials.gov
both provide this contrast, although their handling of boundary proportions
differs; the individual dataset documentation describes the calculations.

### Correlations and scales

A correlation is recorded as $r$ or as Fisher's transformation,
$\operatorname{atanh}(r)=\tfrac12\log[(1+r)/(1-r)]$. For example,
$r=0.8$ becomes approximately $1.099$. The scale distinguishes these
representations; Fisher's transformation is not the test statistic `z`.

Ratios may be recorded on raw or logarithmic scales, including within the same
measure category. Costello and Fox also include absolute SMDs, identified by
`effect_scale = "absolute_smd"`. Consult the scale before comparing estimates.
`raw` indicates no recorded transformation, not common units or coefficient
standardisation; an unknown scale remains missing.

See the [metafor effect-size definitions](https://wviechtb.github.io/metafor/reference/escalc.html)
and [transformation definitions](https://wviechtb.github.io/metafor/reference/transf.html).
