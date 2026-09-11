This dictionary describes the common BEAR data and retained source-specific
metadata. Individual dataset files retain more detailed source coding. See the
[dataset documentation](datasets.html) for source universes, row construction,
filtering and dataset-specific conventions.

| Variable | Meaning | Notes |
| --- | --- | --- |
| `dataset` | Source BEAR dataset. | Defines the source universe and construction rules. |
| `metaid` | Higher-level analysis identifier. | Meta-analysis, review, replicated finding or analogous grouping; see each dataset page. |
| `studyid` | Best available source-unit identifier. | Study, paper, trial, replication site/sample, or a documented approximation. |
| `topic` | What the research is about. | Source-specific subject classification; not harmonised across datasets. |
| `subset` | How observations are divided. | Sampling, provenance, reporting or analysis grouping. |
| `method` | Study design or identification strategy. | Standardised categories listed in Method. |
| `measure` | Type of effect or estimand. | Standardised categories listed in Measure. |
| `effect_scale` | Representation of `b` and `se`. | Does not establish comparable units across studies. |
| `z` | Normal-equivalent test statistic. | Signed where direction is available; see derivations. |
| `z_operator` | Bound on the magnitude of `z`. | Applies to `abs(z)`, never directly to signed `z`. |
| `p` | Retained p-value. | Interpretation and sidedness depend on the source. |
| `b` | Effect estimate. | On the scale given by `effect_scale`, where known. |
| `se` | Standard error corresponding to `b`. | Reported or derived. |
| `ss` | Sample size. | Source-specific definition; consult the dataset page. |
| `year` | Study, publication or registry year. | Convention depends on the dataset. |
| `orig.z` | Original-study z-value attached to a replication. | Direction follows the original study. |
| `orig.z_operator` | Bound on the original-study z magnitude. | Same convention as `z_operator`. |
| `orig.p` | Original-study p-value. | Where available. |
| `orig.b` | Original-study effect estimate. | Where available. |
| `orig.se` | Original-study standard error. | Reported or derived. |
| `orig.ss` | Original-study sample size. | Where available. |
| `outcome_group` | Cochrane outcome grouping. | The retained Cochrane sample contains efficacy outcomes. |
| `source` | Retained source-specific provenance. | `claim_text` for SCORE claims, `replication` for SCORE replications and Many Labs 2, and `PLOS supporting MAT file` for Szucs. |

## Identifiers

`studyid` is not globally unique. Its construction differs by dataset and its
scope may be `dataset`, or only `dataset` plus `metaid`. Some sources lack a
reliable study identifier: BEAR then uses a documented approximate identifier
or, as a last resort, a row-unique ID. Such IDs do not establish that distinct
rows come from distinct primary studies. Every dataset page describes its
construction and limitations.

`metaid` usually identifies a meta-analysis, systematic review, replicated
effect, research question or analogous grouping. Its precise meaning is
dataset-specific; consult the individual dataset documentation before grouping.

## Topic and subset

`topic` describes what the research is about: for example, a discipline,
medical specialty, clinical condition, outcome domain or cognitive domain.
Taxonomies and granularity differ across sources. It is primarily intended for
within-dataset grouping; cross-dataset comparisons require further harmonisation.
Missing values mean no subject classification has been assigned.

`subset` describes a sampling, provenance, reporting or analysis grouping,
such as trial phase, preregistration status, extraction location or source
universe. It does not represent the subject of the research.

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
| `mixed` | Mixed observational and experimental research; used only by Askarov. |
| `not rct, not mixed` | Neither source flag is set; used only by Askarov. May include quasi-experimental research. |
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

| Value | Definition and scale notes |
| --- | --- |
| `smd` | Standardised mean difference, combining Cohen's d, Hedges' g and generic SMD labels. |
| `mean_difference` | Difference between means, in the outcome's units. |
| `median_difference` | Reported difference between medians. |
| `percentage_difference` | Reported difference in percentages; the label alone does not establish a binary risk difference or a common unit convention. |
| `risk_difference` | Difference between event probabilities. |
| `probit_difference` | Difference between probit-transformed event probabilities, $\Phi^{-1}(p_1)-\Phi^{-1}(p_0)$. Boundary corrections differ by source. |
| `correlation` | Raw $r$ or Fisher-transformed correlation, $\operatorname{atanh}(r)=\frac12\log[(1+r)/(1-r)]$, according to `effect_scale`. Fisher's transformation is distinct from the test statistic `z`. |
| `response_ratio` | Ratio of means, $\bar y_1/\bar y_0$, distinct from the risk ratio. Ecological `lnRR` records its logarithm. |
| `odds_ratio` | Ratio of event odds, $[p_1/(1-p_1)]/[p_0/(1-p_0)]$. May be represented on raw or log scales. |
| `risk_ratio` | Ratio of event probabilities, $p_1/p_0$. May be represented on raw or log scales. |
| `hazard_ratio` | Ratio of instantaneous event hazards. |
| `rate_ratio` | Ratio of event rates, such as events per person-year. |
| `geometric_ratio` | Ratio of geometric means, as classified from the reported measure. |
| `ratio` | Ratio whose more specific type is not distinguished. |
| `regression_coefficient` | Regression coefficient or slope; units and standardisation depend on the source. |
| `eta_squared` | Proportion of total variation attributed to an effect. |
| `partial_eta_squared` | Variation attributed to an effect divided by the variation attributed to that effect and its associated error. |
| `other` | Reported measure outside the retained categories. |
| `NA` | Effect measure not classified; a p-value or test statistic alone need not identify it. |


## Effect scale

`measure` describes the estimand; `effect_scale` describes how `b` and `se`
are represented. Consult both before comparing effects.

| Value | Representation |
| --- | --- |
| `raw` | Original effect scale, including raw correlations and regression coefficients. Does not imply common units or coefficient standardisation. |
| `log` | Natural logarithm of a ratio, with standard error on the log scale. |
| `fisher_z` | Fisher-transformed correlation, with standard error on that scale. |
| `smd` | Standardised mean difference. |
| `absolute_smd` | Absolute standardised mean difference, without effect direction. |
| `probit` | Difference on the standard normal quantile scale. |
| `NA` | Scale not classified. |

## Deriving z

Write $\Phi$ for the standard normal distribution function. Where an effect
and its standard error are used, $z=b/se$. For an exact two-sided p-value,

$$
|z|=\Phi^{-1}(1-p/2).
$$

Apply the effect or test-statistic sign when known. For a one-sided upper-tail
p-value, the directional normal quantile is $\Phi^{-1}(1-p)$; its magnitude
is $|\Phi^{-1}(1-p)|$. The quantile itself is non-negative only for $p\leq0.5$.
The source's tail convention and known effect direction must therefore be
considered before interpreting a one-sided p-value as an unsigned magnitude.

For a t-statistic with $\nu$ degrees of freedom,

$$
p=2F_{t_\nu}(-|t|), \qquad
z=\operatorname{sign}(t)\Phi^{-1}(1-p/2).
$$

For estimate $\hat\theta$, confidence bounds $L,U$ and confidence level
$1-\alpha$, put $q=\Phi^{-1}(1-\alpha/2)$. The basic two-sided Wald calculation is

$$
SE_U=\frac{U-\hat\theta}{q}, \qquad
SE_L=\frac{\hat\theta-L}{q}.
$$

When both are usable,

$$
SE=\frac{SE_U+SE_L}{2}, \qquad z=\frac{\hat\theta}{SE}.
$$

Ratio measures may first be transformed to the log scale. See
[Standard procedure for dealing with p-values and confidence intervals](documentation.html#standard-procedure-for-dealing-with-p-values-and-confidence-intervals)
for CI sidedness, malformed or unusual intervals, asymmetry, non-Wald intervals,
p-value inequalities, ambiguous cumulative/tail probabilities, sign assignment,
and selection between CI-derived and p-derived z. Infinite z-values are retained
when they follow from valid inputs, including exact zero p-values.

## Bounds on z magnitude

`z_operator` describes uncertainty or truncation in **`abs(z)`**:

| Value | Interpretation |
| --- | --- |
| `=` | The stored magnitude is treated as exact. |
| `>` | The true magnitude exceeds the stored magnitude. |
| `<` | The true magnitude is smaller than the stored magnitude. |
| `NA` | No operator is supplied. |

For a monotonically decreasing p-to-magnitude conversion,

$$
p<c\ \Longrightarrow\ |z|>z(c), \qquad
p>c\ \Longrightarrow\ |z|<z(c).
$$

For example, `z = -1.96`, `z_operator = ">"` means a negative direction with
an underlying magnitude exceeding 1.96. It **does not** mean $z>-1.96$.
The same interpretation applies to `orig.z_operator` and `orig.z`.

## Original-study values

The `orig.*` columns attach original-study statistics to replication rows in
OSC, Many Labs 2 and SCORE replications. Availability varies by dataset and
statistic; missing entries do not imply zero. Individual dataset documentation
explains pairing and the source of these values.
