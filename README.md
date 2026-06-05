
# Benchmarks of Empirical Accuracy in Research

BEAR is a meta-database containing z values, effect sizes and standard
errors from existing databases in various scientific disciplines. It
doesn’t contribute any new data, but repackages and merges what’s
publicly available in a manner which we hope is maximally user-friendly.
Our intention is to help researchers interested in issues of
replication, exchangeability, meta-analysis etc. etc. The current
release includes millions of z-values, including about 100,000 studies
from curated datasets across a diverse range of fields.

This package includes our entire workflow (in R), documentation,
results, and optional modelling of empirical research.

- **Website:** <https://wwiecek.github.io/BEAR/>
- **If you want to grab data only**, head to the GitHub Releases page
  and grab `BEAR.rds` [at this
  link](https://github.com/wwiecek/BEAR/releases).\*\*
- If you want to do more data work with individual datasets, see
  *Downloading BEAR data* below.
- To re-run everything, including modelling of data, see `main.R`.

![](doc/bear_banner.png)

# Datasets included in BEAR

References, details of data availability and processing, links to raw
data, and short descriptions of each dataset are in [a separate PDF in
the doc folder](doc/datasets.pdf).

Here is a short summary of what’s included in BEAR:

### Replication efforts

| Dataset | Domain | Values | Meta-analyses | Studies | Values/study | Significant |
|:---|:---|---:|---:|---:|---:|---:|
| Many Labs 2 | psychology | 1,592 | 28 | 128 | 12.4 | 41.4% |
| OpenSciCollab | psychology | 97 | 1 | 97 | 1.0 | 35.1% |
| SCORE replications | social & behavioural sciences | 267 | 1 | 163 | 1.6 | 64.4% |

### Metascience datasets

| Dataset | Domain | Values | Meta-analyses | Studies | Values/study | Significant |
|:---|:---|---:|---:|---:|---:|---:|
| Arel-Bundock et al | political science | 16,649 | 46 | 2,252 | 7.4 | 46.7% |
| Askarov et al | economics | 21,408 | 352 | 1,913 | 11.2 | 52.0% |
| Bartos et al | exercise | 2,239 | 215 | 2,239 | 1.0 | 26.3% |
| Brodeur et al | economics | 15,917 | 1 | 328 | 48.5 | 35.7% |
| Costello and Fox | ecology & evolution | 88,218 | 466 | 12,927 | 6.8 | 41.9% |
| Lang | economics | 3,885 | 1 | 736 | 5.3 | 57.6% |
| SCORE, all claims | social & behavioural sciences | 1,942 | 1 | 159 | 12.2 | 74.9% |
| Sladekova et al | psychology | 11,591 | 406 | 3,547 | 3.3 | 59.2% |
| Szucs and Ioannidis | cognitive neuroscience | 16,887 | 1 | 2,261 | 7.5 | 62.0% |
| Yang et al | ecology & evolution | 17,638 | 87 | 3,796 | 4.6 | 41.7% |

### Databases

| Dataset | Domain | Values | Meta-analyses | Studies | Values/study | Significant |
|:---|:---|---:|---:|---:|---:|---:|
| Cochrane\* | medicine & health | 38,058 | 5,871 | 29,327 | 1.3 | 31.1% |
| EUDRA | clinical trials | 8,650 | 1 | 8,650 | 1.0 | 41.4% |
| Metapsy | psychotherapy | 4,395 | 20 | 1,494 | 2.9 | 48.4% |
| Nuijten et al | intelligence | 2,439 | 1 | 1,913 | 1.3 | 52.9% |
| What Works Clearing. | education | 12,045 | 1 | 1,408 | 8.6 | 33.4% |
| clinicaltrials.gov | clinical trials | 41,367 | 1 | 16,597 | 2.5 | 48.4% |
| psymetadata | psychology | 8,514 | 1 | 721 | 11.8 | 35.8% |

### PubMed/Medline scraped data

| Dataset | Domain | Values | Meta-analyses | Studies | Values/study | Significant |
|:---|:---|---:|---:|---:|---:|---:|
| Barnett and Wren\* | biomedicine | 50,000 | 1 | 50,000 | 1.0 | 81.9% |
| Chavalarias et al\* | biomedicine | 50,000 | 1 | 50,000 | 1.0 | 63.1% |
| Head et al\* | biomedicine | 50,000 | 1 | 50,000 | 1.0 | 59.2% |
| Jager and Leek | biomedicine | 15,653 | 1 | 5,322 | 2.9 | 77.7% |

*Main file `BEAR.rds` makes available a smaller analysis subset.
Additional data are available in the `data/` folder: 416k studies for
Barnett and Wren, 1.9mln studies for Chavalarias et al, 90k studies in
6.6k reviews for Cochrane, and 219k studies for Head et al.*

The table groups datasets by broad source family. A separate workflow
classification used below distinguishes curated datasets of single
studies, curated sets of meta-analyses (i.e. with additional `metaid`
grouping column), large-scale scraped datasets from PubMed/Medline, and
replication datasets. Additional groupings (e.g. clinical trial phases)
are available within some datasets. Replication datasets also include
additional columns that store values from original studies.

| Category     | Datasets | Studies | Meta-analyses |  Values | Significant |
|:-------------|---------:|--------:|--------------:|--------:|------------:|
| curated      |        9 |  32,773 |               | 111,646 |       46.4% |
| meta         |        8 |  57,495 |         7,463 | 200,196 |       42.3% |
| replications |        3 |     388 |            30 |   1,956 |       44.2% |
| scrape       |        4 | 155,322 |               | 165,653 |       69.0% |

# Downloading BEAR data

If you only want to grab data, head to the GitHub Releases page and grab
`BEAR.rds` [at this link](https://github.com/wwiecek/BEAR/releases).
Alternatively, in command line do

    curl -L -o BEAR.rds https://github.com/wwiecek/BEAR/releases/download/v2/BEAR.rds

You can also re-generate that file yourself. To work with individual
datasets or to do more manipulation of inputs, you need to take an
additional step. After cloning this repo (which only includes code and
outputs), you need to grab a “submodule” repo that has contents of
`data/` folder. In command line of this repository, do

    git submodule update --init --recursive

That will download about 100 MB of individual datasets. In other words,
downloading all of the input data files is opt-in rather than part of
this repo, to keep the repo size minimal.

If you want to re-derive each of the datasets yourself, you can see all
of data processing done for individual datasets in `process/`. For many
datasets there is nil processing done by us, but for some there are
extensive scripts to download and clean up data.

Once datasets are present in `data/`, `BEAR.rds` is stiched together in
script `workflow/build_bear.R` which does some of the processing
described in [datasets documentation](doc/datasets.pdf).

# Modelling datasets using mixture models

Once `BEAR.rds` exists, you may choose to fit our preferred mixture
models to the sets of z-values. These models are described in the
accompanying paper: [A Statistical Case for Qualified Scientific
Optimism](https://sites.stat.columbia.edu/gelman/research/unpublished/A_statistical_case_for_qualified_scientific_optimism.pdf).

Before fitting models, we first do minimal postprocess
(`workflow/postprocess.R`):

- Add number of observations in each study and number of studies in each
  meta-analysis
- Add study weights as 1 / (N values reported in that study)
- For large datasets, thin out the dataset to 50,000 rows, preferring
  one z-value per paper
- Truncate very large z values (z \> 20) and replace “z = 0” statements
  with “z \< 0.5” (z=0 would not work when fitting mixtures)

In `workflow/fit_mixtures.R` we create a fit for each of the datasets,
saved in `mixtures/`. This can take a few minutes per dataset. We
include the up-to-date results in the repo.

We calculate summaries for each dataset (e.g. probablity of
significance, replication, correct sign) in `workflow/calculate_psr.R`

# Results of mixture modelling

| dataset              | omega |  PoS | PoS_80 | replication | sign |
|:---------------------|------:|-----:|-------:|------------:|-----:|
| Nuijten et al        |  0.85 | 0.48 |   0.29 |        0.47 | 0.88 |
| Askarov et al        |  0.70 | 0.48 |   0.29 |        0.46 | 0.88 |
| SCORE, all claims    |  0.12 | 0.47 |   0.27 |        0.45 | 0.88 |
| Metapsy              |  0.73 | 0.43 |   0.22 |        0.41 | 0.86 |
| Lang                 |  0.24 | 0.42 |   0.22 |        0.41 | 0.86 |
| Arel-Bundock et al   |  0.64 | 0.40 |   0.20 |        0.38 | 0.85 |
| Costello and Fox     |  0.74 | 0.39 |   0.24 |        0.37 | 0.79 |
| Szucs and Ioannidis  |  0.35 | 0.35 |   0.14 |        0.34 | 0.84 |
| ctgov / EU CTR       |  0.50 | 0.35 |   0.22 |        0.34 | 0.76 |
| psymetadata          |  0.66 | 0.32 |   0.21 |        0.31 | 0.72 |
| What Works Clearing. |  0.88 | 0.32 |   0.17 |        0.30 | 0.75 |
| Brodeur et al        |  0.67 | 0.32 |   0.14 |        0.30 | 0.81 |
| Cochrane             |  0.70 | 0.23 |   0.10 |        0.21 | 0.69 |
| Bartos et al         |  0.81 | 0.23 |   0.08 |        0.20 | 0.74 |
| SCORE replications   |  0.70 | 0.50 |   0.33 |        0.49 | 0.88 |
| SCORE original       |  0.04 | 0.48 |   0.28 |        0.46 | 0.88 |
| Many Labs 2          |  0.83 | 0.37 |   0.29 |        0.35 | 0.71 |
| OpenSciCollab        |  1.05 | 0.36 |   0.23 |        0.34 | 0.80 |
| OSC original         |  0.03 | 0.29 |   0.09 |        0.27 | 0.81 |
| Many Labs original   |  0.06 | 0.24 |   0.12 |        0.22 | 0.73 |
| Chavalarias et al    |  0.15 | 0.52 |   0.33 |        0.50 | 0.89 |
| Barnett and Wren     |  0.08 | 0.30 |   0.11 |        0.28 | 0.81 |

`omega` is relative publication probability based on crossing of the
\|z\|=1.96 threshold; `PoS` is probability of significance (assurance)
and `PoS_80` is proportion of studies that reach 80% power;
`replication` and `sign` probabilities are described in accompanying
paper.
