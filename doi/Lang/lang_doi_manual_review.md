# Lang DOI manual review

## Task

Check whether each DOI in the Lang data identifies the correct source article.
The intended mapping is one DOI to one current `studyid`, and one DOI per
`studyid`. Check the DOI itself, the bibliographic match, and whether duplicate
`studyid` values should be combined or corrected.

The DOI values below were all found in Crossref's registry. The present lookup
does not, however, enforce a one-to-one mapping or require a sufficiently close
title match. `source rows` refer to rows in `cr_append.dta`; `score` is the
Crossref search score; and `overlap` is the share of words in the source title
also found in the matched Crossref title.

## 1. One DOI assigned to several study IDs

These records have the same DOI but different current `studyid` values.

| DOI | `studyid` | Source citation or title | Journal/year | Matched title | Source rows | Method |
|---|---|---|---|---|---:|---|
| `10.1257/aer.20191586` | `Lang_paper_2` | Abramitzky, Boustan, Jácome & Pérez (2021), *Intergenerational mobility of immigrants in the United States over two centuries* | 2021 | *Intergenerational Mobility of Immigrants in the United States over Two Centuries* | 3495 | OLS |
| `10.1257/aer.20191586` | `Lang_paper_3` | Same citation; pages 580--609 | 2021 | Same | 3496 | OLS |
| `10.1257/aer.20191586` | `Lang_paper_4` | Same citation; pages 580--610 | 2021 | Same | 3497 | OLS |
| `10.1257/aer.20191586` | `Lang_paper_5` | Same citation; pages 580--611 | 2021 | Same | 3498 | OLS |
| `10.1093/qje/qjab016` | `Lang_paper_29` | Breza & Kinnan (2021), *Measuring the equilibrium impacts of credit: Evidence from the Indian microfinance crisis*, pages 1447--1497 | 2021 | *Measuring the Equilibrium Impacts of Credit: Evidence from the Indian Microfinance Crisis* | 3597 | DID |
| `10.1093/qje/qjab016` | `Lang_paper_30` | Same citation; pages 1447--1498 | 2021 | Same | 3598 | DID |
| `10.1093/qje/qjab016` | `Lang_paper_31` | Same citation; pages 1447--1499 | 2021 | Same | 3599 | DID |
| `10.1257/aer.20201238` | `Lang_paper_42` | Chen (2021), *Team-specific human capital and team performance: Evidence from doctors* | 2021 | *Team-Specific Human Capital and Team Performance: Evidence from Doctors* | 3648 | OLS |
| `10.1257/aer.20201238` | `Lang_paper_43` | Same citation, with an encoding difference | 2021 | Same | 3649 | OLS |
| `10.1111/ecoj.12505` | `Lang_paper_472` | *Migration, population composition and long run economic development: Evidence from settlements in the Pampas* | Economic Journal, 2018 | *Migration, Population Composition and Long Run Economic Development: Evidence from Settlements in the Pampas* | 2004--2009 | IV |
| `10.1111/ecoj.12505` | `Lang_paper_474` | Same title | Economic Journal, 2018 | Same | 2052--2054 | IV |

The first three groups appear to be repeated top-five records with slightly
different page ranges. Confirm whether they are duplicate article records or
distinct records that should retain a shared DOI.

## 2. One study ID assigned two DOIs

`Lang_paper_474` contains two different source titles and overlapping raw row
ranges. This looks like a collision in the source `unique_paperid`.

| `studyid` | Source title | DOI | Matched title | Score | Overlap | Source rows | Tests |
|---|---|---|---|---:|---:|---:|---:|
| `Lang_paper_474` | *Climate change and labour allocation in rural Mexico: Evidence from annual fluctuations in rainfall* | `10.1111/ecoj.12448` | *Climate Change and Labour Allocation in Rural Mexico: Evidence from Annual Fluctuations in Rainfall* | 72.5 | 1.00 | 2043--2056 | 12 |
| `Lang_paper_474` | *Migration, population composition and long run economic development: Evidence from settlements in the Pampas* | `10.1111/ecoj.12505` | *Migration, Population Composition and Long Run Economic Development: Evidence from Settlements in the Pampas* | 83.4 | 1.00 | 2052--2054 | 2 |

Check whether the second article should have a new `studyid`, and whether the
overlapping raw rows are correctly assigned.

## 3. Source-title overlap below 90%

These are the 18 records with a non-missing source title and overlap below
0.90. Low overlap does not necessarily indicate a bad DOI: Crossref often
returns a shortened title. It does indicate that the match should be checked.

| `studyid` | Source title | DOI | Matched title | Score | Overlap | Journal/year | Source rows |
|---|---|---|---|---:|---:|---|---:|
| `Lang_paper_324` | Department of Economics, University of Chicago, United States; Research Department, Statistics Norway | `10.3326/pse.46.2.3` | *Aggregate marginal costs of public funds* | 58.7 | 0.00 | Journal of Public Economics, 2015 | 807--808 |
| `Lang_paper_223` | Freeman Spogli Institute for International Studies, Stanford University | `10.23846/ow11066` | *Paying for performance in China's battle against anemia* | 50.4 | 0.125 | Journal of Development Economics, 2015 | 327--330 |
| `Lang_paper_591` | *Missing Work Is a Pain: The Effect of Cox-2 Inhibitors on Sickness Absence and Disability Pension Receipt* | `10.3368/jhr.53.1.0215-6958r1` | *Missing Work Is a Pain* | 89.0 | 0.231 | Journal of Human Resources, 2018 | 2815--2818 |
| `Lang_paper_689` | *Double for Nothing? Experimental Evidence on an Unconditional Teacher Salary Increase in Indonesia* | `10.5040/9781474209700.0002` | *Double for Nothing?* | 84.8 | 0.300 | Quarterly Journal of Economics, 2018 | 3233--3234 |
| `Lang_paper_584` | *Developing Hope among Impoverished Children Using Child Self-Portraits to Measure Poverty Program Impacts* | `10.3368/jhr.53.2.0816-8112r1` | *Developing Hope among Impoverished Children* | 95.5 | 0.385 | Journal of Human Resources, 2018 | 2784--2786 |
| `Lang_paper_279` | *All Internal in the Family? Measuring Spillovers from Public Health Insurance* | `10.3368/jhr.50.4.959` | *All Internal in the Family?* | 72.4 | 0.400 | Journal of Human Resources, 2015 | 555--556 |
| `Lang_paper_280` | *Child Control in Education Decisions: An Evaluation of Targeted Incentives to Learn in India* | `10.3368/jhr.50.4.1051` | *Child Control in Education Decisions* | 72.4 | 0.444 | Journal of Human Resources, 2015 | 557--588 |
| `Lang_paper_530` | *The heterogeneous effect of information on student performance: Evidence from a randomized control trial in Mexico* | `10.5040/9781474209847.0002` | *The Heterogeneous Effect of Information on Student Performance* | 70.7 | 0.500 | Journal of Development Economics, 2018 | 2521--2527 |
| `Lang_paper_586` | *Effective Policy for Reducing Poverty and Inequality? The Earned Income Tax Credit and the Distribution of Income* | `10.3368/jhr.53.4.1115.7494r1` | *Effective Policy for Reducing Poverty and Inequality?* | 92.3 | 0.500 | Journal of Human Resources, 2018 | 2788--2789 |
| `Lang_paper_589` | *Information, Market Incentives, and Student Performance: Evidence from a Regression Discontinuity Design in Brazil* | `10.3368/jhr.53.2.0115-6868r1` | *Information, Market Incentives, and Student Performance* | 76.2 | 0.500 | Journal of Human Resources, 2018 | 2810--2811 |
| `Lang_paper_585` | *Drug Violence and Migration Flows: Lessons from the Mexican Drug War* | `10.3368/jhr.53.3.0215-6948r4` | *Drug Violence and Migration Flows* | 73.3 | 0.545 | Journal of Human Resources, 2018 | 2787 |
| `Lang_paper_590` | *The Effect of Competition on Executive Compensation and Incentives: Evidence from a Quasi-natural Experiment* | `10.3368/jhr.53.3.0215-6963r1` | *The Effect of Competition on Executive Compensation and Incentives* | 72.2 | 0.583 | Journal of Human Resources, 2018 | 2812--2814 |
| `Lang_paper_581` | *Diversity and Employment Prospects: Neighbors Matter!* | `10.3368/jhr.53.3.0115.6895r1` | *Diversity and Employment Prospects* | 55.6 | 0.667 | Journal of Human Resources, 2018 | 2769--2776 |
| `Lang_paper_208` | *Managerial Practices and Students' Performance* | `10.1093/epolic/eiv015` | *Managerial Practices and Student Performance* | 37.9 | 0.800 | Economic Policy, 2015 | 183 |
| `Lang_paper_627` | *Spillovers from gatekeeping -- Peer effects in absenteeism* | `10.1016/j.jpubeco.2018.08.015` | *Spillovers from gatekeeping -- Peer effects in absenteeism* | 67.4 | 0.857 | Journal of Public Economics, 2018 | 2975 |
| `Lang_paper_570` | *The effect of mortgage securitization on foreclosure and modification* | `10.1016/j.jfineco.2018.01.008` | *The effect of mortgage securitization on foreclosure and modification* | 67.1 | 0.875 | Journal of Financial Economics, 2018 | 2735--2738 |
| `Lang_paper_641` | *Improving police services: Evidence from the French Quarter Task Force* | `10.1016/j.jpubeco.2018.05.002` | *Improving police services: Evidence from the French Quarter Task Force* | 74.3 | 0.889 | Journal of Public Economics, 2018 | 3010--3012 |
| `Lang_paper_656` | *News media and crime perceptions: Evidence from a natural experiment* | `10.1016/j.jpubeco.2018.07.002` | *News media and crime perceptions: Evidence from a natural experiment* | 58.3 | 0.889 | Journal of Public Economics, 2018 | 3099--3102 |

The two clearest source-data problems are `Lang_paper_223` and
`Lang_paper_324`, where the stored source title is an affiliation rather than
an article title. A further 125 top-five records have no source title, so their
DOI match cannot be assessed by title overlap without using the citation text.

## Files used

- `data/Lang.rds`
- `data_raw/Lang/derived/lang_doi_candidates.csv`
- `data_raw/Lang/23259data/Data/cr_append.dta`
- `doi/Lang/lookup.R`
