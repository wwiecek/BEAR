# Sources of data in this project

Up-to-date document with short descriptions is 
[available as a Google Doc](https://docs.google.com/document/d/1ZZAEwfHS0aAELN1w1lqEO6-eeu31_WdL/edit?usp=sharing&ouid=114240127695432531696&rtpof=true&sd=true).

General methodology for creating sets of z-values:

- Extract z as `b/se` or `-qnorm(pvalue/2)`
- If some `z` values were truncated in some way, keep an indicator for that
- Extract year: if available, year of intervention (Askarov et al), if not, year of study; in one case (Yang et al) I retrieve it from study titles, since half of them have dates available
- Keep study indicator, labelled `studyid`
- For sets of meta-analyses, also retain `metaid` indicator
- Remove studies where z's are NA before saving (to reduce size of data)

Post-processing:

- Truncate very large z values ourselves 
- Add number of observations in each study, 
- Add number of studies in each meta-analysis 
- For large datasets, choose one z-value per study
- ...something about calculating weights