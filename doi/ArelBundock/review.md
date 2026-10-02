# Arel-Bundock article metadata review

The 46 `meta_id` values are source identifiers, not publication dates. Journal
and year metadata come from the source package's article-status table, with
version corrections in `article_metadata.csv`. The supplied classifications
were reviewed for all 12 flagged records; none required a topic change.

| `meta_id` | Review decision |
|---|---|
| `AskDouPal2021` | Retain international relations: the article studies US aid conditional on democracy and human rights. The [journal record](https://doi.org/10.1016/j.ejpoleco.2021.102089) is 2022; retain the source ID. |
| `BhaDahHan2019` | Retain elections and campaigns: the outcome is voter turnout after canvassing. [Cambridge](https://doi.org/10.1017/S0007123416000521) lists 2016 online and 2019 issue dates. |
| `DinSchSon2020` | Retain political behaviour: social trust is the outcome in the [article](https://doi.org/10.1146/annurev-polisci-052918-020708). |
| `EshEtAl2021` | Retain public administration: the [replication and meta-analysis](https://doi.org/10.1111/puar.13367) studies public-policy branding and trust. |
| `GreMicRob2006` | Retain methods: the [article](https://doi.org/10.1002/pam.20190) compares experimental with nonexperimental evaluations. The third author is Philip K. Robins. |
| `HolRanMooCro2021` | Retain elections: the [article](https://doi.org/10.1007/s11109-021-09746-2) examines subsequent attitudes and behaviour after voting. It appeared online in 2021 and in a 2023 issue. |
| `KalBro2017` | Retain elections: the [article](https://doi.org/10.1017/S0003055417000363) studies campaign-contact persuasion; the journal issue is 2018. |
| `LiOweMit2018` | Retain political economy: the [article](https://doi.org/10.1093/isq/sqy014) analyses democracy and foreign direct investment. |
| `LuLinWan2019` | Retain public administration: the [publisher record](https://link.springer.com/article/10.1007/s11266-019-00093-9) studies nonprofit financial capacity and vulnerability and confirms both online and issue publication in 2019. |
| `OweLi2020` | Retain methods: the [article](https://doi.org/10.1017/psrm.2020.15) studies conditional publication bias, using the democracy–FDI literature. It appeared online in 2020 and in a 2021 issue. |
| `YesYes2019` | Retain political economy: the [article](https://doi.org/10.1177/0022343318808841) estimates the effect of military expenditure on economic growth. |
| `ZhaEtAl2021` | Retain public administration: the [article](https://doi.org/10.1111/puar.13368) studies satisfaction with public services. It appeared online in 2021 and in a 2022 issue. |

The accepted DOI mapping is in tracked `final/doi_mapping.csv`; the Crossref
cache and candidate audit can be regenerated in `derived/`. Matching requires
title, journal, author, publication-year and article-type agreement.
`deWBek2017` required the
full surname "de Wit" for the author match; [Oxford's issue
record](https://academic.oup.com/jpart/issue/27/2) confirms the Crossref DOI.
`GreGer2019` is the [2019 fourth edition of *Get Out the
Vote*](https://www.brookings.edu/books/get-out-the-vote-2/), a book rather
than a journal article. JSTOR has a matching book-level stable record, but
an article DOI is not established. It remains missing for `doi_collection`;
no chapter or edition identifier is substituted.
