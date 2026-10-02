# Arel-Bundock primary-paper DOI review

`doi_study` identifies a publication represented by a Briggs study label. It
does not identify the source meta-analytic publication (`doi_collection`), an
individual `question_id` synthesis, or a unique underlying experiment.

The source inventory covers all 46 `meta_id` values. Crossref records provide
reference lists for the 45 source publications with an established DOI; the
remaining source is the book `GreGer2019`. The 33 source publications with
Briggs rows have 2,717 deposited references, including 1,776 distinct cited
DOIs. A refresh writes the source inventory and Briggs references with
resolved bibliographic metadata to `derived/`. The local source package
also contains selected
supplements and tables. The accessible full article for `BhaDahHan2019` was
checked against its deposited reference list; its bibliography includes
working papers and other items without journal DOIs.
The full `BhaDahHan2019` PDF is saved locally under
`data_raw/ArelBundock/source_articles/`. Seven other PDF links returned by
OpenAlex did not permit direct download. This pass uses the deposited
reference lists and local supplements without a full-text audit of every
source article.

The accepted mapping uses `meta_id`, `study_id`, `study_year` and
`study_journal` together. A cited DOI is attached only when its publication is
a journal article, its first author's surname starts the source study label,
its year agrees with the study label or cited reference, any cited title
agrees with the DOI record, any available source journal agrees, and no second
DOI passes those checks for that key. DOI-only publisher references are
expanded with OpenAlex metadata; the DOI must still appear in the source
article's deposited reference list. References without a cited DOI are searched
only when a title, author and year are available. API responses and candidate
tables can be regenerated under `derived/`; accepted mappings are tracked in
`final/`.

Twenty-four further source references had a title, first author and year but
no deposited DOI. A separate Crossref lookup accepted eight after checking
title, author, year, publication type and available journal metadata. The
other 16 remain in the candidate audit. The shared lookup now retains author
and year for searches without a journal so these checks are possible.

The current mapping covers 440 distinct Briggs source keys, 405 distinct DOIs,
and 3,788 of 16,649 Arel-Bundock rows across 26 Briggs source publications.
There are 36 keys with multiple plausible DOIs and 11 with a plausible
non-journal publication; they remain missing. Another 895 Briggs keys have no
eligible cited DOI under the direct-reference rules; eight of these were
resolved by the title-based lookup, leaving 887 without a candidate from
either route. `ArcNic2009`, `BurKosLan2013`,
`EshEtAl2021`, `MatKnoValHopSik2019`, `MunRam2021`, `OweLi2020` and
`YesYes2019` have no accepted primary-paper DOI. The latter two use numeric
study labels; `EshEtAl2021` and `MatKnoValHopSik2019` have missing study
labels in the processed data. Doucouliagos rows are outside this matching
pass and remain missing.

The source labels sometimes contain another author's surname later in the
text. Requiring the candidate's first surname at the *start* of the label
prevents, for example, a paper by Jin from being assigned to a label starting
with Lynggaard. Citation years can refer to the printed issue rather than
first online publication. Four distinct DOI records with a two-year difference
were checked against Crossref's online and print dates: DOI
`10.1177/0275074016652243`, `10.1017/S0007123416000168`,
`10.1007/s11127-010-9749-8`, and `10.1080/00036846.2011.599787`.
The cited titles and authors agree. Study labels can also name distinct
experiments within one paper, so a repeated `doi_study` does not imply that
those rows are duplicate estimates or the same experiment.

The current reference lists do not establish a DOI for every included paper.
The next pass should recover included-study bibliographies from the source
articles and supplements for the unresolved Briggs groups, then build a
source-specific crosswalk for the Doucouliagos numeric IDs. Keep candidate
and rejected matches in the local audit; do not fill missing values with the
source meta-analysis DOI or a similarly titled publication.
