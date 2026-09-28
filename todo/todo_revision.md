Yes. I went through the local `todo.md`, the current `work to-do.md`, other Obsidian material, the current `main` branch, and the recent BEAR conversations. The main thing I would change is to stop treating all these notes as equally live. Quite a few are completed investigations, duplicates, or questions that now have an answer.

For the scores below, **10 = I would do this now / it blocks the next phase; 8–9 = important next work; 6–7 = worthwhile; 4–5 = opportunistic; ≤3 = archive/remove unless circumstances change.** “REMOVE” means remove it from the active to-do list, not necessarily erase the historical note.

### A. Release, schema and documentation

| # | Score | Status | Item | Where it came from | Assessment |
|---|---:|---|---|---|---|
| **1** | **10** | **KEEP** | **Define the minimal v3 release checklist and actually release v3** | Recent chats + repo state | This is the biggest missing umbrella task. `NEWS.md` says “v3 — in development”; last release is v2 from 5 June, and direct downloads still fetch v2. Avoid letting another twenty enhancements become implicit prerequisites. |
| **2** | **9** | **KEEP** | **Add roadmap + contribution route to website** | **Local `.md`** | Still live. Explain what is coming next, how to contribute a dataset, and how to report a problem. Particularly valuable now that several people are offering data. |
| **3** | **9** | **KEEP** | **Turn suitable roadmap items into GitHub Issues** | **Local `.md`** | Current repo has no GitHub Issues. This is becoming awkward now that Johann/Tomas/etc. could work independently. Closely related to #2 but worth its own implementation task. |
| **4** | **3** | **REMOVE — essentially done** | Make download links prominent | **Obsidian `work to-do.md`** | Homepage now has an early callout to GitHub Releases plus “Get data” links. I would visually inspect once, then delete this todo. |
| **5** | **8** | **KEEP** | **State BEAR version clearly on website** | **Obsidian** | Still live. Website says “latest”, but not prominently “v2 release / v3 development”. This matters while `main` and the downloadable release differ. |
| **6** | **5** | **KEEP** | Improve shallow-clone instructions | Recent chat, 21 Sep | Submodule already uses `--depth 1`; main clone still says ordinary `git clone`. Tiny documentation task: `git clone --depth 1` plus shallow submodule. |
| **7** | **2** | **REMOVE — done** | Start/maintain `NEWS.md` | Recent v3 conversation | `NEWS.md` now exists and gives v2/v3 changes. Ongoing release notes are normal maintenance, not a todo. |
| **8** | **6** | **KEEP, narrower** | **Self-contained documentation of the mixture model** | **Local `.md`** | There is now a good selection page and README summary, but the full model still points users to the optimism paper. Add one concise model page: latent SNR mixture, selection, weighting, assurance, replication/sign quantities. |
| **9** | **6** | **MERGE/REFRAME** | Audit common schema, especially `source` | **Local `.md`** | The old “remove `source`” question has mostly been answered: it survives only for a few source-specific provenance cases. Replace with: “Can the remaining `source` uses be renamed/moved to better-specific fields?” |
| **10** | **5** | **KEEP** | Data dictionaries for richer public datasets | **Local `.md`** | CT.gov is done. None of the other proposed dictionaries appear done. Prioritise Cochrane/SCORE/Lang/WWC only when somebody actually needs them; don't mechanically create all ten. |
| **11** | **9** | **KEEP** | **Put DOI into combined `BEAR.rds` where applicable** | **Local `.md`** | Surprisingly still live. `doc/doi_action_plan.md` explicitly says the assembled `BEAR.rds` has no DOI column despite source datasets now having many DOIs. This looks like a cheap, useful v3 task. |
| **12** | **8** | **KEEP** | **Finish remaining DOI/article-ID enrichment** | **Local `.md` + recent DOI chats** | Priority candidates now clearly documented: OSC, WWC, Sladekova, Chavalarias; harder source-work cases include Arel-Bundock, Askarov, psymetadata, ManyLabs2, Szucs. Do **not** replace meaningful registry IDs with DOI. |
| **13** | **7** | **KEEP** | Tighten `studyid`/`metaid` semantics and documentation | Recent v3 chats | Generic documentation exists, but some IDs remain approximations. Especially useful before accepting outside PRs. |
| **14** | **6** | **KEEP, small** | Finish `topic` implementation for registries | **Local `.md` (`v3_registry_topics`) + recent chat** | Costello–Fox, Head and Metapsy topic work landed. CT.gov already contains rich domain classification but `build_bear.R` does not currently promote it to `topic`; EUCTR also lacks a common `topic`. |
| **15** | **1** | **REMOVE — done** | Zero/non-positive ratio-CI policy / continuity correction | **Local `.md`** | Current helper explicitly declares ratio CIs unusable unless estimate and both bounds are strictly positive, and falls back to p-derived z where possible. No arbitrary continuity correction. The original todo has been resolved. |
| **16** | **1** | **REMOVE — done** | Decide reported estimate versus CI midpoint for Wald intervals | Recent chat, 21 Sep | Settled: retain reported point estimate; infer SE from interval half-widths; use asymmetry as a diagnostic rather than replacing the estimate. Implemented/documented. |
| **17** | **1** | **REMOVE — done** | Non-Wald CI handling, t→p→normal-z, `z_operator` semantics | Recent chats | All now documented and implemented: non-Wald keyword flags, symmetry criterion, t with df → p → normal-equivalent z, and operator acting on \(|z|\). |

### B. Existing datasets: unresolved things

| # | Score | Status | Item | Source | Assessment |
|---|---:|---|---|---|---|
| **18** | **6** | **KEEP** | **Make Arel-Bundock `meta_id` labels intelligible** | **Local `.md`** | Still live. Current common data uses opaque labels such as `Ahm2014` as `subset` and explicitly leaves `topic = NA`. Map meta-analysis IDs to titles and preferably broad topics. |
| **19** | **5** | **KEEP** | Identify overlaps between Arel-Bundock and Askarov | **Local `.md`** | Useful mainly for understanding corpus overlap, less for core processing. DOI/citation enrichment will make this much easier. |
| **20** | **6** | **KEEP** | Investigate Bartos primary-study IDs | **Local `.md`** | Current `studyid=id` remains row-unique because the supplied source doesn't give an obvious primary-RCT identifier. Worth one serious investigation; if unrecoverable, document and close it permanently. |
| **21** | **8** | **KEEP** | **Resolve Lang hypothesis-selection/MHT question** | **Local `.md`** | Much has been clarified: 3,885 tests/736 articles, multiple principal hypotheses per article, one preferred specification per hypothesis; separate 736-value vector cannot currently be linked. Remaining useful question is whether `MHT` or other fields allow a principled reduced sample and whether results change. |
| **22** | **8** | **KEEP as action** | **Ask Kevin Lang the remaining selection questions** | **Obsidian + local `.md`** | A short author query could settle #21 much more cheaply than prolonged reverse engineering. Also ask whether there are newer/related economics datasets worth importing. |
| **23** | **4** | **MERGE** | Remove unnecessary Lang `source_*` variables | **Local `.md`** | Do as part of public-schema review, not as an independent project. Rich provenance is useful in the source `.rds`; just distinguish reader-facing metadata from audit fields. |
| **24** | **2** | **REMOVE** | Simplify Lang script by removing validation checks | **Local `.md`** | I would drop this todo. Once a pipeline is public/re-runnable, cheap checks on required rows/counts are useful even if you personally run it once. Little upside. |
| **25** | **5** | **KEEP** | Give Lang a useful `topic` | **Local `.md`** | Still absent from common BEAR. JEL classifications would be better than journal if readily available; otherwise a coarse economics-topic mapping could suffice. Not urgent. |
| **26** | **7** | **KEEP** | **Finish Sladekova study-ID work** | **Local `.md` + DOI chats** | Study IDs are much improved, but fallback row-unique IDs remain and DOI preservation still needs work. The current DOI plan explicitly lists Sladekova as unfinished. |
| **27** | **8** | **KEEP** | **Resolve Sladekova values with `abs(yi)>1`** | **Local `.md`** | Data-validity issue, therefore higher priority than cosmetic metadata. Determine whether those 43 rows are errors, another scale, or should be excluded before Fisher transformation. Ask Bartoš/Sladekova if necessary. |
| **28** | **7** | **KEEP, reduced scope** | **Prune CT.gov public schema** | **Local `.md`** | Some sub-tasks are already done: baseline fields have their own dictionary section; `p_sides` and `derivation_rule_id` no longer appear user-facing. Remaining question is mainly raw group metadata and whether `raw_measure` adds anything beyond `effect_source + measure_class + scale`. |
| **29** | **7** | **KEEP** | Add CT.gov subgroup-analysis flag | **Obsidian** | Still not present in current dictionary. Useful for Erik and generally for separating focal/full-sample analyses from subgroup analyses. |
| **30** | **8** | **REPLACE OLD TODO** | **Run and interpret CT.gov predictor analysis** | **Local `.md`** | The code now exists for effect source, phase, domain, trial type and measure plus descriptive summaries. Delete “write code” and replace with “run → inspect → decide which results matter”. |
| **31** | **8** | **KEEP** | CT.gov author-reported vs raw-derived paired comparison | **Local `.md`** | Code exists, but substantive result is still outstanding. This could be especially interesting because it directly measures how investigators' reported statistics compare with results reconstructed from underlying outcomes. |
| **32** | **7** | **KEEP** | Run CT.gov mixture fits for informative subgroups | **Local `.md`** | Also already coded. Run only after descriptive exploration identifies subsets worth fitting. |

### C. Cochrane and PubMed

| # | Score | Status | Item | Source | Assessment |
|---|---:|---|---|---|---|
| **33** | **10** | **KEEP** | **Modernise Cochrane ingestion: new data packages + legacy RM5** | **Obsidian + recent chat, 24 Sep** | One of the clearest major engineering tasks. Current repo still documents an RM5-only pipeline. Support newer Cochrane data-package ZIPs, retain RM5 for old reviews, validate equivalence. |
| **34** | **9** | **KEEP** | **Use Duncan Webb's Cochrane work rather than independently reconstructing it** | **Obsidian + recent email/chat** | He already has study→paper links, DOI/PMID, eligibility status, additional effect/SE rows, study metadata, RoB/PICO material. Decide what BEAR should import and what should remain linked externally. |
| **35** | **9** | **KEEP** | **Represent multiple publications per Cochrane study** | **Obsidian** | Explicitly marked in your notes and now central to article DOI enrichment. Requires separating review DOI, study ID and possibly multiple primary-publication IDs. |
| **36** | **8** | **KEEP** | Confirm current Cochrane redistribution/licensing route | Recent chat | Older Schwab/van Zwet correspondence was reassuring, but if you ingest newer Cochrane packages and redistribute richer row-level metadata, document exactly what is permitted. |
| **37** | **6** | **KEEP** | Enrich PubMed-derived data with article metadata | **Local `.md`** | Titles, journal/discipline, citation counts, article/study design, classification codes where possible, and prevalence of extractable effect types. Useful, but potentially a large rabbit hole. |
| **38** | **7** | **KEEP** | **100k-PubMed-abstract extraction/classification pilot** | **Obsidian** | Keep, but I would no longer start by blindly doing 100k. Use a bounded corpus/pilot first, then scale. |
| **39** | **7** | **KEEP** | PubMed selection as a function of number of reported p-values | **Local `.md`** | Nice, concrete question: one p-value versus many p-values per abstract and how apparent selection changes. More informative than another aggregate p-curve. |
| **40** | **6** | **KEEP** | PubMed “flowchart of studies” classification | **Local `.md`** | Fraction reporting p-values → prospective/retrospective → confirmatory/exploratory → whether significant statistic corresponds to a substantively positive result. Ambitious, likely needs LLM validation. |
| **41** | **1** | **REMOVE from project board** | Men's-height-on-dating-app analogy | **Local `.md`** | Keep in a talk-writing note if you like it. It is not a BEAR task. |

### D. New data sources and linkage

| # | Score | Status | Item | Source | Assessment |
|---|---:|---|---|---|---|
| **42** | **9** | **KEEP** | **Accept Tomas Havranek's economics dataset PR** | **Obsidian** | Very high benefit/cost: 49,845 estimates, 2,972 studies, 42 literatures, CC BY 4.0, and Tomas offered to map it himself. |
| **43** | **8** | **KEEP** | **Use TrialScout/Gustav Nilsson for CT.gov ↔ publication linkage** | **Obsidian** | Avoid building your own fuzzy matcher if their crosswalk is good. Establish accuracy/coverage and import durable NCT↔DOI links. |
| **44** | **8** | **KEEP** | **Use Jamie Cummins's ~4,000 depression-paper corpus as extraction testbed** | **Obsidian / conference** | Better first test of richer PubMed/statistical extraction than immediately doing arbitrary 100k abstracts. Could connect naturally to RegCheck. |
| **45** | **7** | **KEEP** | Review Ryan Briggs's new extraction work | **Obsidian / conference** | Specifically to avoid duplicating extraction infrastructure and see whether parts can become reusable BEAR pipeline components. |
| **46** | **6** | **KEEP** | Investigate MetaCheck data extraction | **Obsidian** | Ask Daniel Lakens what structured study/statistic data are exposed and whether it supplies genuinely new fields/corpus coverage. |
| **47** | **7** | **KEEP** | Add Retraction Watch/correction status by DOI/PMID | **Obsidian** | Simple, interpretable enrichment if licence/API terms permit. Could later support forensic analyses. |
| **48** | **7** | **KEEP** | Gilad Feldman's quantitative-metascience data contribution | Conference/recent conversation | Obtain corpus, provenance, extraction code and licence first; then see whether it belongs inside BEAR or interoperates externally. |
| **49** | **7** | **KEEP** | FORRT replication data integration | Conference/recent conversation | Potentially enlarges BEAR's small replication corpus and gives another validation set for replication prediction. |
| **50** | **7** | **KEEP, batch project** | Add Roodman-derived candidate datasets: Vivalt; Gerber–Malhotra political science and sociology; Schuemie | Recent chat, 15 Sep | These were concrete downloadable candidates. Check duplicate coverage before processing, especially Schuemie versus current medical scrapes and G–M versus Arel-Bundock. |
| **51** | **6** | **KEEP as backlog** | Other strong dataset candidates: Jerke 2025, Brodeur 2026, MetaLab, Turner FDA, van den Akker preregistration pairs, Kühberger, Camerer | Recent chat | Good queue after v3. I would not process all at once. Jerke/Brodeur/MetaLab look particularly natural for BEAR. |
| **52** | **3** | **ARCHIVE separately** | Many-analyst datasets: Breznau, Menkveld, Gould etc. | Recent chat | Interesting metascience, but fundamentally different observation structure. Don't distort BEAR merely to include them. |
| **53** | **8** | **KEEP** | Reconcile/update SCORE with final/current release | Recent dataset conversation | Important because SCORE is especially useful for the replication/reproducibility agenda and the proposed calibration project. |
| **54** | **7** | **KEEP** | Ask Abel Brodeur whether current BEAR treatment is right + what newer data exist | **Obsidian** | Combine source validation with identifying Brodeur 2016/2020/2026 datasets worth incorporating. |

### E. Actual research using BEAR

| # | Score | Status | Item | Source | Assessment |
|---|---:|---|---|---|---|
| **55** | **9** | **KEEP** | **Replication-prediction baseline/calibration using OSC, Many Labs and SCORE** | **Obsidian** | Probably the cleanest new quantitative-metascience project. Hide replication results; predict from originals using BEAR model; compare realised calibration to simple baselines and existing human forecasts where commensurable. |
| **56** | **8** | **KEEP** | **Measure power against scientifically relevant effects** | **Obsidian** | Stronger than simply reporting conventional power. Potential data via Daniel Lakens/psychology and Pavlos Msaouel/oncology. Directly relevant to replication-funding policy. |
| **57** | **8** | **KEEP** | Refit meta-analyses wherever possible and characterise heterogeneity | **Local `.md`** | Very good BEAR-wide analysis. Heterogeneity is directly relevant to what “replication” should mean and to the choice between another study and synthesis. |
| **58** | **8** | **KEEP, after dependencies** | PubMed vs CT.gov vs Cochrane with Nick DeVito | **Obsidian** | Strong paper idea once Cochrane refresh/linkage and CT.gov v3 are stable. Compare how evidence changes across registry, publication and synthesis layers. |
| **59** | **7** | **KEEP** | Cross-dataset analysis of which standard metascience claims generalise | Other Obsidian material + recent BEAR talk | This is arguably BEAR's main scientific raison d'être: publication selection, apparent power, replication etc. across qualitatively different sampling frames. Could absorb parts of the “Myths of metascience” work. |
| **60** | **6** | **KEEP** | Re-use Roodman/filtration datasets in your modelling work | **Obsidian `work to-do.md`** | Research-project rather than BEAR-maintenance task. Keep linked to the Gelman/optimism work rather than cluttering the BEAR engineering queue. |
| **61** | **5** | **KEEP as exploratory** | Inspect Anne Scheel's project for BEAR-relevant data/questions | **Obsidian** | Worth an hour; not yet a project. |
| **62** | **4** | **MERGE/ARCHIVE** | “Making BEAR more data rich” | **Local `.md`** | Too vague to remain an active todo. Its concrete descendants are already #18–20, #27, #37 and #42–54. Delete the umbrella item once those are captured. |

### F. Outputs, interoperability and longer-term ambitions

| # | Score | Status | Item | Source | Assessment |
|---|---:|---|---|---|---|
| **63** | **6** | **KEEP, longer-term** | **Define BEAR-compatible schema/benchmark for AI extraction and integrity tools** | AI-for-Research-Integrity notes + recent chat | Useful conference-derived ambition: same corpus can benchmark extraction, reproducibility and integrity tools. Don't let this block v3. |
| **64** | **6** | **KEEP** | Coordinate with Dan Elton's combined replication database | Conference notes/recent chat | Compare schemas and divide labour rather than maintaining overlapping replication databases independently. |
| **65** | **5** | **KEEP, exploratory** | Registration→paper→code/data→effect/check pipeline / RegCheck interoperability | Conference discussions | Potentially powerful, but this is really another infrastructure programme. Start with Jamie's bounded corpus before generalising. |
| **66** | **4** | **MERGE** | CTA BEAR announcement + Clinical Trials Abundance BEAR blog | **Obsidian** | You have effectively two writing todos describing the same family of output. Merge into one “BEAR/clinical-trials article once CT.gov results are worth writing about”. |
| **67** | **3** | **OPPORTUNISTIC** | Write/collaborate with The 100% CI about BEAR | **Obsidian** | Fine outreach opportunity, but no reason to carry it on the active technical roadmap. |

### What I would actually keep on the active board

The list above contains **67 historical/current ideas**, but I would only have roughly a dozen in an active BEAR board. The centre of gravity should be: **cut v3; contributor/issue infrastructure; DOI in `BEAR.rds`; finish the important ID/data-integrity issues; Cochrane refresh + Duncan integration; Tomas PR; CT.gov analysis; SCORE update; replication prediction; and then the PubMed/CT.gov/Cochrane research programme**.

There are also some very clear deletions from your local file: the ratio-CI policy, Wald midpoint question, non-Wald/t/z rules, starting `NEWS.md`, Lang “remove validation checks”, the generic “make BEAR more data rich”, and the men's-height joke as a project task. Several CT.gov sub-items have also already been done: the baseline dictionary grouping and removal of `p_sides`/`derivation_rule_id` from the public dictionary.

The strongest thing your existing lists are missing is **#1: release management**. You have spent a lot of effort making `main` substantially better than v2, but users following the documented direct-download command still receive v2. I would use “what absolutely must happen before v3?” as the filter for the next technical pass, and explicitly defer everything else.

If I were converting this into the next working document, I would make only three buckets: **v3 blockers**, **next research/data projects**, and **backlog/archive**, and assign Johann/Marcus/external collaborators against the corresponding numbered items above.