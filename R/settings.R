bear_names <- c(
  "ArelBundock" = "Arel-Bundock et al",
  "Askarov" = "Askarov et al",
  "Bartos" = "Bartos et al",
  "BarnettWren" = "Barnett and Wren",
  "Brodeur" = "Brodeur et al",
  "Lang" = "Lang",
  "ctgov_euctr" = "clinicaltrials.gov + EU CTR",
  "clinicaltrials" = "clinicaltrials.gov",
  "euctr" = "EUDRA",
  "Chavalarias" = "Chavalarias et al",
  "Cochrane" = "Cochrane",
  "CostelloFox" = "Costello and Fox",
  "Head" = "Head et al",
  "JagerLeek" = "Jager and Leek",
  "OSC" = "OpenSciCollab replications",
  "SCORE_claims" = "SCORE, all claims",
  "SCORE_replications" = "SCORE replications",
  "Metapsy" = "Metapsy",
  "Sladekova" = "Sladekova et al",
  "WWC" = "What Works Clearing.",
  "Yang" = "Yang et al",   
  "psymetadata" = "psymetadata",
  "Nuijten" = "Nuijten et al",
  "ManyLabs2" = "Many Labs 2",
  "SCORE original" = "SCORE original",
  "Many Labs original" = "Many Labs original",
  "OSC original" = "OSC original",
  "Szucs" = "Szucs and Ioannidis"
  # "Adda" = "Adda et al",
  # "ctgov" = "clinicaltrials (v2 cut)",
  # "Cochrane2019" = "Cochrane (2019 cut)",
)

bear_labels <- c(
  "ArelBundock" = "Arel-Bundock et al\npolitical science",
  "Askarov" = "Askarov et al\neconomics",
  "Bartos" = "Bartoš et al\nexercise",
  "BarnettWren" = "Barnett and Wren\nMedline ratios",
  "Brodeur" = "Brodeur et al\neconomics",
  "Lang" = "Lang\neconomics",
  "ctgov_euctr" = "clinicaltrials.gov + EU CTR\nclinical trials",
  "clinicaltrials" = "clinicaltrials.gov",
  "euctr" = "EU CTR\nclinical trials",
  "Chavalarias" = "Chavalarias et al\nMedline & PubMed",
  "Cochrane" = "Cochrane Database of\nSystematic Reviews",
  "CostelloFox" = "Costello and Fox\necology & evolution",
  "Head" = "Head et al\nbiomedicine",
  "JagerLeek" = "Jager and Leek\nbiomedicine",
  "OSC" = "OpenSciCollab replications\npsychology",
  "SCORE_claims" = "SCORE claims\nsocial & behav. sci.",
  "SCORE_replications" = "SCORE replications\nsocial & behav. sci.",
  "Metapsy" = "Metapsy\npsychotherapy",
  "Sladekova" = "Sladekova et al\npsychology",
  "WWC" = "What Works Clearing.\neducation",
  "Yang" = "Yang et al\necology & evolution",
  "psymetadata" = "psymetadata\npsychology",
  "Nuijten" = "Nuijten et al\nintelligence",
  "ManyLabs2" = "Many Labs 2\npsychology",
  "SCORE original" = "SCORE original\nsocial & behav. sci.",
  "Many Labs original" = "Many Labs original\npsychology",
  "OSC original" = "OpenSciCollab original\npsychology",
  "Szucs" = "Szucs & Ioannidis\ncognitive neuroscience"
  # "Adda" = "Adda et al",
  # "ctgov" = "clinicaltrials (v2 cut)",
  # "Cochrane2019" = "Cochrane (2019 cut)",
)

bear_domain <- c(
  "ArelBundock"   = "political science",
  "Askarov"       = "economics",
  "Bartos"        = "exercise",
  "BarnettWren"   = "biomedicine",
  "Brodeur"       = "economics",
  "Lang"          = "economics",
  "ctgov_euctr"   = "clinical trials",
  "clinicaltrials"= "clinical trials",
  "euctr"         = "clinical trials",
  "Chavalarias"   = "biomedicine",
  "Cochrane"      = "medicine & health",
  "CostelloFox"   = "ecology & evolution",
  "Head"          = "biomedicine",
  "JagerLeek"     = "biomedicine",
  "ManyLabs2"     = "psychology",
  "SCORE original" = "social & behavioural sciences",
  "Many Labs original" = "psychology",
  "OSC original" = "psychology",
  "Metapsy"       = "psychotherapy",
  "Nuijten"       = "intelligence",
  "OSC"           = "psychology",
  "SCORE_claims"  = "social & behavioural sciences",
  "SCORE_replications" = "social & behavioural sciences",
  "psymetadata"   = "psychology",
  "Sladekova"     = "psychology",
  "WWC"           = "education",
  "Yang"          = "ecology & evolution",
  "Szucs"         = "cognitive neuroscience"
)

missing_dataset_labels <- setdiff(names(bear_domain), names(bear_labels))
if (length(missing_dataset_labels) > 0) {
  stop("Missing dataset labels for: ",
       paste(missing_dataset_labels, collapse = ", "))
}



if (!exists("dataset_classification_file")) {
  dataset_classification_file <- "doc/dataset_classification.csv"
}
bear_df_classes <- read.csv(dataset_classification_file, stringsAsFactors = FALSE)

bear_class_cols <- c("replication", "meta_analysis", "database",
                     "metascience_paper", "pubmed_scrape", "targeted",
                     "random_sample", "primary_outcome", "rct",
                     "workflow_classification", "summary_group")
bear_dataset_classes <- lapply(bear_df_classes[bear_class_cols], setNames,
                               nm = bear_df_classes$dataset)

bear_data_summary_group_levels <- c(
  "Databases", "Metascience datasets", "Replication efforts", "PubMed/Medline scraped data")

missing_dataset_classification <- setdiff(names(bear_domain),
                                          bear_df_classes$dataset)
if (length(missing_dataset_classification) > 0) {
  stop("Missing dataset classification for: ",
       paste(missing_dataset_classification, collapse = ", "))
}

# This is a vector to remove some fitted mixtures 
# which we will not use in the BEAR paper
paper_do_not_include <- c(
  # "ctgov", "Cochrane2019", 
  # "euctr", "clinicaltrials",
  "Sladekova", 
  "ManyLabs2", "Many Labs original",
  "Head", "JagerLeek",
  "Yang")

bear_colors <- c(
  # "curated"      = "#E41A1C",
  "curated"      = "#377EB8",
  "meta"         = "#377EB8",
  "scrape"       = "#4DAF4A",
  "replications" = "#B8860B"
)
