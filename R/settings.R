bear_names <- c(
   "ArelBundock" = "Arel-Bundock et al",
   "Askarov" = "Askarov et al",
   "Bartos" = "Bartoš et al",
   "BarnettWren" = "Barnett and Wren",
   "Brodeur" = "Brodeur et al",
   "ctgov_euctr" = "ctgov / EU CTR",
   "clinicaltrials" = "clinicaltrials.gov",
   "euctr" = "EUDRA",
   "Chavalarias" = "Chavalarias et al",
   "Cochrane" = "Cochrane",
   "CostelloFox" = "Costello and Fox",
   "Head" = "Head et al",
   "JagerLeek" = "Jager and Leek",
   "OSC" = "OpenSciCollab",
   "Metapsy" = "Metapsy",
   "Sladekova" = "Sladekova et al",
   "WWC" = "What Works Clearing.",
   "Yang" = "Yang et al",   
   "psymetadata" = "psychology",
   "Nuijten" = "Nuijten et al",
   "ManyLabs2" = "Many Labs 2"
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
  "ctgov_euctr" = "ctgov & EU CTR\nclinical trials",
  # "clinicaltrials" = "clinicaltrials.gov",
  # "euctr" = "EUDRA",
  "Chavalarias" = "Chavalarias et al\nMedline & PubMed",
  "Cochrane" = "Cochrane Database of\nSystematic Reviews",
  "CostelloFox" = "Costello and Fox\necology & evolution",
  # "Head" = "Head et al",
  # "JagerLeek" = "Jager and Leek",
  "OSC" = "OpenSciCollab\npsychology",
  "Metapsy" = "Metapsy\npsychotherapy",
  # "Sladekova" = "Sladekova et al",
  "WWC" = "What Works Clearing.\neducation",
  "Yang" = "Yang et al\necology & evolution",
  "psymetadata" = "metapsydata\npsychology",
  "Nuijten" = "Nuijten et al\nintelligence",
  "ManyLabs2" = "Many Labs 2\npsychology"
  # "Adda" = "Adda et al",
  # "ctgov" = "clinicaltrials (v2 cut)",
  # "Cochrane2019" = "Cochrane (2019 cut)",
)

bear_classification <- c(
   "ArelBundock" = "meta",
   "Askarov" = "meta",
   "Bartos" = "meta",
   "BarnettWren" = "scrape",
   "Brodeur" = "curated",
   "Chavalarias" = "scrape",
   "ctgov_euctr" = "curated",
   "clinicaltrials" = "curated",
   "Cochrane" = "meta",
   "CostelloFox" = "meta",
   "euctr" = "curated",
   "Head" = "scrape",
   "JagerLeek" = "scrape",
   "ManyLabs2" = "replications",
   "Metapsy" = "meta",
   "Nuijten" = "curated",
   "OSC" = "replications",
   "psymetadata" = "curated",
   "Sladekova" = "meta",
   "WWC" = "curated",
   "Yang" = "meta"
   # "Adda" = "curated",
   # "ctgov" = "curated",
   # "Cochrane2019" = "meta",
)

# This is a vector to remove some fitted mixtures 
# which we will not use in the BEAR paper
paper_do_not_include <- c("ctgov", "Cochrane2019", 
                          "euctr", "clinicaltrials",
                          "Sladekova", "Head", "JagerLeek",
                          "Yang")

bear_colors <- c(
  "curated"      = "#E41A1C",
  "meta"         = "#377EB8",
  "scrape"       = "#4DAF4A",
  "replications" = "#B8860B"
)

