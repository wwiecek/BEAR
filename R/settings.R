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
   "psymetadata" = "psychology",
   "OSC" = "OpenSciCollab",
   "Metapsy" = "Metapsy",
   "Sladekova" = "Sladekova et al",
   "WWC" = "What Works Clearing.",
   "Yang" = "Yang et al"   
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
   "euctr" = "curated",
   "Cochrane" = "meta",
   "CostelloFox" = "meta",
   "Head" = "scrape",
   "JagerLeek" = "scrape",
   "Metapsy" = "meta",
   "OSC" = "curated",
   "Sladekova" = "meta",
   "WWC" = "curated",
   "Yang" = "meta"   
   # "Adda" = "curated",
   # "ctgov" = "curated",
   # "Cochrane2019" = "meta",
)

# This is a vector to remove some fitted mixtures which we will not use in the paper
paper_do_not_include <- c("ctgov", "Cochrane2019", "euctr", "clinicaltrials")

cols_grp <- c("curated" = "#E41A1C", 
              "meta" = "#377EB8", 
              "scrape" = "#4DAF4A")
