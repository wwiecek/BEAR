# Download data from 
# https://datadryad.org/dataset/doi:10.5061/dryad.79d43
# accessed by WW June 2025
library(tidyverse)

# WW the original analysis file was not available, but a different raw data file that
# seems to match it well is available, so I load that instead
# Load the p values data file, and the file containing the FoR categories for each journal
# d <- read.csv("raw_data/extracted.p.values.11-June-2014.cleaned.csv", row.names=1)

d <- read.csv("data_raw/Head/1. TEXT_MINING/raw_data/p.values.csv")
journal.categories <- read.csv("data_raw/Head/1. TEXT_MINING/raw_data/journal.categories.csv", row.names=1)

# Get rid of papers that did not yield any p values
d <- d[!is.na(d$p.value), ]
# only keep papers with a single DOI (tyipcally we lose ~100K files here)
# NB some papers really have 0 or >1 DOI
d <- d[which(d$num.dois==1),]

# WW comment: I used a different col as this probably got renamed to d$num.results.sections.found
# only keep papers for which we have >0 results sections (e.g. reviews and commentaries often have 0 results sections)
d <- d[which(d$num.results.sections.found > 0),]

# some papers have (legitimately, I've checked) zero authors. Remove these.
d <- d[which(d$num.authors>0),]

# some journals publish large 'supplements' that contain conference many short conference abstracts
# (with results sections) in one file. Remove these
d <- d[-c(grep("(Suppl)", d$file.name)),]

# we have a few records in this dataset in which the journal.name wasn't extracted properly, we fix that here
d$journal.name <- as.character(d$journal.name)
fixed.names <- as.character(d$folder.name[which(is.na(d$journal.name))])
fixed.names <- gsub("_", " ", fixed.names)
d$journal.name[which(is.na(d$journal.name))] <- fixed.names
d$journal.name <- as.factor(d$journal.name)

# now we add in the FoR categories 
journal.categories$journal.name <- journal.categories$Abbreviation
d <- merge(d, journal.categories,by="journal.name")

# WW addition: join PMIDs created by grab_pmid.R and then write to new RDS
pmid <- readRDS("data_raw/Head/doi2pmid_progress.rds")

d %>% 
  left_join(rename(pmid, first.doi = doi)) %>% 
  select(journal.name, first.doi, pmid, p.value, operator, section, Category, year) %>% 
  saveRDS("data/Head.rds") #compression: 30x smaller than CSV
  # write_csv("data_raw/Head/p_values_cleaned_ww.csv")

rm(d)
rm(pmid)
