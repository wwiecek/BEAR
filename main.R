# Master script for BEAR workflow


# (1) re-generating BEAR dataset

# This is optional
# To download data, you need to link to DataBEAR ...
# this will create data/ folder

# We do not include data/ (transformed datasets which are combined into BEAR)
# or data_raw/ (raw datasets which are processed into data/)

# We do include all of the scripts that processed raw data in process/
# Stitching together of cleaned up datasets is done in...

source("workflow/build_bear.R")
# Output: data/BEAR.rds


# (2) post-process data to generate datasets for fitting mixtures

# This is a specific design decision (thinning datasets, 
# study weights according to N rows in each study) which
# you only need if you'd like to fit the mixtures as we do in the paper

source("workflow/postprocess.R")
# output: paper/bear_lists.Rdata


# (3) fit mixture model to sets of |z| values
source("workflow/fit_mixtures.R")
# output: mixtures/

# (4) calculate performance of each fit
# (power, sign, replication)

source("workflow/calculate_psr.R")
# output: paper/power_sign_rep.csv
