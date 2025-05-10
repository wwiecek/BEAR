# Scripts for fetching data from datasets

# Metapsy -----
remotes::install_github("metapsy-project/metapsyData")
library(metapsyData)
sets <- listData()$shorthand
sets <- sets[sets != "template"]

# Currently two datasets are broken, awaiting a fix
sets <- sets[sets != "suicide-psyctr"]
sets <- sets[sets != "psychosis-psyctr"]

metapsy_dt <- list()
for(i in 1:length(sets)) metapsy_dt[[sets[i]]] <- getData(sets[i])$data
saveRDS(metapsy_dt, file = "data/metaPsy/metapsy_dt.rds")

