# Metapsy data fetched 
devtools::install_github("metapsy-project/metapsyData")
library(metapsyData)
sets <- listData()$shorthand
sets <- sets[sets != "template"]

# Currently (May 2025) two datasets are broken, awaiting a fix
# sets <- sets[sets != "suicide-psyctr"]
# sets <- sets[sets != "psychosis-psyctr"]
# This was fixed in May 2025 by Matthias Harrer

metapsy_dt <- list()
for(i in 1:length(sets)) metapsy_dt[[sets[i]]] <- getData(sets[i])$data
saveRDS(metapsy_dt, file = "data_raw/Metapsy/Metapsy_Jan2026.rds")
