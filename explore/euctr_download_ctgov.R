# CTGOV2

# Connect or create local SQLite database
db <- nodbi::src_sqlite(dbname = "ctgov_trials.sqlite", collection = "ctgov")

# Loop over time periods to download in batches
if (!file.exists("ctgov_trials.sqlite")){
  for (i in 1:(length(date_seq)-1)) {
    query <- ctrGenerateQueries(
      startAfter=date_seq[i],
      startBefore=date_seq[i+1],
      onlyWithResults = TRUE
    )
    download_batch(query, db, collection="CTGOV2")
  }
}

# to see fields, do for example:
# ctrShowOneTrial("NCT06956170", con = db)

result <- dbGetFieldsIntoDf(
  fields = c("protocolSection.designModule.designInfo.allocation", 
             "protocolSection.designModule.designInfo.interventionModel", 
             "protocolSection.designModule.designInfo.maskingInfo.masking", 
             "protocolSection.designModule.designInfo.primaryPurpose", 
             "protocolSection.designModule.phases", 
             "protocolSection.designModule.studyType",
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciLowerLimit",
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciNumSides", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciPctValue", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciUpperLimit", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.dispersionType", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.dispersionValue", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.nonInferiorityType", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValue", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValueComment", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.paramType", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.paramValue", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.statisticalComment", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.statisticalMethod", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.description", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.paramType", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.title", 
             "resultsSection.outcomeMeasuresModule.outcomeMeasures.type", 
             "protocolSection.sponsorCollaboratorsModule.leadSponsor.class", 
             "protocolSection.sponsorCollaboratorsModule.leadSponsor.name",
             "protocolSection.statusModule.studyFirstSubmitDate"),
  calculate = c(
    "f.startDate",
    "f.trialPhase",             # phase
    "f.statusRecruitment",      # early stopping
    "f.sampleSize",             # compute .primaryEndpointFirstPsize
    "f.primaryEndpointResults"  # compute .primaryEndpointFirstPvalue
  ), 
  con = db
)


d2=data.frame(id=result$"_id",
              phase=result$.trialPhase,
              date=result$protocolSection.statusModule.studyFirstSubmitDate,
              completion_date=as.Date(NA),
              endpoint=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.type,
              endpoint_title=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.title,
              purpose=result$protocolSection.designModule.designInfo.primaryPurpose,
              intervention=result$protocolSection.designModule.studyType,
              n=result$.primaryEndpointFirstPsize,
              pval=result$.primaryEndpointFirstPvalue,
              trunc=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.pValue,
              estimate=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.paramValue,
              lower=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciLowerLimit,
              upper=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciUpperLimit,
              estimand=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.paramType,
              side=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciNumSides,
              level=result$resultsSection.outcomeMeasuresModule.outcomeMeasures.analyses.ciPctValue)

get_trunc = function(strn){
  first_char <- substr(strn, 1, 1)
  if (first_char %in% c("=", "<", ">")) {
    return(first_char)
  } else {
    return("=")
  }
}
d2$trunc=sapply(d2$trunc, get_trunc)

d2 = d2 %>% rowwise %>% mutate(across(c(endpoint,estimand,side), process_string,to_numeric=FALSE))
d2 = d2 %>% rowwise %>% mutate(across(c(estimate,lower,upper,level), process_string))

d2=cbind(collection="CTGOV",d2)

d=bind_rows(d1,d2)
