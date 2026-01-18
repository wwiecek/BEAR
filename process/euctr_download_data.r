setwd(this.path::here())

library(ctrdata)
library(nodbi)
library(DBI)
library(RSQLite)
library(jsonlite)
library(dplyr)

# From: https://cran.r-project.org/web/packages/ctrdata/ctrdata.pdf
# • EUCTR: The EU Clinical Trials Register holds more than 44,350 clinical trials 
# (at least one investigational medicinal product, IMP; in the European Union and beyond), 
# including more than 25,400 trials with results, which continue to be added.
# 
# • CTIS: The EU Clinical Trials Information System, launched in 2023, holds 
# about 9,800 publicly accessible clinical trials, including more than 270 with 
# results or a report (only as PDF cannot load any CTIS results.) No results in a structured
# electronic format are foreseeably available.
# 
# • CTGOV2: ClinicalTrials.gov holds more than 550,000 interventional and observational 
# studies, including almost 70,000 interventional studies with results.
# 
# • ISRCTN: The ISRCTN Registry holds almost 26,800 interventional and observational 
# health studies, including more than 14,600 studies with results (as references). 
# No results in a structured electronic format are foreseeably available.
#
# Important: ctrdata-trial-concepts Trial concepts implemented across registers

# Connect or create local SQLite database
db = nodbi::src_sqlite(dbname = "euctr_trials.sqlite", collection = "euctr")

# Function to download batch
download_batch = function(query, db, collection = "EUCTR") {
  ctrLoadQueryIntoDb(
    queryterm = query[collection],
    con = db,
    euctrresults = TRUE,
    euctrprotocolsall = FALSE
  )
}

# Loop over time periods to download in batches
start_date = as.Date("1990-01-01")
end_date = as.Date("2026-01-01")
date_seq = seq.Date(from = start_date, to = end_date, by = "year")

if (!file.exists("euctr_trials.sqlite")){
  for (i in 1:(length(date_seq)-1)) {
    query = ctrGenerateQueries(
      startAfter=date_seq[i],
      startBefore=date_seq[i+1],
      onlyWithResults = TRUE
    )
    download_batch(query, db, collection="EUCTR")
  }
}

# to see fields, do for example:
# ctrShowOneTrial("2023-000150-20-DK", con = db)

# Get result set
result <- dbGetFieldsIntoDf(
  fields = c("eudractNumber",
             "x6_date_on_which_this_record_was_first_entered_in_the_eudract_database",
             "x7_date_of_the_end_of_the_trial",
             "a3_full_title_of_the_trial", 
             "b1_sponsor.b11_name_of_sponsor", 
             "e51_primary_end_points", 
             "e511_timepoints_of_evaluation_of_this_end_point", 
             "e65_efficacy", 
             "e64_safety", 
             "e811_randomised", 
             "e814_double_blind", 
             "e816_cross_over", 
             "e815_parallel_group",
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.valueEqualityRelation", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.method.value", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.type.value", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.pointEstimate", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.upperLimit", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.sides.value", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.percentage", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.lowerLimit", 
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.analysisSpecification.value",
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.type.value",
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.title",
             "endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.subjectAnalysisSetComparisonGroupId", 
             "endPoints.endPoint.subjectAnalysisSetReportingGroups.subjectAnalysisSetReportingGroup.subjects", 
             "endPoints.endPoint.title", 
             "endPoints.endPoint.type.value", 
             "endPoints.endPoint.unit", 
             "endPoints.endPoint.subjectAnalysisSetReportingGroups.subjectAnalysisSetReportingGroup.id", 
             "endPoints.endPoint.subjectAnalysisSetReportingGroups.subjectAnalysisSetReportingGroup.subjectAnalysisSetId"), 
  calculate = c(
    "f.trialPhase",             # phase
    "f.statusRecruitment",      # early stopping
    "f.sampleSize",             # compute .primaryEndpointFirstPsize
    "f.primaryEndpointResults"  # compute .primaryEndpointFirstPvalue
  ), 
  con = db
)

# Disconnect when done
DBI::dbDisconnect(db$con, shutdown = TRUE)

process_string <- function(txt,to_numeric=TRUE) {
  txt <- gsub("\\s+", "", txt)                 # Remove all whitespace
  txt <- gsub(",", ".", txt)                   # Replace commas with periods
  txt <- strsplit(txt, "/", fixed = TRUE)[[1]] # Split on "/"
  txt <- txt[1]                                # Take the first element
  if (to_numeric) txt=as.numeric(txt)          # Convert to numeric
  return(txt)
}

d1=data.frame(id=result$"_id",
             phase=result$.trialPhase,
             date=result$x6_date_on_which_this_record_was_first_entered_in_the_eudract_database,
             completion_date=result$x7_date_of_the_end_of_the_trial,
             endpoint=result$endPoints.endPoint.type.value,
             endpoint_title=result$endPoints.endPoint.title,
             efficacy=result$e65_efficacy,
             n=result$.primaryEndpointFirstPsize,
             pval.calc=result$.primaryEndpointFirstPvalue,
             pval=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.value,
             trunc=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.statisticalHypothesisTest.valueEqualityRelation,
             estimate=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.pointEstimate,
             lower=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.lowerLimit,
             upper=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.upperLimit,
             estimand=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.type.value,
             side=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.sides.value,
             level=result$endPoints.endPoint.statisticalAnalyses.statisticalAnalysis.parameterEstimate.confidenceInterval.percentage)

d1 = d1 %>% rowwise %>% mutate(across(c(endpoint,trunc,estimand,side), process_string,to_numeric=FALSE))
d1 = d1 %>% rowwise %>% mutate(across(c(pval, estimate,lower,upper,level), process_string))
d1$endpoint=gsub("ENDPOINT_TYPE.","",d1$endpoint)
d1$estimand=gsub("PARAMETER_TYPE.","",d1$estimand)
d1$side=gsub("CONF_INTERVAL_SIDE.","",d1$side)
d1=cbind(collection="EUCTR",d1)

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

saveRDS(d, file = "data_raw/eutrials/data_euctr_ctgov.rds")
# write.csv(d, file = "data/eutrials/data_euctr_ctgov.csv")

# Disconnect when done
DBI::dbDisconnect(db$con, shutdown = TRUE)
