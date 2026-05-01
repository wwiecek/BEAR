knit_with_df <- function(dataframe, fit = NULL, rmd_file = "template.Rmd", output_file = NULL) {
  # If output file is not specified, create one based on the input file name
  if (is.null(output_file)) {
    # Replace .Rmd extension with .pdf (since the template outputs PDF)
    output_file <- gsub("\\.Rmd$|\\.rmd$", ".pdf", rmd_file)
  }
  
  # Create a temporary environment to store the data frame
  params_env <- new.env(parent = globalenv())
  
  # Assign the data frame to the environment
  d <- dataframe
  params_env$d <- d
  if(!is.null(fit)) 
    params_env$fit <- fit
  else {
    operator <- ifelse(d$truncated, "<", "=")
    fit <- fit_mixture(z = d$z, operator = operator, k = 4, weights = 1 / d$k)
    params_env$fit <- fit
  }
  
  # Use rmarkdown::render with envir parameter to pass the data frame
  rmarkdown::render(
    input = rmd_file,
    output_file = output_file,
    envir = params_env,
    quiet = FALSE  # Set to TRUE to suppress output messages
  )
  
  message("Report generated: ", output_file)
  return(output_file)
}
