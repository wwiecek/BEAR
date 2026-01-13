stitch_and_knit <- function(description_rmd, template_rmd, dataframe, fit = NULL, output_file = NULL) {
  # Create a temporary file in the current working directory
  temp_filename <- paste0("temp_combined_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rmd")
  temp_file <- file.path(getwd(), temp_filename)
  
  # Read and combine the files
  tryCatch({
    desc_content <- readLines(description_rmd)
    template_content <- readLines(template_rmd)
    
    # Find YAML sections
    desc_yaml_indices <- which(desc_content == "---")
    if (length(desc_yaml_indices) < 2) {
      stop("Description file missing proper YAML header")
    }
    yaml_end <- desc_yaml_indices[2]
    
    template_yaml_indices <- which(template_content == "---")
    template_start <- if (length(template_yaml_indices) >= 2) {
      template_yaml_indices[2] + 1
    } else {
      1 # If no YAML in template, start from beginning
    }
    
    # Combine content
    combined_content <- c(
      desc_content[1:yaml_end],                         # YAML header
      desc_content[(yaml_end+1):length(desc_content)],  # Description body
      template_content[template_start:length(template_content)] # Template content
    )
    
    # Write combined content to temp file
    writeLines(combined_content, temp_file)
    
    # Use existing knit_with_df function to render
    result <- knit_with_df(
      dataframe = dataframe,
      fit = fit,
      rmd_file = temp_file,
      output_file = output_file
    )
    
    return(result)
    
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  }, finally = {
    # Clean up the temp file if it exists
    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }
  })
}
