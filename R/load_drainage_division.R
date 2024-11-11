#' Load Drainage Division Data
#'
#' @param division_name Character; name of the drainage division
#' @param layers Character vector; which layers to load ("AWRADrainageDivision", "RiverRegion", 
#'               "AHGFNetworkStream", "AHGFWaterbody"). Default loads all.
#' @return List of sf objects for the specified drainage division
#' @importFrom sf st_make_valid
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom magrittr %>%
#' @export
load_drainage_division <- function(division_name, 
                                   layers = c("AWRADrainageDivision", "RiverRegion", 
                                              "AHGFNetworkStream", "AHGFWaterbody")) {
  
  # Create standardized suffix from division name
  division_suffix <- division_name %>%
    stringr::str_replace_all("[[:space:]-]", "_") %>%
    stringr::str_replace_all("[^[:alnum:]_]", "") %>%
    tolower()
  
  # Get base directory
  base_dir <- system.file("extdata/spatial", division_suffix, 
                          package = "ausrivRdata")
  
  if (base_dir == "") {
    stop("Drainage division '", division_name, "' not found in package data")
  }
  
  # Initialize results list
  division_data <- list()
  
  # Load requested layers
  for (layer in layers) {
    filename <- file.path(base_dir, paste0(layer, "_", division_suffix, ".rds"))
    
    if (file.exists(filename)) {
      tryCatch({
        # Load and validate geometry
        data <- readRDS(filename) %>%
          st_make_valid()
        
        division_data[[layer]] <- data
        message("Loaded ", layer, " for ", division_name)
        
      }, error = function(e) {
        warning("Error loading ", layer, ": ", e$message)
        NULL
      })
    } else {
      warning("File not found for layer '", layer, "'")
    }
  }
  
  # Check if any data was loaded
  if (length(division_data) == 0) {
    stop("No data could be loaded for division '", division_name, "'")
  }
  
  return(division_data)
}

