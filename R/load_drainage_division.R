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
  
  # Match input name to standardized directory name
  matched_name <- NULL
  
  # First try exact match
  if (division_name %in% names(.division_name_map)) {
    matched_name <- .division_name_map[[division_name]]
  } else {
    # Try case-insensitive match
    idx <- which(tolower(names(.division_name_map)) == tolower(division_name))
    if (length(idx) == 1) {
      matched_name <- .division_name_map[[idx]]
    }
  }
  
  if (is.null(matched_name)) {
    stop("Invalid division name: '", division_name, "'\n",
         "Available divisions:\n", paste("-", names(.division_name_map), collapse = "\n"))
  }
  
  # Get base directory
  base_dir <- system.file("extdata/spatial", matched_name, 
                          package = "ausrivRdata")
  
  if (base_dir == "") {
    stop("Data directory not found for division: ", division_name)
  }
  
  # Initialize results list
  division_data <- list()
  
  # Load requested layers
  for (layer in layers) {
    filename <- file.path(base_dir, paste0(layer, "_", matched_name, ".rds"))
    
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
