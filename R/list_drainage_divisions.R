#' List Available Drainage Divisions
#'
#' @return Character vector of available drainage division names
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom magrittr %>%
#' @export
list_drainage_divisions <- function() {
  base_dir <- system.file("extdata/drainage_divisions", package = "ausrivRdata")
  if (base_dir == "") {
    stop("Package data directory not found")
  }
  
  # Get directory names and convert back to original format
  list.dirs(base_dir, full.names = FALSE, recursive = FALSE) %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()
}

