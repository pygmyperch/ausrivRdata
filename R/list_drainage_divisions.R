#' List Available Drainage Divisions
#'
#' @return Character vector of available drainage division names
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom magrittr %>%
#' @export
list_drainage_divisions <- function() {
  # Return the official names
  names(.division_name_map)
}
