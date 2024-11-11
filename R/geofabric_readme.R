#' Read Geofabric README
#'
#' This function returns the contents of the Geofabric README file.
#'
#' @return A character vector containing the lines of the README file.
#' @export
#'
#' @examples
#' readme_contents <- geofabric_readme()
#' cat(readme_contents, sep = "\n")
geofabric_readme <- function() {
  readme_path <- system.file("extdata", "Geofabric_V3x_FTP_README.txt", package = "ausrivRdata")
  readLines(readme_path)
}