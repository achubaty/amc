#' Load a study area from file
#'
#' Simple wrapper around [sf::st_read()] to load a kml or shapefile,
#' and optionally reproject it.
#'
#' @param path path to directory containing the file
#' @param filename the name of the file
#' @param proj (optional) a crs projection string to reproject the study area to.
#'
#' @return An `sf` object.
#'
#' @export
#' @importFrom sf st_read st_transform
loadStudyArea <- function(path = NULL, filename = NULL, proj = NULL) {
  if (is.null(path) | is.null(filename) | !file.exists(file.path(path, filename)))
    stop("'filename' and 'path' must be provided and must exist.")

  studyArea <- file.path(path, filename) |>
    sf::st_read()

  if (!is.null(proj)) {
    studyArea <- sf::st_transform(studyArea, proj)
  }

  return(studyArea)
}
