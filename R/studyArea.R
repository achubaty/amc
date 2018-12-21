#' Load a study area from file
#'
#' Simple wrapper around \code{readOGR} to load a kml or shapefile.
#'
#' @param path path to directory containing the file
#' @param filename the name of the file.
#' @param proj (optional) a crs projection string to reproject the study area to.
#'
#' @return A \code{SpatialPolygonsDataFrame}
#'
#' @export
#' @importFrom sp spTransform
#' @importFrom rgdal readOGR
loadStudyArea <- function(path = NULL, filename = NULL, proj = NULL) {
  if (is.null(path) | is.null(filename) | !file.exists(file.path(path, filename)))
    stop("'filename' and 'path' must be provided and must exist.")

  studyArea <- rgdal::readOGR(file.path(path, filename))

  if (!is.null(proj))
    studyArea <- sp::spTransform(studyArea, proj)

  return(studyArea)
}
